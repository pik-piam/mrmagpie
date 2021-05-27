#' @title calcYields
#' @description This function extracts yields from LPJmL to MAgPIE
#'
#' @param source Defines LPJmL version for main crop inputs and isimip replacement.
#' For isimip choose crop model/gcm/rcp/co2 combination formatted like this: "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @param climatetype Switch between different climate scenarios
#' @param cells if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return magpie object in cellular resolution
#' @author Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("Yields", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass getYears add_columns dimSums time_interpolate
#' @importFrom madrat toolFillYears

calcYields <- function(source=c(lpjml="ggcmi_phase3_nchecks_72c185fa", isimip=NULL),
                           climatetype="GSWP3-W5E5:historical", cells="magpiecell"){

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+12)
  on.exit(options(magclass_sizeLimit=sizelimit))

  if(climatetype=="GSWP3-W5E5:historical"){ stage <- "smoothed"
  } else{                                   stage <- "harmonized2020"}

  LPJ2MAG      <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
  lpjml_crops  <- unique(LPJ2MAG$LPJmL)
  irrig_types  <- c("irrigated","rainfed")
  yields       <- NULL

  for(crop in lpjml_crops){

    subdata <- as.vector(outer(crop, irrig_types, paste, sep="."))
    tmp     <- calcOutput("LPJmL_new", version=source[["lpjml"]], climatetype=climatetype,
                          subtype="harvest", subdata=subdata, stage=stage, aggregate=FALSE)
    yields  <- mbind(yields, tmp)
  }

  # include garbage collector to solve recurring memory problems
  gc()

  # LPJmL to MAgPIE crops
  yields    <- toolAggregate(yields, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel=TRUE)

  # Check for NAs
  if(any(is.na(yields))){
    stop("produced NA yields")
  }

  FAOproduction     <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,,"dm"])
  MAGarea           <- calcOutput("Croparea", sectoral="kcr", physical=TRUE, aggregate=FALSE)


  MAGcroptypes  <- findset("kcr")
  missing       <- c("betr","begr")
  MAGcroptypes  <- setdiff(MAGcroptypes, missing)
  FAOproduction <- add_columns(FAOproduction[,,MAGcroptypes],addnm = missing,dim = 3.1)
  FAOproduction[,,missing] <- 0

  FAOYields         <- dimSums(FAOproduction,dim=1)/dimSums(MAGarea, dim=1)

  matchingFAOyears <- intersect(getYears(yields),getYears(FAOYields))
  FAOYields        <- FAOYields[,matchingFAOyears,]
  Calib            <- new.magpie("GLO", matchingFAOyears, c(getNames(FAOYields), "pasture"), fill=1, sets=c("iso","year","data"))
  Calib[,matchingFAOyears,"oilpalm"]   <- FAOYields[,,"oilpalm"]/FAOYields[,,"groundnut"]      # LPJmL proxy for oil palm is groundnut
  Calib[,matchingFAOyears,"cottn_pro"] <- FAOYields[,,"cottn_pro"]/FAOYields[,,"groundnut"]    # LPJmL proxy for cotton is groundnut
  Calib[,matchingFAOyears,"foddr"]     <- FAOYields[,,"foddr"]/FAOYields[,,"maiz"]             # LPJmL proxy for fodder is maize
  Calib[,matchingFAOyears,"others"]    <- FAOYields[,,"others"]/FAOYields[,,"maiz"]            # LPJmL proxy for others is maize
  Calib[,matchingFAOyears,"potato"]    <- FAOYields[,,"potato"]/FAOYields[,,"sugr_beet"]       # LPJmL proxy for potato is sugar beet

  # interpolate between FAO years
  Calib <- toolFillYears(Calib, getYears(yields))

  # recalibrate yields for proxys
  yields <- yields * Calib[,,getNames(yields, dim=1)]

  if(cells=="magpiecell") {
    yields <- toolCoord2Isocell(yields)
  }

   if (!is.na(source["isimip"])){
    to_rep <- calcOutput("ISIMIP3bYields", subtype=source[["isimip"]], cells=cells, aggregate=F)
    common_vars <- intersect(getNames(yields),getNames(to_rep))
    common_years <- intersect(getYears(yields), getYears(to_rep))
    # convert to array for memory
    yields <- as.array(yields); to_rep <- as.array(to_rep)
    #yields[,common_years,common_vars] <- ifelse(to_rep[,common_years,common_vars] >0, to_rep[,common_years,common_vars], yields[,common_years, common_vars])
    yields[,common_years, common_vars] <- to_rep[,common_years,common_vars]
    yields <- as.magpie(yields); to_rep <- as.magpie(to_rep)

  }
  #check again, what makes sense irrigation=FALSE/TRUE?
  crop_area_weight <- dimSums(calcOutput("Croparea", sectoral="kcr", physical=TRUE, irrigation=FALSE,
                                         cellular=TRUE, cells=cells, aggregate = FALSE, years="y1995", round=6), dim=3)
  if (cells=="lpjcell") {
    crop_area_weight <- addLocation(crop_area_weight)
  }


  return(list(
    x=yields,
    weight=crop_area_weight,
    unit="t per ha",
    description="Yields in tons per hectar for different crop types.",
    isocountries=FALSE))
}
