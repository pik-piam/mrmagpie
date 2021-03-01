#' @title calcYields
#' @description This function extracts yields from LPJmL to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
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

calcYields <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"),
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
    tmp     <- calcOutput("LPJmL_new", version=lpjml["crop"], climatetype=climatetype,
                          subtype="harvest", subdata=subdata, stage=stage, aggregate=FALSE)
    yields  <- mbind(yields, tmp)
  }

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
  Calib            <- new.magpie("GLO", getYears(yields), c(getNames(FAOYields), "pasture"), fill=1)
  Calib[,matchingFAOyears,"oilpalm"]   <- FAOYields[,,"oilpalm"]/FAOYields[,,"groundnut"]      # LPJmL proxy for oil palm is groundnut
  Calib[,matchingFAOyears,"cottn_pro"] <- FAOYields[,,"cottn_pro"]/FAOYields[,,"groundnut"]    # LPJmL proxy for cotton is groundnut
  Calib[,matchingFAOyears,"foddr"]     <- FAOYields[,,"foddr"]/FAOYields[,,"maiz"]             # LPJmL proxy for fodder is maize
  Calib[,matchingFAOyears,"others"]    <- FAOYields[,,"others"]/FAOYields[,,"maiz"]            # LPJmL proxy for others is maize
  Calib[,matchingFAOyears,"potato"]    <- FAOYields[,,"potato"]/FAOYields[,,"sugr_beet"]       # LPJmL proxy for potato is sugar beet

  # interpolate between FAO years
  Calib <- toolFillYears(Calib, getYears(yields))

  # recalibrate yields for proxys
  yields <- Calib[,,getNames(yields, dim=1)] * yields

  if(cells=="magpiecell") yields <- toolCoord2Isocell(yields)

  #check again, what makes sense irrigation=FALSE/TRUE?
  crop_area_weight <- dimSums(calcOutput("Croparea", sectoral="kcr", physical=TRUE, irrigation=FALSE,
                                         cellular=TRUE, cells=cells, aggregate = FALSE, years="y1995", round=6), dim=3)


  return(list(
    x=yields,
    weight=crop_area_weight,
    unit="t per ha",
    description="Yields in tons per hectar for different crop types.",
    isocountries=FALSE))
}
