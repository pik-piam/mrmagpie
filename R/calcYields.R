#' @title calcYields
#' @description This function extracts yields from LPJmL to MAgPIE
#'
#' @param source Defines LPJmL version for main crop inputs and isimip replacement.
#' For isimip choose crop model/gcm/rcp/co2 combination formatted like this: "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @param climatetype Switch between different climate scenarios
#' @param cells if cellular is TRUE: "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @param weighting use of different weights (totalCrop (default), totalLUspecific, cropSpecific, crop+irrigSpecific,
#'                                            avlCropland, avlCropland+avlPasture)
#' @return magpie object in cellular resolution
#' @author Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("Yields", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass getYears add_columns dimSums time_interpolate
#' @importFrom madrat toolFillYears
#' @importFrom mrcommons toolLPJmLVersion

calcYields <- function(source=c(lpjml="ggcmi_phase3_nchecks_9ca735cb", isimip=NULL),
                           climatetype="GSWP3-W5E5:historical", cells="magpiecell", weighting = "totalCrop"){

  cfg <- toolLPJmLVersion(version = source["lpjml"], climatetype = climatetype)

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+12)
  on.exit(options(magclass_sizeLimit=sizelimit))

  if(climatetype == "GSWP3-W5E5:historical"){ stage       <- "smoothed"
                                              climatetype <- cfg$baseline_hist
  } else{                                     stage <- "harmonized2020"}

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


  if (weighting == "totalCrop") {

    crop_area_weight <- dimSums(calcOutput("Croparea", sectoral="kcr", physical=TRUE, irrigation=FALSE,
                                           cellular=TRUE, cells=cells, aggregate = FALSE, years="y1995", round=6), dim = 3)

  } else if (weighting %in% c("totalLUspecific", "cropSpecific", "crop+irrigSpecific")) {

    crop <- calcOutput("Croparea", sectoral="kcr", physical=TRUE, irrigation=TRUE,
                       cellular=TRUE, cells=cells, aggregate = FALSE, years="y1995", round=6)

    past <- calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven", fao_corr=TRUE,
                       input_magpie=TRUE, cells = cells, years="y1995", round=6)[, ,"past" ]

    if (weighting == "crop+irrigSpecific"){

      crop_area_weight <- new.magpie(cells_and_regions = getCells(yields), years = NULL,
                                     names = getNames(yields), fill = NA)
      crop_area_weight[, , findset("kcr")] <- crop + 10^-10
      crop_area_weight[, , "pasture"]      <- mbind(setNames(past + 10^-10, "irrigated"),
                                                    setNames(past + 10^-10, "rainfed"))

    } else if (weighting == "cropSpecific") {

      crop_area_weight <- new.magpie(cells_and_regions = getCells(yields), years = NULL,
                                     names = getNames(yields, dim = 1), fill = NA)

      crop_area_weight[, , findset("kcr")] <- dimSums(crop, dim = 3.1) + 10^-10
      crop_area_weight[, , "pasture"]      <- past + 10^-10

    } else {

      crop_area_weight <- new.magpie(cells_and_regions = getCells(yields), years = NULL,
                                     names = getNames(yields, dim = 1), fill = (dimSums(crop, dim = 3) + 10^-10))

      crop_area_weight[, , "pasture"]      <- past  + 10^-10

    }

  } else if (weighting == "avlCropland") {

    crop_area_weight <- setNames(calcOutput("AvlCropland", marginal_land = "all_marginal", cells = cells,
                                   country_level = FALSE, aggregate = FALSE), NULL)

  } else if (weighting == "avlCropland+avlPasture") {

    avlCrop <- setNames(calcOutput("AvlCropland", marginal_land = "all_marginal", cells = cells,
                        country_level = FALSE, aggregate = FALSE), "avlCrop")

    LU1995  <- setYears(calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, nclasses = "seven",
                                   fao_corr = TRUE, input_magpie = TRUE, cells = cells, years = "y1995", round = 6), NULL)

    crop_area_weight <- new.magpie(cells_and_regions = getCells(yields), years = NULL,
                                   names = getNames(yields, dim = 1), fill = NA)

    crop_area_weight[, ,  findset("kcr")] <- new.magpie(cells_and_regions = getCells(yields), years = NULL,
                                                        names = getNames(yields, dim = 1), fill = avlCrop)
    crop_area_weight[, , "pasture"]       <- pmax(avlCrop,
                                             dimSums(LU1995[, , c("primforest", "secdforest", "forestry", "past")], dim = 3))

  } else {

    stop("Weighting setting is not available.")
  }

  if(any(is.na(crop_area_weight))) stop("NAs in weights.")

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
