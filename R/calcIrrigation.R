#' @title calcIrrigation
#' @description This function extracts irrigation water (airrig: water applied additionally to rainfall) from LPJmL to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param rainfedweight For clustering airrig is weighted with cropland_irrigated + rainfedweight * cropland_rainfed (default: 0.01)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Abhijeet Mishra
#'
#' @examples
#' \dontrun{ calcOutput("Irrigation", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom mrcommons toolHarmonize2Baseline

calcIrrigation <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"),
                           climatetype="GSWP3-W5E5:historical", rainfedweight=0.01){

  if(climatetype=="GSWP3-W5E5:historical"){ stage <- "smoothed"
  } else{                                   stage <- "harmonized2020"}

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit=1e+10)
  on.exit(options(magclass_sizeLimit=sizelimit))

  # Read in airrig (irrigation water applied additionally to rainfall where irrigation takes place):
  lpj_airrig   <- toolCoord2Isocell(collapseNames(calcOutput("LPJmL_new", version=lpjml["crop"], climatetype=climatetype,
                                                             subtype="irrig", aggregate=FALSE, stage=stage)))

  # Load LPJmL to MAgPIE mapping to aggregate to MAgPIE crops
  LPJ2MAG      <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mappingfolder")
  # Aggregate to MAgPIE crops
  mag_airrig   <- toolAggregate(lpj_airrig, LPJ2MAG, from = "LPJmL", to = "MAgPIE", dim=3.1, partrel=TRUE)
  # Remove pasture (pasture is not irrigated in MAgPIE)
  mag_airrig   <- mag_airrig[,,"pasture",invert=T]
  # Remove negative airrig
  mag_airrig[mag_airrig<0] <- 0

  # Check for NAs
  if(any(is.na(mag_airrig))){
    stop("produced NA airrig")
  }

  # Clustering weight:
  cropland_total   <- dimSums(calcOutput("Croparea", sectoral="kcr", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate = FALSE, years="y1995", round=6), dim=3.2)
  crop_area_weight <- collapseNames(cropland_total[,,"irrigated"]) + rainfedweight * collapseNames(cropland_total[,,"rainfed"])

  return(list(
    x=mag_airrig,
    weight=crop_area_weight,
    unit="m^3 per ha per yr",
    description="Irrigation water (water applied in addition to rainfall) for different crop types following LPJmL irrigation system assumptions",
    isocountries=FALSE))
}
