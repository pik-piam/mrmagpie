#' @title calcIrrigation
#' @description This function extracts irrigation water (airrig: water applied additionally to rainfall)
#'              from LPJmL to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param rainfedweight For clustering airrig is weighted with
#'                      cropland_irrigated + rainfedweight * cropland_rainfed (default: 0.01)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' calcOutput("Irrigation", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset

calcIrrigation <- function(lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"),
                           climatetype = "GSWP3-W5E5:historical", rainfedweight = 0.01) {

  cfg <- toolLPJmLVersion(version = lpjml["crop"], climatetype = climatetype)

  sizelimit <- getOption("magclass_sizeLimit") # nolint
  options(magclass_sizeLimit = 1e+12) # nolint
  on.exit(options(magclass_sizeLimit = sizelimit)) # nolint

  if (grepl("GSWP3-W5E5", climatetype)) {
    stage       <- "smoothed"
    climatetype <- cfg$baseline_hist
  } else {
    stage       <- "harmonized2020"
  }

  # Read in airrig (irrigation water applied additionally to rainfall where irrigation takes place):
  airrigLPJ   <- toolCoord2Isocell(collapseNames(calcOutput("LPJmL_new", version = lpjml["crop"],
                                                             climatetype = climatetype, subtype = "irrig",
                                                             aggregate = FALSE, stage = stage)))

  # Load LPJmL to MAgPIE mapping to aggregate to MAgPIE crops
  mapping     <- toolGetMapping("MAgPIE_LPJmL.csv",
                                 type = "sectoral", where = "mappingfolder")
  # Aggregate to MAgPIE crops
  airrigMAG   <- toolAggregate(airrigLPJ, mapping,
                               from = "LPJmL", to = "MAgPIE", dim = 3.1, partrel = TRUE)
  # Remove pasture (pasture is not irrigated in MAgPIE)
  airrigMAG   <- airrigMAG[, , "pasture", invert = TRUE]
  # Remove negative airrig
  airrigMAG[airrigMAG < 0] <- 0

  # Check for NAs
  if (any(is.na(airrigMAG))) {
    stop("produced NA airrig")
  }

  # Clustering weight:
  totalCropland  <- dimSums(calcOutput("Croparea", sectoral = "kcr", physical = TRUE, cellular = TRUE,
                                         irrigation = TRUE, aggregate = FALSE, years = "y1995", round = 6), dim = 3.2)
  weightCropArea <- collapseNames(totalCropland[, , "irrigated"]) +
                                  rainfedweight * collapseNames(totalCropland[, , "rainfed"])

  return(list(x = airrigMAG,
              weight = weightCropArea,
              unit = "m^3 per ha per yr",
              description = "Irrigation water (water applied in addition to rainfall) for
                             different crop types following LPJmL irrigation system assumptions",
              isocountries = FALSE))
}
