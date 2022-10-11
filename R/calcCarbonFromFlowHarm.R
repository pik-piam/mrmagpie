#' @title calcCarbonFromFlowHarm
#' @description This function calculates carbon sink per year and uses them for smoothing and harmonization
#'              and calculates carbon from it for MAgPIE again (to avoid jumps in carbon sink estimates)
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("calcCarbonFromFlowHarm", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass add_dimension

calcCarbonFromFlowHarm <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_44ac93de", crop="ggcmi_phase3_nchecks_9ca735cb"),
                                   climatetype = "GSWP3-W5E5:historical") {

  # Create settings for LPJmL from version and climatetype argument
  cfg_lpjml <- toolLPJmLVersion(version=lpjml["natveg"], climatetype = climatetype)

  .getLPJmLCPools <- function(pool, cfg) {
    out <- calcOutput("LPJmL_new", version = cfg$lpjml,
                      climatetype = cfg$climatetype,
                      subtype = pool, stage = "smoothed",
                      aggregate = FALSE)
    out <- toolCoord2Isocell(out)
    out <- setNames(out, pool)
    return(out)
  }

  # Load raw data on carbon stocks
  cfg         <- list(lpjml=lpjml["natveg"], climatetype=cfg_lpjml$baseline_hist)
  carbonStock <- mbind(.getLPJmLCPools("vegc", cfg),
                  .getLPJmLCPools("soilc", cfg),
                  .getLPJmLCPools("litc", cfg))

  # Load smoothed and harmonized Carbon Sink
  carbonSink  <- calcOutput("CarbonSink", version = lpjml["natveg"], climatetype = climatetype,
                           stage = "harmonized2020", aggregate = FALSE)

  # Calculate Carbon densities from carbon flows
  years       <- getYears(carbonSink, as.integer = TRUE)

  for (y in years[years > 1995]) {
    carbonStock[,y,] <- setYears(carbonStock[, y - 1, ], y) + carbonSink[, y, ]
  }

  topsoilc       <- calcOutput("TopsoilCarbon", lpjml=lpjml, climatetype=climatetype, aggregate=FALSE)
  cshare         <- calcOutput("SOCLossShare", aggregate=FALSE, years="y1995")

  ####################################################
  #Create the output object
  ####################################################

  out  <- new.magpie(cells_and_regions = getCells(carbonStock),
                                 years = getYears(carbonStock),
                                 names = getNames(carbonStock))

  out  <- add_dimension(out, dim = 3.1, add = "landtype",
                        nm = c("crop","past","forestry","primforest","secdforest", "urban", "other"))

  ####################################################
  #Calculate the appropriate values for all land types and carbon types.
  ####################################################

  #Factor 0.012 is based on the script subversion/svn/tools/carbon_cropland, executed at 30.07.2013
  out[, , "crop.vegc"]       <- 0.012 * carbonStock[,,"vegc"]
  out[, , "crop.litc"]       <- 0 # does not make sense
  out[, , "crop.soilc"]      <- cshare * topsoilc + (carbonStock[,,"soilc"] - topsoilc)
  out[, , "past"]            <- carbonStock
  out[, , "forestry"]        <- carbonStock
  out[, , "primforest"]      <- carbonStock
  out[, , "secdforest"]      <- carbonStock
  out[, , "urban"]           <- 0
  out[, , "other"]           <- carbonStock #or grass?

  # Check for NAs
  if(any(is.na(out))){
    stop("produced NA Carbon")
  }

  weight <- calcOutput("CellArea", aggregate=FALSE)

  return(list(
    x=out,
    weight=weight,
    unit="t per ha",
    description="Carbon in tons per hectar for natural vegetation based on carbon sink harmonization.",
    isocountries=FALSE))
}
