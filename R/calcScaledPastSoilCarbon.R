#' @title calcScaledPastSoilCarbon
#' @description calculates the mean and sd of the scaled pasture soil carbon dataset
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param sar Average range for smoothing annual variations
#' @param scenario scenario specifications (eg. ssp126_co2_limN)
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("ScaledPastSoilCarbon", lsu_levels = c(seq(0, 2, 0.2), 2.5), scenario)
#' }
#'
#'
#' @import madrat
#' @import magclass
#' @importFrom raster rasterFromXYZ
#' @importFrom raster area
#' @importFrom raster "crs<-"
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'

calcScaledPastSoilCarbon <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJML5.2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN", sar = 20) {

    x <- calcOutput("CollectSoilCarbonLSU", lsu_levels = lsu_levels, lpjml = lpjml, climatetype = climatetype, scenario = scenario, sar = sar)
    xmax <- max(x[["x"]])
    xmin <-  min(x[["x"]])
    y <- (x[["x"]] - xmin)/xmax - xmin

    return(
      list(
        x = y,
        weight = x$weight,
        unit = NULL,
        description = "Soil carbon stocks per lsu level",
        isocountries = FALSE
      )
    )
  }
