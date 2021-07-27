#' @title calcGrasslandsYields
#' @description Calculates rangelands maximum output and managed pastures yields
#' @param past_mngmt pasture areas management option
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param scenario specify ssp scenario
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("GrasslandsYields", lsu_levels, past_mngmt = "2me", lpjml, climatetype)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'

calcGrasslandsYields <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), past_mngmt = "me2", lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN") {

    gCm2yTotDMy <- (10000 * 2.21 / 1e6)
    x <- calcOutput("RangelandsMax_new", lsu_levels = lsu_levels, lpjml = lpjml, climatetype = climatetype, scenario = scenario, report = "harvest", aggregate = F)
    y <- calcOutput("Pastr_new", past_mngmt = past_mngmt, lpjml = lpjml, climatetype = climatetype, scenario = scenario,  aggregate = F)
    pasture <- mbind(x, y)
    pasture <- toolHoldConstantBeyondEnd(pasture)
    pasture <- pasture * gCm2yTotDMy

    # # Calculating weights
    # landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
    # landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
    # landcoords <- raster::rasterFromXYZ(landcoords)
    # crs(landcoords) <- "+proj=longlat"
    # cell_size <- raster::area(landcoords)
    # weight <- cell_size * landcoords
    # weight <- as.magpie(weight)
    # weight <- toolOrderCells(collapseDim(addLocation(weight), dim = c("x", "y")))

    weight <- calcOutput("LUH2v2", aggregate = "cluster", landuse_types = "LUH2v2", cellular = TRUE)[,,c("range", "pastr")]

    return(
      list(
        x = pasture,
        weight = weight,
        unit = "t/DM/y",
        description = paste("Maximum grasslands yields obtained with rangelands and managed pastures yields for", past_mngmt),
        isocountries = FALSE
      )
    )
  }
