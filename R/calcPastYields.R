#' @title calcPastYields
#' @description Calculates rangelands maximum output and managed pastures yields
#' @param past_mngmt pasture areas management option
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("PastYields", lsu_levels, past_mngmt = "2me", lpjml, climatetype)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'


calcPastYields <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), past_mngmt = "2me", lpjml = c(range = "LPJmL_range", pastr = "LPJmL_pastr"), climatetype = "HadGEM2_ES:rcp8p5:co2") {

    years <- 1995:2100
    x <- calcOutput("ContGrazMax", lsu_levels = lsu_levels, lpjml = lpjml["range"], climatetype = climatetype, report = "harvest", aggregate = F)
    y <- calcOutput("Pastr", past_mngmt = past_mngmt, lpjml = lpjml["pastr"], climatetype = climatetype, aggregate = F)
    x <- toolFillYears(x, years = years)
    y <- toolFillYears(y, years =  years)
    pasture <- mbind(x, y)

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
    landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cell_size <- raster::area(landcoords)
    weight <- cell_size * landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight), dim = c("x", "y")))

    return(
      list(
        x = pasture,
        weight = weight,
        unit = "t/DM/y",
        description = paste("Maximum pasture yields obtained with rangelands and managed pastures yields for", past_mngmt),
        isocountries = FALSE
      )
    )
  }
