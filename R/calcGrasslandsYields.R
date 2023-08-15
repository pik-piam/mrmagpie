#' @title calcGrasslandsYields
#' @description Calculates rangelands maximum output and managed pastures yields
#' @param past_mngmt pasture areas management option
#' @param lsu_levels Livestock unit levels in the source folder
#' @param subtype Switch between different climate scenarios
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Global Circulation Model to be used
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("GrasslandsYields", lsu_levels, past_mngmt = "me2", subtype)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'

calcGrasslandsYields <- function(lpjml = lpjml[["grass"]], climatetype = climatetype, subtype = "/co2/Nreturn0p5", # nolint
                                 lsu_levels = c(seq(0, 2, 0.2), 2.5), past_mngmt = "me2") { # nolint

    gCm2yTotDMy <- (10000 * 2.21 / 1e6)
    x <- calcOutput("RangelandsMaxNew", lsuLevels = lsu_levels, lpjml = lpjml, climatetype = climatetype,
                    scenario = paste0(subtype, "/limN"), report = "harvest", aggregate = FALSE)
    if (past_mngmt == "mdef") {
      n <- "/unlimN"
    } else if (past_mngmt == "me2") {
      n <- "/limN"
    } else {
      stop("past_mngmt not available yet")
    }
    y <- calcOutput("Pastr_new", past_mngmt = past_mngmt, lpjml = lpjml, climatetype = climatetype,
                    scenario = paste0(subtype, n), aggregate = FALSE)
    invalid <- (y - x) < 0
    # substituting pastr yields that are smaller than rangelands by the value of rangeland yields
    y[invalid]  <- x[invalid]
    pasture <- mbind(x, y)
    invalid <- (pasture[, , "pastr"] - pasture[, , "range"]) < 0
    pasture <- toolHoldConstantBeyondEnd(pasture)
    pasture <- pasture * gCm2yTotDMy

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell", where = "mappingfolder"))
    landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cellSize <- raster::area(landcoords)
    weight <- cellSize * landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight), dim = c("x", "y")))

    return(
      list(
        x = pasture,
        weight = weight,
        unit = "t/DM/y",
        description = paste("Maximum grasslands yields obtained with rangelands and managed pastures yields for",
                            past_mngmt),
        isocountries = FALSE
      )
    )
  }
