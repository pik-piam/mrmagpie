#' @title calcCollectSoilCarbonLSU
#' @description Calculate soil carbon stocks for different LSU and climate conditions
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param sar Average range for smoothing annual variations
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("CollectSoilCarbonLSU ", lsu_levels = c(seq(0, 2, 0.2), 2.5))
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom raster rasterFromXYZ
#' @importFrom raster area
#' @importFrom raster "crs<-"
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'

calcCollectSoilCarbonLSU  <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJmL_cgrazing", climatetype = "HadGEM2_ES:rcp8p5:co2", sar = 20) {

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
    landcoords <- cbind(landcoords, rep(1,nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cell_size <- raster::area(landcoords)
    weight <- cell_size*landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight),dim=c("x","y")))

    lsu_levels <- gsub("\\.", "p", lsu_levels)
    y <- list()
    for (lsu in lsu_levels) {
      .subtype <- paste(paste(lpjml, climatetype, lsu, sep = ":"), "soilc", sep = ".")
      x <- readSource("LPJmL", subtype = .subtype, convert = "onlycorrect")
      getNames(x) <- gsub("soilc", lsu, getNames(x))
      y[[lsu]] <- x
    }
    y <- mbind(y)
    y <- toolTimeAverage(y, averaging_range = sar)
    y <- toolHoldConstant(y, (max(getYears(y, as.integer = TRUE)) + 1):2150)

    return(
      list(
        x = y,
        weight = weight,
        unit = "gC/m2",
        description = "Soil carbon stocks per lsu level",
        isocountries = FALSE
      )
    )
  }
