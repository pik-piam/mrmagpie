#' @title calcCollectSoilCarbonLSU
#' @description Calculate soil carbon stocks for different LSU and climate conditions
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatemodel Switch between different climate scenarios
#' @param sar Average range for smoothing annual variations
#' @param scenario scenario specifications (eg. ssp126_co2_limN)
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("CollectSoilCarbonLSU", lsu_levels = c(seq(0, 2, 0.2), 2.5), scenario)
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
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJML5.2_pasture", climatemodel = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN", sar = 20) {

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
    landcoords <- cbind(landcoords, rep(1,nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cell_size <- raster::area(landcoords)
    weight <- cell_size*landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight),dim = c("x","y")))

    lsu_levels <- gsub("\\.", "p", lsu_levels)
    y <- list()
    for (lsu in lsu_levels) {
      .subtype <- paste(lpjml, climatemodel,paste0(scenario,"/", lsu),sep = ":")
      hist <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "soilc_past_hist", sep = ":"), convert = F))
      scen <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "soilc_past_scen", sep = ":"), convert = F))
      x <- mbind(hist,scen)
      getNames(x) <- lsu
      y[[lsu]] <- x
    }
    y <- mbind(y)
    #y <- toolTimeAverage(y, averaging_range = sar)
    y <- toolHoldConstant(y, (max(getYears(y, as.integer = TRUE)) + 1):2150)

    unit_transform <- 0.01               # Transformation factor gC/m^2 --> t/ha
    y <- y * unit_transform

    return(
      list(
        x = y,
        weight = weight,
        unit = "t/ha",
        description = "Soil carbon stocks per lsu level",
        isocountries = FALSE
      )
    )
  }
