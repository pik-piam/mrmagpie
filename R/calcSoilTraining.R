#' @title calcSoilTraining
#' @description Calculate soil carbon stocks for different LSU and climate conditions
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param sar Smooth average range
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilTraining")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @importFrom raster rasterFromXYZ
#' @importFrom raster area
#' @importFrom raster "crs<-"
#'
#'

calcSoilTraining <-
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

    # time <- magpiesets::findset("t_all")
    co2 <- c(co2="rising", noco2="static")
    GCM <- strsplit(climatetype, split = ":")
    GCM <- paste("soilc", GCM[[1]][1],GCM[[1]][2],sep = "_")

    ###-> something is wrong here (SoilCarbon is not a valid calcOutput type like this)
    x <- calcOutput("SoilCarbon", aggregate = "cluster", lsu_levels = lsu_levels, lpjml = lpjml, climatetype = climatetype, sar = sar )
    y <- calcOutput("Environment", aggregate = "cluster", climatetype=climatetype, sar = sar )
    w <- add_columns(y, addnm = GCM)
    w <- add_dimension(w, dim = 3.1, add = "lsu", nm = getItems(x, dim = 3))
    w[,,GCM] <- x

    return(
      list(
        x = w,
        weight = weight,
        unit = NULL,
        description = "Training data for soil carbon machine learninig model",
        isocountries = FALSE
      )
    )
  }
