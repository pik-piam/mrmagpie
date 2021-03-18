#' @title calcPastYields_new
#' @description Calculates continuous grazing maximum output and mowing yields
#' @param mowing_events number of mowing events per year expressed as `2me`
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param scenario specify ssp scenario
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{calcOutput("PastYields_new", lsu_levels, mowing_events = "2me", lpjml, climatetype)}
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'


calcPastYields_new <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), mowing_events = "me2", lpjml= "lpjml5.2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN") {


    x <- calcOutput("ContGrazMax_new", lsu_levels = lsu_levels, lpjml = lpjml, climatetype = climatetype, scenario = scenario, report = "harvest", aggregate = F)
    y <- calcOutput("Mowing_new", mowing_events = mowing_events, lpjml = lpjml, climatetype = climatetype, scenario = scenario,  aggregate = F)
    pasture <- mbind(x,y)

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
    landcoords <- cbind(landcoords, rep(1,nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cell_size <- raster::area(landcoords)
    weight <- cell_size*landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight),dim=c("x","y")))

    return(
      list(
        x = pasture,
        weight = weight,
        unit = "t/DM/y",
        description = paste("Maximum pasture yields obtained with continuous grazing and mowing yields for",mowing_events),
        isocountries = FALSE
      )
    )
  }


