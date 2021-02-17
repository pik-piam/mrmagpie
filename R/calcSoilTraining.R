#' @title calcSoilCarbon
#' @description Calculate soil carbon stocks for different LSU and climate conditions
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
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
#'
#'

calcSoilTraining <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJmL_cgrazing", climatetype = "HadGEM2_ES:rcp8p5:co2", sar = 20) {
    co2 <- c(co2="rising", noco2="static")
    GCM <- strsplit(climatetype, split = ":")
    GCM <- paste("soilc", GCM[[1]][1],GCM[[1]][2],sep = "_")

    x <- calcOutput("SoilCarbon", aggregate = F, lsu_levels = lsu_levels, lpjml = lpjml, climatetype = climatetype, sar = sar )[,1995:2100,]
    y <- calcOutput("Environment", climatetype=climatetype, sar = sar, aggregate = F)[,1995:2100,]
    w <- add_columns(y, addnm = GCM)
    w <- add_dimension(w, dim = 3.1, add = "lsu", nm = getItems(x, dim = 3))
    w[,,GCM] <- x

    return(
      list(
        x = w,
        weight = NULL,
        unit = NULL,
        description = "Training data for soil carbon machine learninig model",
        isocountries = FALSE
      )
    )
  }
