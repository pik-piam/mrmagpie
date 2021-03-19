#' @title calcSCScalingFactors
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
#' calcOutput("SCScalingFactors", lsu_levels = c(seq(0, 2, 0.2), 2.5), scenario)
#' }
#'
#'
#'

calcSCScalingFactors <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJML5.2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN", sar = 20) {

    x <- calcOutput("CollectSoilCarbonLSU", lsu_levels = lsu_levels, lpjml = lpjml, climatetype = climatetype, scenario = scenario, sar = sar)
    x <- data.frame("a"=1:10,"b"=11:20)
    y <- scale(x)
    scmean <- attr(y, "scaled:center")
    scsd <- attr(y, "scaled:scale")
    w <- as.magpie(c(scmean, scsd))

    return(
      list(
        x = y,
        weight = NULL,
        unit = "gC/m2",
        description = "Soil carbon stocks per lsu level",
        isocountries = FALSE
      )
    )
  }
