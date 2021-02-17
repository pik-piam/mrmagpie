#' @title calcCO2Atmosphere
#' @description Disaggregate CO2 global atmospheric concentration to cellular level
#' @param rcp specify the RCP (rcp85, rcp60, rcp45 or rcp26)
#' @param co2_evolution Define `rising` for rising CO2 according to the climate scenario selected or
#' `static` for stable CO2 at the last past time step level.
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CO2Atmosphere", aggregate = FALSE, rcp = "rcp8p5", co2_evolution = "rising")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom utils tail
#' @importFrom magpiesets findset
#'

calcCO2Atmosphere <- function(rcp = "rcp8p5", co2_evolution = "rising" ) {

  x <- readSource("CO2Atmosphere", subtype = rcp, convert = "onlycorrect")

  if(co2_evolution == "rising"){

    cells <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
    cells$glo <- "GLO"
    x <- toolAggregate(x, rel = cells, from = "glo", to = "celliso")

  } else if(co2_evolution == "static") {
    past <- tail(findset("past"),1)
    f_year <- match(past,getYears(x))
    for (i in f_year:length(getYears(x))) {
      x[, i, ] <- x[,past,]
    }
    cells <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
    cells$glo <- "GLO"
    x <- toolAggregate(x, rel = cells, from = "glo", to = "celliso")
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "ppm",
    description = "Atmosphere CO2 concentration",
    isocountries = FALSE
  ))
}
