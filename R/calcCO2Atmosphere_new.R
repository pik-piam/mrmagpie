#' @title calcCO2Atmosphere_new
#' @description Disaggregate CO2 global atmospheric concentration to cellular level
#' @param subtype specify the version and scenario eg. "ISIMIP3b:ssp126"
#' @param co2Evolution Define `rising` for rising CO2 according to the climate scenario selected or
#'                      `static` for stable CO2 at the last past time step level.
#' @param cells "magpiecell" or "lpjcell"
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CO2Atmosphere_new", aggregate = FALSE, subtype, co2Evolution)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom utils tail
#' @importFrom magpiesets findset

calcCO2Atmosphere_new <- function(subtype       = "ISIMIP3b:ssp126", #nolint
                                  co2Evolution = "rising",
                                  cells         = "lpjcell") {

  x <- readSource("CO2Atmosphere_new", subtype = subtype, convert = FALSE)

  if (co2Evolution == "static") {
    past <- tail(findset("past"), 1)
    fYear <- match(past, getYears(x))
    for (i in fYear:length(getYears(x))) {
      x[, i, ] <- x[, past, ]
    }
  }

  # expand to cellular resolution
  cellMap     <- toolGetMappingCoord2Country(pretty = TRUE)
  cellMap$glo <- "GLO"
  x           <- toolAggregate(x, rel = cellMap, from = "glo", to = "coords")
  x           <- x[cellMap$coords, , ]
  getItems(x, dim = 1, raw = TRUE) <- paste(cellMap$coords, cellMap$iso, sep = ".")
  getSets(x) <- c("x", "y", "iso", "year", "data")

  # reduce to 59k cells
  if (cells == "magpiecell") {
    x <- toolCoord2Isocell(x, cells = cells)
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "ppm",
    description = "Atmosphere CO2 concentration",
    isocountries = FALSE
  ))
}
