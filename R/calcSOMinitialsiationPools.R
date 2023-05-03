#' @title calcSOMinitialsiationPools
#' @description calculates Soil Organic Matter Pool, accounting for
#'              the management history as initialisation to magpie
#'
#' @param cells "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return List of magpie object with results on country or cellular level,
#'         weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("SOMinitialsiationPools")
#' }
#'
calcSOMinitialsiationPools <- function(cells = "magpiecell") {

  past <- findset("past")
  som  <- calcOutput("SOM", cells = cells, aggregate = FALSE)
  som  <- collapseNames(som[, past, c("soilc")])

  return(list(
    x = som,
    weight = NULL,
    unit = "Mt C",
    description = "Soil carbon in cropland and non-cropland soils.",
    isocountries = FALSE,
    min = 0,
    max = 1000
  ))
}
