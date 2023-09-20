#' @title calcDegradationYieldReduction
#' @description Function creates dummy file for including yield reduction coefficients to represent land degradation
#'
#' @param cells number of halfdegree grid cells to be returned.
#'              Options: "magpiecell" (59199), "lpjcell" (67420)
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("DegradationYieldReduction", aggregate = FALSE)
#' }
#' @importFrom magpiesets findset
#'

calcDegradationYieldReduction <- function(cells = "lpjcell") {

  # create a dummy data set, which is later used to define yield impacts of land degradation
  coordMapping <- mrcommons::toolGetMappingCoord2Country()
  cellnames    <- paste(coordMapping$coords, coordMapping$iso, sep = ".")
  x <- new.magpie(cells_and_regions = cellnames,
                  years = seq(1995, 2150, 5),
                  names = c("soil_loss", "poll_loss"),
                  sets = c("x.y.iso", "year", "data1"),
                  fill = 0)

  if (cells == "magpiecell") {
    x <- mrcommons::toolCoord2Isocell(x, cells = cells)
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "dummy (none)",
    description = "Dummy file for yield reduction coefficients to represent land degradation",
    isocountries = FALSE))
}
