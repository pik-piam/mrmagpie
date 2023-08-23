#' @title calcDegradationYieldReduction
#' @description Function creates dummy file for including yield reduction coefficients to represent land degradation
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("DegradationYieldReduction", aggregate = FALSE)
#' }
#' @importFrom magpiesets findset
#'

calcDegradationYieldReduction <- function() {
# create a dummy data set, which is later used to define yield impacts of land degradation
cells <- toolGetMapping("CountryToCellMapping.csv", type = "cell")[, "celliso"]
x <- new.magpie(cells_and_regions = cells,
                years = seq(1995, 2150, 5),
                names = c("soil_loss", "poll_loss"),
                sets = c("region.cell", "year", "data1"),
                fill = 0)

return(list(
  x = x,
  weight = NULL,
  unit = "dummy (none)",
  description = "Dummy file for yield reduction coefficients to represent land degradation",
  isocountries = FALSE))
}
