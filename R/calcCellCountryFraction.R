#' @title calcCellCountryFraction
#' @description cell fraction belonging to a country based on LanduseInitialisation
#'
#' @return Clustered MAgPIE object on requested resolution
#' @author Florian Humpenoeder
#'
#' @examples
#' \dontrun{
#' calcOutput("calcCellCountryFraction", aggregate = FALSE)
#' }
#'
#' @export

calcCellCountryFraction <- function() {

  weight <- calcOutput("LandArea", cells = "lpjcell", aggregate = FALSE)

  x <- new.magpie(cells_and_regions = getItems(weight, dim = 1),
                  years = NULL,
                  names = getItems(weight, dim = 1.3),
                  fill = 0)
  for (r in getItems(weight, dim = 1.3)) {
    x[r, , r] <- 1
  }

  getSets(x) <- c("x", "y", "iso", "year", "data")

  return(list(x = x,
              weight = weight,
              unit = "share",
              description = "cell fraction belonging to a country",
              isocountries = FALSE))
}
