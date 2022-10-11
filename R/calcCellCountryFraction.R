#' @title calcCellCountryFraction
#' @description cell fraction belonging to a country based on LanduseInitialisation
#'
#' @return Clustered MAgPIE object on requested resolution
#' @author Florian Humpenoeder
#'
#' @examples
#' \dontrun{ calcOutput("calcCellCountryFraction", aggregate = FALSE) }
#'
#' @export

calcCellCountryFraction <-function(){

  weight <- calcOutput("CellArea", aggregate=FALSE)

  x <- new.magpie(getCells(weight),NULL,getRegions(weight),fill = 0)
  for(r in getRegions(x)) {
    x[r,,r] <- 1
  }

  return(list(
    x=x,
    weight=weight,
    unit="share",
    description="cell fraction belonging to a country",
    isocountries=FALSE))
}
