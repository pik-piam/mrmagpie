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

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven", fao_corr=TRUE, input_magpie=TRUE, years="y1995", round=6), dim=3)

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
