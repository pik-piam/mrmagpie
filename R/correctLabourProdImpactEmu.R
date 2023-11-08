#' @title correctLabourProdImpactEmu
#' @description correct labour productivity impacts from climate change emulated by the LAMACLIMA project
#' @description based on method of Orlov et al. 2019. Economics of Disasters and Climate Change, 3(3), 191-211.
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @param x magpie object provided by the read function
#' @author Michael Windisch
#' @seealso
#'   \code{\link{readLabourProdImpactEmu}}
#' @examples
#'
#' \dontrun{
#'   readSource("LabourProdImpactEmu", convert="onlycorrect")
#' }
#'

correctLabourProdImpactEmu <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)

  return(x)
}
