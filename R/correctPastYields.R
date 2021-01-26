#' @title correctPastYields
#' @description Correct pasture yields
#' @return Magpie objects with results on cellular level.
#' @param x magpie object provided by the read function
#' @author Marcos Alves
#' @seealso
#'   \code{\link{readPastYields}}
#' @examples
#'
#' \dontrun{
#'   readSource("PastYields", subtype = "ContinousGrazing:elevatedCO2:rcp8p5.multiple")
#' }
#'
#' @importFrom madrat toolConditionalReplace

correctPastYields <- function(x){

  x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)

  return(x)
}
