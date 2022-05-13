#' @title correctGCMClimate_new
#' @description Correct GCMs climate variables
#' @param x magpie object provided by the read function
#' @return Magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves
#' @seealso
#' \code{\link{readGCMClimate_new}}
#' @examples
#'
#' \dontrun{
#' readSource("GCMClimate_new", subtype, convert="onlycorrect")
#' }
#'
#' @import magclass
#' @importFrom madrat toolConditionalReplace

correctGCMClimate_new <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)

  return(x)
}
