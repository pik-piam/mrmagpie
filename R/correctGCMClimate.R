#' @title correctGCMClimate
#' @description Correct GCMs climate variables
#'              NOTE: This function will be depreciate soon, please use mrland::correctLPJmLClimate
#' @param x magpie object provided by the read function
#' @return Magpie objects with results on cellular level, weight, unit and description.
#' @author Marcos Alves, Felicitas Beier
#' @seealso
#' \code{\link{readGCMClimate}}
#' @examples
#'
#' \dontrun{
#' readSource("GCMClimate", subtype, convert="onlycorrect")
#' }
#'
#' @import magclass
#' @importFrom madrat toolConditionalReplace

correctGCMClimate <- function(x) { # nolint

  x <- toolConditionalReplace(x,
                              conditions = c("is.na()"),
                              replaceby = 0)

  return(x)
}
