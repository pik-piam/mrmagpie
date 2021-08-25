#' @title correctHalfEarth
#' @description correct HalfEarth data
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author Felicitas Beier
#' @seealso
#'   \code{\link{readHalfEarth}}
#' @examples
#' \dontrun{
#' readSource("HalfEarth", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#' @importFrom mrcommons toolCell2isoCell toolCoord2Isocell
#' @importFrom magclass hasCoords

correctHalfEarth <- function(x) {
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  if (hasCoords(x)) {
    x <- toolCoord2Isocell(x, fillMissing = 0)
  } else {
    x <- toolCell2isoCell(x)
  }
  return(x)
}
