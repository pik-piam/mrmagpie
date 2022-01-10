#' @title correctProtectArea
#' @description Read calibrated protection area file
#' @return magpie object on cellular level
#' @param x magpie object provided by the read function
#' @author David Chen, Felicitas Beier
#' @seealso
#'   \code{\link{readProtectArea}}
#' @examples
#'
#' \dontrun{
#'   readSource("ProtectArea", convert="onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#' @importFrom mrcommons toolCell2isoCell

correctProtectArea <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  # In this data set, FF stands for intact forest landscapes (IFL)
  getNames(x) <- gsub("FF", "IFL", getNames(x))

  return(x)
}
