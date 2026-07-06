#' @title correctGAMI
#' @description Correct the GAMI v2.1 forest-age dataset: replace missing / negative
#' class-fraction values by 0 and attach ISO country codes to the coordinate cells.
#' @param x magpie object provided by \code{readGAMI}
#' @return magpie object on the 67420 lpj-cell grid, cleaned
#' @author Florian Humpenoeder
#' @seealso \code{\link{readGAMI}}
#' @examples
#' \dontrun{
#' readSource("GAMI", convert = "onlycorrect")
#' }
#' @importFrom madrat toolConditionalReplace
#' @importFrom mstools toolCoord2Isocoord
correctGAMI <- function(x) {
  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)
  x <- toolCoord2Isocoord(x)
  return(x)
}
