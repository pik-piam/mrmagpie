#' @title correctBphMask
#'
#' @description correct cellnames
#' @return magpie objects on cellular level
#' @param x magpie object provided by the read function
#'
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#'   readSource("readBphMask", convert="onlycorrect")
#' }

correctBphMask <- function(x){

  #might be added
  #x <- toolConditionalReplace(x, conditions = c("is.na()","<0"), replaceby = 0)
  x <- toolCell2isoCell(x)

  return(x)
}
