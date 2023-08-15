#' @title correctGPM2
#' @description correct peatland area
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#'
#' \dontrun{
#'   readSource("GPM2", convert="onlycorrect")
#' }

correctGPM2 <- function(x) {

  x[is.na(x)] <- 0

  return(x)
}
