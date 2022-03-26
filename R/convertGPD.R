#' @title convertGPD
#' @description convert GPD
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on iso level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' readSource("GPD", convert = TRUE)
#' }
#'
convertGPD <- function(x) {

  # convert to Mha
  x <- x / 1000
  getCells(x) <- toolCountry2isocode(getCells(x))   # replace country names with iso3 codes
  # remove values for Netherland Antilles if they are 0 as they otherwise trigger warnings
  # but only remove if they are really 0 as otherwise a split and redistribution of the
  # data to nowadays existing ISO countries is required.
  if (all(x["ANT", , ] == 0)) {
    x <- x["ANT", , , invert = TRUE]
  }
  x <- toolCountryFill(x, fill = 0)       # fill missing countries with 0
  return(x)
}
