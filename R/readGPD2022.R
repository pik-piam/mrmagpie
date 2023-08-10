#' @title readGPD2022
#' @description read x
#' Data from the Global Peatland Database provided by Alexandra Barthelmes.
#' The original xls file has been clean-up manually (country names). Turkey had
#' two identical entries in the original xls file.
#' Sources:
#' "Inventory Reports and National Communications UNFCC 2014",
#' "soil and peatland science",
#' "European Mires Book" ,
#' "own estimates (incl. GIS data)",
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' readSource("x", convert = "onlycorrect")
#' }
#' @importFrom readxl read_xls

readGPD2022 <- function() {
  # read-in xls file Global Peatland Database
  x <- read_xls("GPD2022.xls")
  x <- x[x$ISO3 != "NA", ]
  x <- x[, names(x)[-2]]

  # convert to magclass object
  x <- as.magpie(x, spatial = 1)
  x[is.na(x)] <- 0

  return(x)
}
