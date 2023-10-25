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

readGPD2022 <- function() {
  # read-in csv file Global Peatland Database
  x <- utils::read.csv("GPD2022.csv", header = TRUE)
  x <- x[!is.na(x$ISO3), ]
  x <- x[, names(x)[-2]]

  # convert to magclass object
  x <- as.magpie(x, spatial = 1)
  x[is.na(x)] <- 0

  return(x)
}
