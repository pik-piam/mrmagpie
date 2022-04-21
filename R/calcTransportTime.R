#' @title calcTransportTime
#' @description Function extracts travel time to major cities in minutes
#' @param object cities or ports
#' @param minDist 5 or 50 for cities, Large|Medium|Small|Very Small|Any for ports. For Cities, indicates
#' 5 000 or 20 000 or 50 000 as settlement population size for travel time to, while .
#'
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{ calcOutput("TransportTime", aggregate = FALSE) }
#'
#'

calcTransportTime <- function(object = "cities", minDist = 5) {

  x <- readSource("TravelTimeNelson2019", subtype = object, convert = FALSE)

  if (object == "cities") {
    if (minDist == 50) {
      out <- x[,,"travel_time_to_cities_11"]
    } else if (minDist == 20) {
      out <- x[,,"travel_time_to_cities_10"]
    } else if (minDist == 5) {
      out <- x[,,"travel_time_to_cities_12"]
    }

  else {stop("only minDist for cities are 5, 20 and 50 thousand people")
      }
  }

  else if (object == "ports") {
    portSizes <- setNames(seq(1, 5), c("Large", "Medium", "Small", "Very Small", "Any"))

    if (minDist %in% names(portSizes)) {
      layerName <- paste0("travel_time_to_ports_", portSizes[minDist])
      out <- x[,,layerName]
    } else {
      stop("only Large, Medium Small, Very Small, or Any allowed for port minDist")
      }
  }

  weight <- calcOutput("GridPop_new", aggregate=FALSE)[,2015,"SSP2"]

  return(list(
    x = out,
    weight = weight,
    unit = "Travel Time (minutes)",
    description = "Travel time to major cities or ports Nelson 2019",
    isocountries = FALSE))
}
