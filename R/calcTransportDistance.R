#' @title calcTransportDistance
#' @description Function extracts travel time to major cities in minutes
#' This function now deprecated - use calcTransportTime instead
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{
#' calcOutput("TransportDistance", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset
#'

calcTransportDistance <- function() {

  x      <- readSource("TransportDistance", convert = "onlycorrect")
  weight <- calcOutput("CellArea", aggregate = FALSE)

  return(list(
    x = x,
    weight = weight,
    unit = "Travel Time (minutes)",
    description = "Travel time to major cities Nelson 2008 EC JRC, see model documentation",
    isocountries = FALSE))
}
