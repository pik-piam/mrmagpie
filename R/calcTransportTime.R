#' @title calcTransportTime
#' @description Function extracts travel time to major cities in minutes
#' @param subtype currently only cities of 5, 20, or 50 thousand people ("cities5", "cities20", "cities50") or
#' ports of various sizes ("portsLarge|Medium|Small|VerySmall|Any")
#' @param cells number of cells to be returned: magpiecell (59199), lpjcell (67420)
#' @return magpie object in cellular resolution
#' @author David Chen
#'
#' @examples
#' \dontrun{
#' calcOutput("TransportTime", aggregate = FALSE)
#' }
#'
calcTransportTime <- function(subtype = "cities50", cells = "magpiecell") {

  x <- readSource("TravelTimeNelson2019", subtype = subtype, convert = FALSE)

   if (cells == "magpiecell") {
      x <- toolCoord2Isocell(x)
    }

  weight <- calcOutput("CellArea", cells = cells, aggregate = FALSE)
  return(list(
    x = x,
    weight = weight,
    unit = "Travel Time (minutes)",
    description = "Travel time to major cities or ports",
    isocountries = FALSE))
}
