#' @title calcLuh2SideLayers
#' @description Function extracts biodiversity data for LUH2 land cover types
#'
#' @param cells  number of cells to be returned: magpiecell (59199), lpjcell (67420)
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("Luh2SideLayers", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset
#'

calcLuh2SideLayers <- function(cells = "lpjcell") {

  x  <- readSource("BendingTheCurve", subtype = "luh2_side_layers", convert = "onlycorrect")

  if (cells == "magpiecell") {

      out <- toolCoord2Isocell(x)

  } else if (cells == "lpjcell") {

      out <- x

  } else {
      stop("Please specify cells argument")
  }

  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

return(list(
  x = x,
  weight = weight,
  unit = "boolean",
  description = paste0("Data from LUH2 provided by David Leclere from IIASA, ",
                       "Bending the curve on biodiversity loss"),
  isocountries = FALSE))
}
