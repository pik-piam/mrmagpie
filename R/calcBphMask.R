#' @title calcBphMask
#' @description Mask of Datapoints of biogeophysical temperature change of afforestation (degree C)
#'              to be used as weight.
#'              File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#' @param cells lpjcell for 67420 cells or magpiecell for 59199 cells
#' @return magpie object in cellular resolution
#' @author Michael Windisch, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("BphMask", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource

calcBphMask <- function(cells = "magpiecell") {

  # load BphMask data
  x <- readSource("Windisch2021", subtype = "refordefor_dT_ANN_nonzeromask_05",
                  convert = "onlycorrect")

  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

  if (cells == "magpiecell") {
    x      <- toolCoord2Isocell(x)
  }

  return(list(x = x,
              weight = weight,
              unit = "none",
              description = "Nonan Mask of BPH Effect Dataset",
              isocountries = FALSE))
}
