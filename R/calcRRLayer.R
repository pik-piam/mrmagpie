#' @title calcRRLayer
#' @description Function extracts range-rarity as used for biodiversity loss
#'
#' @param cells  number of cells to be returned: magpiecell (59199), lpjcell (67420)
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("RRLayer", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset
#'

calcRRLayer <- function(cells = "lpjcell") {

  x <- readSource("BendingTheCurve", subtype = "rr_layer", convert = "onlycorrect")

  if (cells == "magpiecell") {
      x <- toolCoord2Isocell(x)
  }
  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

return(list(
  x = x,
  weight = weight,
  unit = "Range-Rarity (-)",
  description = paste0("range-rarity layer provided by David Leclere from IIASA, ",
                       "Bending the curve on biodiversity loss"),
  isocountries = FALSE))
}
