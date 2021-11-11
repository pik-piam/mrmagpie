#' @title calcProtectArea
#' @description Function extracts conservation protected area
#'
#' @param cells number of cells of landmask (select "magpiecell" for 59199 cells or "lpjcell" for 67420 cells)
#'
#' @return magpie object in cellular resolution with different protection scenarios
#' @author Felicitas Beier, David Chen
#'
#' @examples
#' \dontrun{
#' calcOutput("ProtectArea", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset addLocation
#'

calcProtectArea <- function(cells = "magpiecell") {

  # Protection Area mz file (conservation priority area in Mha)
  x <- readSource("ProtectArea", convert = "onlycorrect")

  # Half Earth Protection Share
  protectShr           <- readSource("HalfEarth", convert = "onlycorrect")
  getNames(protectShr) <- "HalfEarth"
  getSets(protectShr)  <- c("x", "y", "iso", "year",  "data")

  if (cells == "magpiecell") {

    protectShr <- toolCoord2Isocell(protectShr, cells = cells)

  } else if (cells == "lpjcell") {

    tmp <- collapseDim(addLocation(x), dim = c("region", "cell"))

    x   <- new.magpie(cells_and_regions = getCells(collapseDim(protectShr, dim = "iso")),
                      years = getYears(tmp),
                      names = getNames(tmp), fill = 0,
                      sets = c("x.y.iso", "year", "data"))
    x[getCells(tmp), , ] <- tmp

  } else {
    stop("Please select magpiecell or lpjcell in cells argument of calcProtectArea")
  }

  # Land area (in Mha):
  landArea <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = cells,
                          nclasses = "seven", fao_corr = TRUE, input_magpie = TRUE, years = "y1995", aggregate = FALSE)
  landArea <- dimSums(landArea, dim = 3)

  # Land area to be protected by 2050 (in Mha)
  protectArea <- protectShr * landArea

  # Add HalfEarth scenario to Protection Area file
  x <- mbind(x, protectArea)

  return(list(x            = x,
              weight       = NULL,
              unit         = "Mha",
              description  = "conservation priority areas",
              isocountries = FALSE))
}
