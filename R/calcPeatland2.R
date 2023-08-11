#' @title calcPeatland2
#' @description This function calculates degraded and intact peatland area at cell level.
#' The function takes degraded and intact peatland area from the Global Peatland Database 2022 (GPD2022) at the
#' national level and downscales the peatland area to grid cell level using gridded peatland area
#' from the Global Peatland Map 2.0 (GPM2)
#' The data has been provided by Alexandra Barthelmes.
#' @param cells number of cells to be returned: magpiecell (59199), lpjcell (67420)
#' @return magpie object in cellular resolution
#' @author Florian Humpenoeder
#'
#' @examples
#' \dontrun{
#' calcOutput("Peatland2", aggregate = FALSE)
#' }
#'
#' @importFrom madrat toolAggregate

calcPeatland2 <- function(cells = "magpiecell") {
  # Country-level data on intact and degraded peatland from Global Peatland Database for 2022 (GPD2022)
  gpd2022 <- readSource("GPD2022", convert = TRUE)

  # Global Peatland Map 2.0; peatland location and extent; but no information on status peatlands (intact / degraded)
  gpm2 <- readSource("GPM2", convert = "onlycorrect")

  # Dissag. GPD2022 from country to cell with GPM2 as weight
  map <- mrcommons::toolGetMappingCoord2Country(pretty = TRUE)
  outCell   <- toolAggregate(x = mrcommons::toolIso2CellCountries(gpd2022, cells = "lpjcell"), rel = map,
                             weight = gpm2, dim = 1, from = "iso", to = "coords")
  names(dimnames(outCell)) <- c("coords", "t", "d3")
  dimnames(outCell) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "d3" = getNames(outCell))

  if (cells == "magpiecell") {
    outCell <- mrcommons::toolCoord2Isocell(outCell)
  }

  description <- "Intact and degraded peatland area (Mha) by land-use type, based GPD 2022 and GPM2.0"

  return(list(
    x = outCell,
    weight = NULL,
    unit = "Mha",
    description = description,
    isocountries = FALSE))
}
