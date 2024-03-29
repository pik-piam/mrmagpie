#' @title calcPeatland
#' @description This function calculates degraded and intact peatland area at cell level.
#' The function takes degraded and intact peatland area from the Global Peatland Database (GPD) at the national level and downscales the peatland area to grid cell level using gridded potential peatland area.
#' The GPD has been provided by Alexandra Barthelmes. The potential peatland area has been provided by Leifeld_2018 (DOI 10.1038/s41467-018-03406-6).
#' @param subtype degraded (default) or intact
#' @param cells "magpiecell" or "lpjcell"
#'
#' @return magpie object in cellular resolution
#' @author Florian Humpenoeder
#'
#' @examples
#' \dontrun{
#' calcOutput("Peatland", aggregate = FALSE)
#' }
#'
#' @importFrom magclass clean_magpie
#' @importFrom madrat toolGetMapping toolAggregate
#' @importFrom mrcommons toolIso2CellCountries

calcPeatland <-function(subtype = "degraded", cells = "lpjcell"){

  GPD         <- readSource("GPD", convert = TRUE)
  potPeatArea <- readSource("Leifeld2018", convert = "onlycorrect")

  # Total and drained peatland area
  PeatAreaTotal   <- collapseNames(GPD[, , "PeatAreaTotal"])
  names(dimnames(PeatAreaTotal)) <- c("iso", "t", "d3")
  PeatAreaDrained <- collapseNames(GPD[, , "PeatAreaDrained"])
  names(dimnames(PeatAreaDrained)) <- c("iso", "t", "d3")

  #Dissag. from country to cell
  #CountryToCell   <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  PeatAreaTotal   <- toolAggregate(x=toolIso2CellCountries(PeatAreaTotal, cells = "lpjcell"), rel = map, weight = potPeatArea, dim = 1, from = "iso", to = "coords")
  names(dimnames(PeatAreaTotal)) <- c("coords","t","d3")
  PeatAreaDrained <- toolAggregate(x=toolIso2CellCountries(PeatAreaDrained, cells = "lpjcell"), rel = map, weight = potPeatArea, dim = 1, from = "iso", to = "coords")
  names(dimnames(PeatAreaDrained)) <- c("coords", "t", "d3")

  # potPeatArea is the upper limit of peatland area in a cell; this will reduce the peatland area of GPD!
  PeatAreaTotal[PeatAreaTotal > potPeatArea] <- potPeatArea[PeatAreaTotal > potPeatArea]
  PeatAreaDrained[PeatAreaDrained > potPeatArea] <- potPeatArea[PeatAreaDrained > potPeatArea]

  # check; GPD is somewhat higher because we used potPeatArea as upper limit!
  # dimSums(GPD,dim=1)
  # dimSums(PeatAreaTotal,dim=1)
  # dimSums(PeatAreaDrained,dim=1)

  dimnames(PeatAreaTotal) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "d3" = NULL)
  dimnames(PeatAreaDrained) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "d3" = NULL)
  PeatAreaIntact <- PeatAreaTotal - PeatAreaDrained

  if(subtype=="degraded") {
    description <- "Degraded peatland area (Mha) in 0.5 degree resolution based on Humpenoeder et al 2020 (DOI 10.1088/1748-9326/abae2a)"
    x <- PeatAreaDrained
  } else if(subtype == "intact") {
    description <- "Intact peatland area (Mha) in 0.5 degree resolution based on Humpenoeder et al 2020 (DOI 10.1088/1748-9326/abae2a)"
    x <- PeatAreaIntact
  }

  if (cells == "magpiecell") {
    x <- mrcommons::toolCoord2Isocell(x, cells = cells)
  }

  return(list(x = x,
              weight = NULL,
              unit = "Mha",
              description = description,
              isocountries = FALSE))

}
