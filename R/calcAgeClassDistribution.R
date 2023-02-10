#' @title calcAgeClassDistribution
#' @description This function calculates the share of each age class in secondary forests in each
#'              MAgPIE simulation cluster based on Global Forest Age Dataset from Poulter et al. 2019
#'
#' @return magpie object in cluster resolution
#' @author Abhijeet Mishra, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("AgeClassDistribution", aggregate = FALSE)
#' }
#'
#' @importFrom magclass where

calcAgeClassDistribution <- function() {

  poulterDataset <- readSource("GFAD", convert = "onlycorrect")  ## Poulter data is fraction of each cell

  # Area of cells
  mapping   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")

  ## magpie_coord is loaded automatically with package -- not when running line by line
  cb <- as.data.frame(magpie_coord)
  cellArea  <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cb$lat / 180 * pi)

  cellArea        <- as.data.frame(cellArea)
  cellArea$cell   <- mapping$celliso
  cellAreaMagpie <- as.magpie(cellArea[, c(2, 1)], filter = FALSE)
  getNames(cellAreaMagpie) <- NULL

  ######################

  getCells(poulterDataset) <- mapping$celliso

  forestArea <- poulterDataset * cellAreaMagpie

  forestArea <- dimSums(forestArea, dim = 3.1)

  getNames(forestArea) <- gsub(pattern = "X", replacement = "class", x = getNames(forestArea))

  zeroForestArea <- dimSums(forestArea, dim = 3)

  acDistribution <- forestArea / dimSums(forestArea, dim = 3)

  ## Set age classes to 0 where forest does not exist
  ## Only checking where zero forest area exists
  acDistribution[magclass::where((setYears(zeroForestArea, "y2000")) == 0)$true$regions, , ] <- 0

  out <- acDistribution

  names(dimnames(out))[1] <- "ISO.cell"

  return(list(
    x = out,
    weight = cellAreaMagpie,
    unit = "1",
    description = "Fraction of each age class in secondary forest from each spatially
                   explicit cell as described in Poulter age classes",
    isocountries = FALSE))
}
