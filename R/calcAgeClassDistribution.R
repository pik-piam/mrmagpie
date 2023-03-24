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

  # Cell fraction from poulter data set
  poulterDataset <- readSource("GFAD", convert = "onlycorrect")

  # Calculate cellarea
  mapping <- toolGetMappingCoord2Country()
  cb <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                      type = "cell", where = "mrcommons")
  cellArea <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cb$lat / 180 * pi)
  cellArea <- as.magpie(cellArea, spatial = 1)
  getItems(cellArea, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")
  getSets(cellArea) <- c("x", "y", "iso", "year", "data")

  ######################
  forestArea <- poulterDataset * cellArea

  forestArea <- dimSums(forestArea, dim = 3.1)

  getNames(forestArea) <- gsub(pattern = "X", replacement = "class", x = getNames(forestArea))

  zeroForestArea <- dimSums(forestArea, dim = 3)

  acDistribution <- forestArea / dimSums(forestArea, dim = 3)

  ## Set age classes to 0 where forest does not exist
  ## Only checking where zero forest area exists
  acDistribution[magclass::where((setYears(zeroForestArea, "y2000")) == 0)$true$regions, , ] <- 0

  out <- acDistribution

  names(dimnames(out))[1] <- "ISO.cell"

  return(list(x = out,
              weight = cellArea,
              unit = "1",
              description = "Fraction of each age class in secondary forest from each spatially
                             explicit cell as described in Poulter age classes",
              isocountries = FALSE))
}
