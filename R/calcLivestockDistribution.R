#' @title calcLivestockDistribution
#' @description Disaggregate Livestock estimates based on the GLW3 dataset.
#' @return MAgPIE objects with livestock numbers on a cellular level.
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' calcOutput("LivestockDistribution")
#' }
#'
#' @importFrom tidyr pivot_wider
#' @export
#'
calcLivestockDistribution <- function() {
  past <- findset("past")
  past <- past[7:length(past)]

  #############################
  ### Disaggregation weights###
  #############################

  mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
  glw3    <- readSource("GLW3", subtype = "Da", convert = "onlycorrect")

  #############################
  ### FAO livestock Numbers ###
  #############################

  lvstTypes    <- c("1746|Cattle and Buffaloes + (Total).stock", "1749|Sheep and Goats + (Total).stock")
  livestockFAO <- readSource("FAO", subtype = "LiveHead")[, past, lvstTypes]

  #################################
  ###        Feedbaskets        ###
  #################################

  fbask                <- calcOutput("FeedBasketsPast", aggregate = FALSE)
  fbaskRumPasture      <- dimSums(fbask[, past, c("alias_livst_rum", "alias_livst_milk")][, , "pasture"])
  fbaskRumTotal        <- dimSums(fbask[, past, c("alias_livst_rum", "alias_livst_milk")])
  fbaskPastureFraction <- fbaskRumPasture / fbaskRumTotal

  #################################
  ### Calculating LSU/ha        ###
  #################################

  # source calulation from EU statistics
  # https://docs.google.com/spreadsheets/d/1SZAAVl1SLwrrK6j329tq5zo1VZhfFtxUHTmsGQKYCCk/edit#gid=0
  conversionRateLSU            <- c(0.7, 0.1)
  conversionRateLSU            <- as.magpie(conversionRateLSU)
  dimnames(conversionRateLSU)  <- list("region", "year", c("large", "small"))

  livestockFAOscaled           <- livestockFAO * conversionRateLSU * fbaskPastureFraction
  livestockFAOscaled           <- dimSums(livestockFAOscaled[, , c(1, 4)])
  getYears(livestockFAOscaled) <- past
  livestockFAOscaled           <- livestockFAOscaled[unique(mapping$iso)]
  livestockCell                <- toolAggregate(livestockFAOscaled, rel = mapping,
                                                from = "iso", to = "celliso", weight = glw3)
  livestockCell                <- livestockCell / 1e6

  return(list(x = livestockCell,
              weight = NULL,
              unit = "Total Livestock numbers per cell (mio)",
              description = "Pasture correction factor for the historical dates",
              isocountries = FALSE))
}
