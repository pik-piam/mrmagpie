#' @title calcLivestockDistribution
#' @description Disaggregate Livestock estimates based on the GLW3 dataset.
#' @param cells       "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
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
calcLivestockDistribution <- function(cells = "lpjcell") {

  past <- findset("past")
  past <- past[7:length(past)]

  #############################
  ### Disaggregation weights###
  #############################

  mapping <- toolGetMappingCoord2Country()
  glw3    <- readSource("GLW3", subtype = "Da", convert = "onlycorrect")
  glw3    <- glw3[paste(mapping$coords, mapping$iso, sep = "."), , ]
  getItems(glw3, dim = 1, raw = TRUE) <- mapping$coords

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
  dimnames(conversionRateLSU)  <- list("GLO", "year", c("large", "small"))

  livestockFAOscaled           <- livestockFAO * conversionRateLSU * fbaskPastureFraction
  livestockFAOscaled           <- dimSums(livestockFAOscaled[, , c(1, 4)])
  getItems(livestockFAOscaled, dim = 2) <- past
  getSets(livestockFAOscaled)  <- c("iso", "year", "data")
  livestockFAOscaled           <- livestockFAOscaled[intersect(getItems(livestockFAOscaled, dim = 1), 
                                                               unique(mapping$iso)), , ]
  livestockCell <- toolAggregate(livestockFAOscaled, rel = mapping,
                                 from = "iso", to = "coords",
                                 weight = glw3)[mapping$coords, , ]
  getItems(livestockCell, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")

  livestockCell                <- livestockCell / 1e6

  if (cells == "magpiecell") {
    livestockCell <- toolCoord2Isocell(livestockCell)
  }

  return(list(x = livestockCell,
              weight = NULL,
              unit = "Total Livestock numbers per cell (mio)",
              description = "Pasture correction factor for the historical dates",
              isocountries = FALSE))
}
