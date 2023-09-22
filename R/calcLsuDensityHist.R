#' @title calcLsuDensityHist
#' @description Calculate livestock historical livestock densities
#' @param disagg_type select the disaggregaton weights for biomass production (can be either grassland or livestock)
#' @param cells       "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("LsuDensityHist")
#' }
calcLsuDensityHist <- function(disagg_type = "grassland", cells = "lpjcell") { #nolint

  magYearsPast <- findset("past")
  biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, magYearsPast, "pasture"]
  biomass <- collapseNames(biomass)

  land <- calcOutput("LanduseInitialisation", cellular = TRUE, cells = "lpjcell",
                     nclasses = "nine", aggregate = FALSE)[, magYearsPast, ]
  grasslLand <- land[, , c("past", "range")]
  grasslLand <- setNames(grasslLand, c("pastr", "range"))
  grasslShares <- setNames(grasslLand[, , "pastr"] / dimSums(grasslLand, dim = 3), "pastr")
  grasslShares <- add_columns(grasslShares, addnm = "range", dim = 3.1)
  grasslShares[, , "range"] <- 1 - grasslShares[, , "pastr"]
  grasslShares[is.nan(grasslShares) | is.infinite(grasslShares)] <- 0

  livestock  <- setNames(readSource("GLW3"), "liv_numb")
  livstSplit <- livestock * grasslShares
  livstSplit <- collapseNames(livstSplit)
  livstSplitCtry <- dimSums(livstSplit, dim = c("x", "y"))

  livstShareCtry <- livstSplitCtry[, , "pastr"] / dimSums(livstSplitCtry, dim = 3)
  livstShareCtry[is.nan(livstShareCtry) | is.infinite(livstShareCtry)] <- 0
  livstShareCtry <- add_columns(livstShareCtry, addnm = "range", dim = 3.1)
  livstShareCtry[, , "range"] <- 1 - livstShareCtry[, , "pastr"]

  if (disagg_type == "livestock") {
    weight <- livstSplit
  } else {
    if (disagg_type == "grassland") {
      weight <- grasslLand
    } else {
      stop(paste0("disagg_type ", disagg_type, " is not supported"))
    }
  }

  # reduce number of countries to those in 67k mapping
  mapping <- toolGetMappingCoord2Country()
  biomass <- biomass[intersect(getItems(biomass, dim = 1),
                               unique(mapping$iso)), , ]

  biomassSplit <- biomass * livstShareCtry
  biomassSplitCell <- toolAggregate(biomassSplit, rel = mapping,
                                    weight = weight, from = "iso", to = "coords")

  # removing values above simulation
  lsuEq <- (8.9 * 365) / 1000 # tDM y-1
  lsus <- biomassSplitCell / lsuEq
  lsuHa <- lsus / grasslLand
  lsuHa[is.nan(lsuHa) | is.infinite(lsuHa)] <- 0
  lsuHa[lsuHa[, , "range"] > 2.5] <-  2.5
  lsuHa[lsuHa[, , "pastr"] > 10] <-  10

  if (cells == "magpiecell") {
    lsuHa <- toolCoord2Isocell(lsuHa)
    grasslLand <- toolCoord2Isocell(grasslLand)
  }

  return(list(
    x = lsuHa,
    weight = grasslLand,
    unit = "LSU/ha",
    description = "Cattle livestock density",
    isocountries = FALSE
  ))
}
