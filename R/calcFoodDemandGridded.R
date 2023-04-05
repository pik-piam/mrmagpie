#' @title calcFoodDemandGridded
#' @description Calculates grid-level food demand
#' @return Gridded magpie object of food demand disaggregated by rural urban pop
#' @param attribute dm or calories ("ge") or other massbalance attribute
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("FoodDemandGridded")
#' }
#'
calcFoodDemandGridded <- function(attribute = "dm") {

# Country-level food demand
foodDemand <- calcOutput("FAOmassbalance", aggregate = FALSE)
foodDemand <- dimSums(foodDemand[, , c("food", "feed", "flour1")],
                      dim = 3.2)[, , attribute]
hist <- getYears(foodDemand)

# Gridded population
gridPop <- collapseNames(calcOutput("GridPop", aggregate = FALSE,
                                    cellular = TRUE, cells = "lpjcell",
                                    source = "Gao", urban = TRUE)[, hist, "SSP2"])
# Country-level population
popAgg <- dimSums(gridPop, dim = c("x", "y"))

share <- (gridPop / dimSums(popAgg, dim = 3))
countries <- getItems(share, dim = "iso")

# Disaggregate food demand to gridded level
mapping <- toolGetMappingCoord2Country()
mapping$coordiso <- paste(mapping$coords, mapping$iso, sep = ".")
foodDisagg <- toolAggregate(foodDemand[countries, , ], rel = mapping,
                            from = "iso", to = "coordiso")

foodDisaggUrb <- foodDisagg * share

return(list(x = foodDisaggUrb,
            weight = NULL,
            unit = "Mt",
            description = "Food demand in by grid cell",
            isocountries = FALSE))
}
