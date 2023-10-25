#' @title calcFoodDemandGridded
#' @description Calculates grid-level food demand, note also includes food and feed
#' @return Gridded magpie object of food demand disaggregated by rural urban pop
#' @param attribute dm or calories ("ge") or other massbalance attribute
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("FoodDemandGridded")
#' }
#'
calcFoodDemandGridded <- function(attribute = "dm", feed = TRUE) {

foodDemand <- calcOutput("FAOmassbalance", aggregate = FALSE)
if (feed) {
    prods <- c("food", "feed", "flour1")
     } else { prods <- c("food", "flour1") }

foodDemand <- dimSums(foodDemand[, , prods],
                      dim = 3.2)[, , attribute]
hist <- getYears(foodDemand)


gridPop <- collapseNames(calcOutput("GridPop", aggregate = FALSE,
                                    source = "Gao", urban = TRUE)[, hist, "SSP2"])

mapping <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
popAgg <- toolAggregate(gridPop, rel = mapping, from = "celliso", to = "iso") # not all countries

share <- (gridPop / dimSums(popAgg, dim = 3))
countries <- getItems(share, dim = 1.1)

foodDisagg <- toolAggregate(foodDemand[countries, , ], rel = mapping,
                            from = "iso", to = "celliso")

foodDisaggUrb <- foodDisagg * share


return(list(x = foodDisaggUrb,
            weight = NULL,
            unit = "Mt",
            description = "Food demand in by grid cell",
            isocountries = FALSE))
}
