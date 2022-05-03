#' @title calcFoodDemandGridded
#' @description Calculates grid-level food demand
#' @return Gridded magpie object of food demand disaggregated by rural urban pop
#' @param attribute dm or calories ("ge") or other massbalance attribute
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("FoodDemandGridded")
#' }

calcFoodDemandGridded <- function(attribute = "dm"){

foodDemand <- calcOutput("FAOmassbalance", aggregate = FALSE)
foodDemand <- foodDemand[,, "food"][,, attribute] #food or domestic supply?
hist <- getYears(foodDemand)


gridPop <- collapseNames(calcOutput("GridPop_new", aggregate = FALSE,
                                    source = "Gao", urban = TRUE)[, hist, "SSP2"])

mapping <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
popAgg <- toolAggregate(gridPop, rel = mapping, from = "celliso", to = "iso") #not all countries

share <- (gridPop / dimSums(popAgg, dim = 3))
countries <- getRegions(share)

foodDisagg <- toolAggregate(foodDemand[countries,,], rel = mapping,
                            from = "iso", to = "celliso")

foodDisaggUrb <- foodDisagg * share


return(list(x = foodDisaggUrb,
            weight = NULL,
            unit = "million current US$MER/yr",
            description = "Transport costs of GTAP commodities (half of input to market and market to value"))
}
