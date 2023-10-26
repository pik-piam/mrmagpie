#' @title calcFoodDemandGridded
#' @description Calculates grid-level food demand, note also includes food and feed
#' @return Gridded magpie object of food demand disaggregated by rural urban pop
#' @param attribute dm or calories ("ge") or other massbalance attribute
#' @param feed whether to include feed demand in the gridded demand
#' @param prod for memory reasons
#' @author David M Chen
#' @importFrom withr local_options
#' @importFrom magpiesets findset
#' @examples
#' \dontrun{
#' calcOutput("FoodDemandGridded")
#' }
#'
calcFoodDemandGridded <- function(attribute = "dm", prod = "k", feed = TRUE) {

  foodDemand <- calcOutput("FAOmassbalance", aggregate = FALSE)
  if (feed) {
    prods <- c("food", "feed", "flour1")
  } else {
    prods <- c("food", "flour1")
  }

  foodDemand <- collapseNames(foodDemand[, , prods][, , attribute])
  hist <- getYears(foodDemand)


  gridPop <- collapseNames(calcOutput("GridPop", aggregate = FALSE,
                                      source = "Gao", urban = TRUE)[, hist, "SSP2"])

  mapping <- toolGetMapping("CountryToCellMapping.csv", type = "cell", where = "mappingfolder")
  popAgg <- toolAggregate(gridPop, rel = mapping, from = "celliso", to = "iso") # not all countries

  share <- (gridPop / dimSums(popAgg, dim = 3))
  countries <- getItems(share, dim = 1.1)


  foodDisagg <- toolAggregate(foodDemand[countries, , ], rel = mapping,
                              from = "iso", to = "celliso")

  foodDisaggUrb <- foodDisagg * share

  #give all feed demand to rural
  foodDisaggUrb[, , "feed"][, , "rural"] <- foodDisaggUrb[, , "rural"][, , "feed"] +
    foodDisaggUrb[, , "urban"][, , "feed"]
  foodDisaggUrb[, , "urban"][, , "feed"] <- 0

  local_options(magclass_sizeLimit = 1e+12)

  prods <- findset(prod)
  #sum up demand dimension
  foodDisaggUrb <- dimSums(foodDisaggUrb[, , prods], dim = 3.2)

  return(list(x = foodDisaggUrb,
              weight = NULL,
              unit = "Mt",
              description = "Food demand in by grid cell",
              isocountries = FALSE))
}
