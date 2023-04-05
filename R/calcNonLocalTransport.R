#' @title calcNonLocalTransport
#' @description Calculates grid-level amount of food that would be transported,
#' assuming that food produced in the grid cell is first consumed by local population
#' i.e. amount of food greater than local rural demand, split into that which feeds the local
#' urban population, and that which exceeds total local demand and is available to export
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("NonLocalTransport")
#' }

calcNonLocalTransport <- function() {

  productionPri <- calcOutput("Production",  products = "kcr",
                              cellular = TRUE, cells = "lpjcell",
                              aggregate = FALSE,)
  productionLi <- calcOutput("Production", products = "kli",
                             cellular = TRUE, cells = "lpjcell",
                             aggregate = FALSE)
  production <- collapseNames(mbind(productionPri, productionLi)[, , "dm"])

  foodDisaggUrb <- calcOutput("FoodDemandGridded", aggregate = FALSE)

  foodDisaggUrb <- collapseNames(foodDisaggUrb[, , getItems(production, dim = 3)])

  # Calculate how much food can go to cities and be exported (both incur transport costs)
  ruralCons <- ifelse(production > foodDisaggUrb[,, "rural"],
                      collapseNames(foodDisaggUrb[,, "rural"]),
                      production)

  transpRurToUrb <-  ifelse(production > dimSums(foodDisaggUrb, dim = 3.2),
                            collapseNames(foodDisaggUrb[,, "urban"]),
                            collapseNames(production - foodDisaggUrb[,, "rural"]))
  transpRurToUrb[which(transpRurToUrb < 0)] <- 0
  transpRurToUrb <- add_dimension(transpRurToUrb, dim = 3.2, nm = "RurToUrb")

  transpExp <- ifelse(production > dimSums(foodDisaggUrb, dim = 3.2),
                      collapseNames(production - dimSums(foodDisaggUrb, dim = 3.2)),
                      0)
  transpExp <- add_dimension(transpExp, dim = 3.2, nm = "Exported")

  out <- mbind(transpRurToUrb, transpExp)

  return(list(x = out,
              weight = NULL,
              unit = "Mt dm",
              description = "Food produced that is transported to
                          local city, and amount that is transported and exported",
              isocountries = FALSE))
}
