#' @title calcNonLocalTransport
#' @description Calculates grid-level amount of food that would be transported,
#' assuming that food produced in the grid cell is first consumed by local population
#' i.e. amount of food greater than local rural demand
#' @author David M Chen
#' @examples
#' \dontrun{
#' calcOutput("NonLocalTransport")
#' }

calcNonLocalTransport <- function() {

productionPri <- calcOutput("Production", aggregate = FALSE,
                            cellular = TRUE, products = "kcr")
productionLi <- calcOutput("Production", aggregate = FALSE,
                           cellular = TRUE, products = "kli")
production <- collapseNames(mbind(productionPri, productionLi)[,,"dm"])

foodDisaggUrb <- calcOutput("FoodDemandGridded", aggregate = FALSE)

foodDisaggUrb <- collapseNames(foodDisaggUrb[,,getItems(production, dim = 3)])

#Calculate how much food can go to cities and be exported (both incur transport costs)
ruralCons <- ifelse(production > foodDisaggUrb[,, "rural"], 
                    collapseNames(foodDisaggUrb[,, "rural"]),
                    production)

transpRurToUrb <-  ifelse(production > dimSums(foodDisaggUrb, dim = 3.2),
                          collapseNames(foodDisaggUrb[,, "urban"]),
                          collapseNames(production - foodDisaggUrb[,, "rural"]))
transpRurToUrb[which(transpRurToUrb < 0)] <- 0

transpExp <- ifelse(production > dimSums(foodDisaggUrb, dim = 3.2),
                    collapseNames(production - dimSums(foodDisaggUrb, dim = 3.2)),
                    0)

amtTransported <- transpRurToUrb + transpExp


return(list(x = amtTransported,
            weight = NULL,
            unit = "Mt dm",
            description = "Food produced greater than local consumption",
            isocountries = FALSE))
}
