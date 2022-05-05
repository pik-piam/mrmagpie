#' @title calcPackagingMarketingCosts
#' @description calculates per-ton marketing and packaging costs for food that leaves a cell
#' Currnetly assume expert guess 50 USD / ton of packaging/marketing costs 
#' (100 USD/t in model, of which half is already in GTAP)
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David M Chen

calcPackagingMarketingCosts <- function() {
  
  packaged <- calcOutput("NonLocalTransport", aggregate = FALSE)[,,"Exported"]
  
  out <- 50 * packaged
  
  return(list(x = out,
              weight = NULL,
              unit = "million USD05",
              description = "Packaging costs in USD per t dm by country and product",
              isocountries = TRUE))
  
}
