#' @title calcLabourProdImpactEmu
#' @description Spatial and temporal aggr. of labour productivity impacts from climate change emulated by LAMACLIMA
#' @description based on method of Orlov et al. 2019. Economics of Disasters and Climate Change, 3(3), 191-211.
#' @param timestep 5-year or yearly
#' @param cellular cellular is true
#' @return List of magpie object of gridded (0.5) productivity as percentage of full labour prod 1
#' @author Michael Windisch
#' @importFrom magclass dimSums

calcLabourProdImpactEmu <- function(timestep = "5year", cellular = TRUE) {

  out <- readSource("LabourProdImpactEmu", convert = "onlycorrect")

  if (timestep == "5year") {
    out <- out[, seq(1995, 2095, 5), ]
  } else if (timestep == "yearly") {
    out <- out
  }

  out <- (100-out)/100

  avl_crop_area_weight <- calcOutput("AvlCropland",marginal_land="all_marginal",cells="magpiecell",country_level=FALSE,aggregate=FALSE)
  avl_crop_area_weight[avl_crop_area_weight == 0] <- 10^-10


  return(list(
    x = out,
    weight = avl_crop_area_weight,
    unit = "Percentage of total labour productivity loss",
    description = "Labour productivity impacts as percentage of full labour prod 1",
    isocountries = FALSE))

}
