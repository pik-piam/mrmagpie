#' @title calcLabourProdImpactEmu
#' @description Spatial and temporal aggr. of labour productivity impacts from climate change emulated by LAMACLIMA
#' @description based on method of Orlov et al. 2019. Economics of Disasters and Climate Change, 3(3), 191-211.
#' @param timestep 5-year or yearly
#' @param cellular cellular is true
#' @param subtype impact for rcp based laborprod decrease, relief for LCLM based relief of impact
#' @param cells "magpiecell" or "lpjcell"
#' @return List of magpie object of gridded (0.5) labour productivity as percentage of full labour prod 1
#' @author Michael Windisch, Florian Humpenöder
#' @importFrom magclass dimSums mbind

calcLabourProdImpactEmu <- function(timestep = "5year", cellular = TRUE,
                                    subtype = "impact", cells = "lpjcell") {

  out <- readSource("LabourProdImpactEmu", convert = "onlycorrect")

  if (timestep == "5year") {
    out <- out[, seq(1995, 2095, 5), ]
  } else if (timestep == "yearly") {
    out <- out
  }

  out <- (100 - out) / 100

  if (subtype == "impact") {
    getNames(out) <- gsub("CTL_rcp119", "rcp119", getNames(out))
    getNames(out) <- gsub("CTL_rcp585", "rcp585", getNames(out))

    out <- mbind(out[, , "rcp119"], out[, , "rcp585"])
  } else if (subtype == "relief") {
    out <- mbind(out[, , "FRST"], out[, , "CROP"], out[, , "HARV"], out[, , "IRR"])
  }

  middle <- out[, , "ensmean"]
  getNames(middle) <- gsub("ensmean", "ensvalue", getNames(middle))
  std <- out[, , "ensstd"]
  getNames(std) <- gsub("ensstd", "ensvalue", getNames(std))


  upper <- middle + (1 - std)
  lower <- middle - (1 - std)

  upper[upper > 1] <- 1
  lower[lower < 0] <- 0

  getNames(upper)  <- gsub("ensvalue", "ensupper", getNames(upper))
  getNames(lower)  <- gsub("ensvalue", "enslower", getNames(lower))
  getNames(middle) <- gsub("ensvalue", "ensmean", getNames(middle))

  out <- mbind(lower, middle, upper)

  avlCropAreaWeight <- calcOutput("AvlCropland", cells = cells,
                                  marginal_land = "all_marginal:rainfed_and_irrigated",
                                  country_level = FALSE, aggregate = FALSE)
  avlCropAreaWeight[avlCropAreaWeight == 0] <- 10^-10

  if (cells == "magpiecell") {
    out <- mstools::toolCoord2Isocell(out, cells = cells, fillMissing = 1)
  }

  return(list(x            = out,
              weight       = avlCropAreaWeight,
              unit         = "Percentage of total labour productivity (1)",
              description  = "Remaining labour productivity as percentage of full labour
                              productivity after accounting for climate change impacts on
                              labour productivity",
              isocountries = FALSE))

}
