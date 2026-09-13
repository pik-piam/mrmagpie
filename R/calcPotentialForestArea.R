#' @title calcPotentialForestArea
#'
#' @description Calculates the area than can be potentially covered by forests,
#'              based on environmental conditions.
#'
#' @param refData Determines the reference data that the estimated potential
#'                forest area is derived from (currently only "lpj")
#' @param countryLevel    Whether output shall be at country level.
#'                        Requires aggregate=FALSE in calcOutput.
#' @param lpjml   Defines LPJmL version for crop/grass and natveg specific inputs.
#'                Only relevant, if refData = "lpj".
#' @param climatetype Switch between different GCM climate scenarios.
#'                    Only relevant, if refData = "lpj".
#' @param grassyCorrection If TRUE, the potential forest area is reduced by the grassland-ecoregion
#'                    cover fraction (RESOLVE 2017 biomes 7-10, calcGrassyEcoregions), correcting the
#'                    LPJmL overestimation of forest in open grassy ecosystems.
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze, Florian Humpenoeder
#'
#' @examples
#' \dontrun{
#' calcOutput("PotentialForestArea", aggregate = FALSE)
#' }
#'

calcPotentialForestArea <- function(refData = "lpj",
                                    countryLevel = FALSE,
                                    lpjml = "lpjml5.10.0-m4",
                                    climatetype = "MRI-ESM2-0:ssp245",
                                    grassyCorrection = TRUE) {

  if (refData == "lpj") {

    vegc <- calcOutput("LPJmLHarmonize", lpjmlversion = lpjml,
                       climatetype = climatetype, subtype = "pnv:vegc",
                       aggregate = FALSE)

    potForest <- toolConditionalReplace(vegc, c("<20", ">=20"), c(0, 1))

    landArea <- calcOutput("LandArea", aggregate = FALSE)

    potForestArea <- potForest * landArea

    if (grassyCorrection) {
      # reduce the potential forest area by the grassland-ecoregion cover fraction, where LPJmL
      # overestimates forest in open grassy ecosystems
      grassy <- calcOutput("GrassyEcoregions", aggregate = FALSE)
      potForestArea <- potForestArea * (1 - setYears(grassy, NULL))
    }

  } else {
    stop("refData in calcPotentialForestArea not found. Please select existing data.")
  }

  getNames(potForestArea) <- NULL

  if (countryLevel) {
    out <- toolCountryFill(dimSums(potForestArea, dim = c("x", "y")), fill = 0)
  } else {
    out <- potForestArea
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Potential forest area",
              isocountries = countryLevel))
}
