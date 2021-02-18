#' @title calcTopsoilCarbon_new
#' @description This function extracts topsoil carbon densities from LPJ to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("TopsoilCarbon_new", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset

calcTopsoilCarbon_new <- function(lpjml=c(natveg="LPJmL4", crop="LPJmL5"), climatetype){


  soilc_layer_natveg <- calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype, subtype="soilc_layer", stage="harmonized", aggregate=FALSE)
  topsoilc           <- soilc_layer_natveg[,,1] + 1/3 * soilc_layer_natveg[,,2]
  getNames(topsoilc) <- "topsoilc"

  # Check for NAs
  if(any(is.na(topsoilc))){
    stop("produced NA Carbon")
  }

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven", fao_corr=TRUE, input_magpie=TRUE, years="y1995", round=6), dim=3)

  return(list(
    x=topsoilc,
    weight=weight,
    unit="t per ha",
    description="Topsoil carbon in tons per hectar for natural vegetation.",
    isocountries=FALSE))
}
