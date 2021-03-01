#' @title calcTopsoilCarbon
#' @description This function extracts topsoil carbon densities from LPJ to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("TopsoilCarbon", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom mrcommons toolCoord2Isocell

calcTopsoilCarbon <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"),
                                  climatetype="GSWP3-W5E5:historical"){

  if(climatetype=="GSWP3-W5E5:historical"){ stage <- "smoothed"
  } else{                                   stage <- "harmonized2020"}

  soilc_layer_natveg <- toolCoord2Isocell(calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype,
                                                     subtype="soilc_layer", stage=stage, aggregate=FALSE))
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
