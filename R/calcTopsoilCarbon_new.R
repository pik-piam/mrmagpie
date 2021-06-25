#' @title calcTopsoilCarbon_new
#' @description This function extracts topsoil carbon densities from LPJ to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#' @param fromFlows TRUE, if calculated from harmonized flows
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("TopsoilCarbon", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom mrcommons toolCoord2Isocell

calcTopsoilCarbon_new <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_44ac93de", crop="ggcmi_phase3_nchecks_9ca735cb"),
                                  climatetype="GSWP3-W5E5:historical", fromFlows = FALSE){

  if(climatetype=="GSWP3-W5E5:historical"){ stage <- "smoothed"
  } else{                                   stage <- "harmonized2020"}

  soilc_layer_natveg <- toolCoord2Isocell(calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype,
                                                     subtype="soilc_layer", stage=stage, aggregate=FALSE))

  # Check for NAs
  if(any(is.na(topsoilc))){
    stop("produced NA Carbon")
  }

  if (fromFlows) {

    .getCPoolsFromFlows <- function(pool, flow, refYear) {
      # calculate carbon pools from carbon flows
      years <- getYears(pool, as.integer = TRUE)
      out   <- pool
      for (y in years[years > refYear]) out[, y, ] <- setYears(pool[, y - 1, ], y) + flow[, y, ]
      out   <- toolConditionalReplace(out, "<0", 0)
      return(out)
    }

    flow <- calcOutput("CarbonSink", version = lpjml["natveg"], climatetype = climatetype,
                       stage = "harmonized2020", pool = "soilc_layer", aggregate = FALSE)
    soilc_layer_natveg <- .getCPoolsFromFlows(soilc_layer_natveg, flow, 1995)
  }

  topsoilc           <- soilc_layer_natveg[,,1] + 1/3 * soilc_layer_natveg[,,2]
  getNames(topsoilc) <- "topsoilc"
  weight             <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven",
                                           fao_corr=TRUE, input_magpie=TRUE, years="y1995", round=6), dim=3)

  return(list(
    x=topsoilc,
    weight=weight,
    unit="t per ha",
    description="Topsoil carbon in tons per hectar for natural vegetation.",
    isocountries=FALSE))
}
