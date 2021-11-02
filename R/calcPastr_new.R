#' @title calcPastr_new
#' @description Calculates managed pasture yields
#' @param past_mngmt pasture areas management option
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param scenario specify ssp scenario
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("Pastr_new", past_mngmt = "2me", lpjml = "LPJml_pastr", climatetype)
#' }
#'


calcPastr_new <- function(past_mngmt = "me2", lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN") {

  years <- seq(1965,2100, 5)
  .subtype <- paste(lpjml, climatetype,paste0(scenario,"/", past_mngmt),sep = ":")
  hist <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_hist", sep = ":"), convert = F))
  scen <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_scen", sep = ":"), convert = F))
  x <- mbind(hist,scen)
  x <- x[, years, "mgrass"]
  getNames(x) <- gsub("mgrass", "pastr", getNames(x))

  return(list(
    x = x,
    weight = NULL,
    unit = "gC/m2/y",
    description = paste("Managed pasture yields"),
    isocountries = FALSE
  ))
}
