#' @title calcMowing_new
#' @description Calculates mowing pasture yields
#' @param mowing_events number of mowing events per year expressed as `2me`
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param scenario specify ssp scenario
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("Mowing_new", mowing_events = "2me", lpjml = "LPJml_mowing", climatetype)
#' }
#'


calcMowing_new <- function(mowing_events = "me2", lpjml = "lpjml5.2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN") {


  .subtype <- paste(lpjml, climatetype,paste0(scenario,"_", mowing_events),sep = ":")
  hist <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_hist", sep = ":"), convert = F))
  scen <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_scen", sep = ":"), convert = F))
  x <- mbind(hist,scen)
  x <- x[, , "mgrass"]
  getNames(x) <- gsub("mgrass", "mowing", getNames(x))

  return(list(
    x = x,
    weight = NULL,
    unit = "tDM/ha/yr",
    description = paste("Mowing_new pasture yields"),
    isocountries = FALSE
  ))
}
