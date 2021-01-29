#' @title calcMowing
#' @description Calculates mowing pasture yields
#' @param mowing_events number of mowing events per year expressed as `2me`
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("Mowing", mowing_events = "2me", lpjml = "LPJml_mowing", climatetype)
#' }
#'


calcMowing <- function(mowing_events = "2me", lpjml = "LPJml_mowing", climatetype = "HadGEM2_ES:rcp8p5:co2") {
  .subtype <- paste(paste(lpjml, climatetype, mowing_events, sep = ":"), "harvest", sep = ".")
  x <- readSource("LPJmL", subtype = .subtype, convert = "onlycorrect")
  x <- x[, , "mgrass"]
  getNames(x) <- gsub("mgrass", "mowing", getNames(x))

  return(list(
    x = x,
    weight = NULL,
    unit = "tDM/ha/yr",
    description = paste("Mowing pasture yields"),
    isocountries = FALSE
  ))
}
