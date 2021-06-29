#' @title calcPastr
#' @description Calculates managed pastures yields
#' @param past_mngmt pasture areas management option
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("Pastr", past_mngmt = "2me", lpjml = "LPJmL_pastr", climatetype)
#' }
#'
calcPastr <- function(past_mngmt = "2me", lpjml = "LPJmL_pastr", climatetype = "HadGEM2_ES:rcp8p5:co2") {
  .subtype <- paste(paste(lpjml, climatetype, past_mngmt, sep = ":"), "harvest", sep = ".")
  x <- readSource("LPJmL", subtype = .subtype, convert = "onlycorrect")
  x <- x[, , "mgrass"]
  getNames(x) <- gsub("mgrass", "pastr", getNames(x))

  return(list(
    x = x,
    weight = NULL,
    unit = "tDM/ha/yr",
    description = paste("Managed pastures yields"),
    isocountries = FALSE
  ))
}
