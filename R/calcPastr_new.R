#' @title calcPastr_new
#' @description Calculates managed pasture yields
#' @param past_mngmt   pasture areas management option
#' @param lpjml        Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype  Switch between different climate scenarios (default: "CRU_4")
#' @param scenario     specify ssp scenario
#' @param cells       "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("Pastr_new", past_mngmt = "me2", lpjml = "LPJml_pastr", climatetype)
#' }
#'
calcPastr_new <- function(past_mngmt = "me2", # nolint
                          lpjml = "lpjml5p2_pasture", climatetype = "MRI-ESM2-0:ssp370",
                          scenario = "/co2/Nreturn0p5/limN", cells = "lpjcell") {

  yearsHist <- seq(1965, 2010, 5)
  .subtype <- paste0(lpjml, ":", climatetype, paste0(scenario, "/", past_mngmt))
  hist <- readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_hist", sep = ":"), convert = FALSE)
  scen <- readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_scen", sep = ":"), convert = FALSE)
  yearsScen <- seq(2015, max(getYears(scen, as.integer = TRUE)), 5)
  hist <- hist[, yearsHist, "mgrass"]
  scen <- scen[, yearsScen, "mgrass"]
  x <- mbind(hist, scen)
  magclass::getNames(x) <- gsub("mgrass", "pastr",  magclass::getNames(x))

  if (cells == "magpiecell") {
      x <- toolCoord2Isocell(x)
  }

  return(list(x = x,
              weight = NULL,
              unit = "gC/m2/y",
              description = "Managed pasture yields",
              isocountries = FALSE))
}
