#' @title calcBphTCRE
#' @description Transient Climate Response to accumulated doubling of CO2.
#'              File based on CMIP5 +1perc CO2 per year experiment.
#'              To be used in the translation to carbon equivalents of BphEffect
#' @param cells lpjcell for 67420 cells or magpiecell for 59199 cells
#' @return magpie object in cellular resolution
#' @author Michael Windisch, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("BphTCRE", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource

calcBphTCRE <- function(cells = "magpiecell") {

  # load input data for BphTCRE
  a <- readSource("Windisch2021", subtype = "annmean_pertCha_05_EW1",
                  convert = "onlycorrect")

  d <- readSource("Windisch2021", subtype = "annstd_diff_pertCha_05_EW1",
                  convert = "onlycorrect")

  # prepare the statistical range
  x <- new.magpie(cells_and_regions = getCells(a),
                  years = NULL,
                  names = c("ann_TCREmean", "ann_TCREhigh", "ann_TCRElow"),
                  fill = 0)
  x[, , "ann_TCREmean"] <- a[, , 1]
  x[, , "ann_TCREhigh"] <- a[, , 1] + d[, , 1]
  x[, , "ann_TCRElow"]  <- a[, , 1] - d[, , 1]

  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

  if (cells == "magpiecell") {
    x      <- toolCoord2Isocell(x)
  }

  return(list(x = x,
              weight = weight,
              unit = "degC per tC per ha",
              description = "Local Transient Climate Response to tc per ha in degC",
              isocountries = FALSE))
}
