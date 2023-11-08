#' @title calcGCMClimate
#' @description Disaggregate CO2 global atmospheric concentration to cellular level
#'              NOTE: This function will be depreciate soon, please use mrland::calcLPJmLClimate
#' @param subtype type of climate data to collect,
#'                consisting of data source, GDM, RCP, time period, variable and time resolution
#'                separated by ":"
#' @param smooth  set averaging value for smoothing trajectories
#' @param cells    number of halfdegree grid cells to be returned.
#'                 Options: "magpiecell" (59199), "lpjcell" (67420)
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("GCMClimate", subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1850-2100:tas:annual_mean")
#' }
#'
#' @importFrom madrat toolSplitSubtype toolTimeAverage
#' @importFrom magclass getNames
#' @importFrom magpiesets findset
#' @importFrom mstools toolHoldConstant
#'

calcGCMClimate <- function(subtype = "ISIMIP3bv2:IPSL-CM6A-LR:ssp126:1850-2100:tas:annual_mean",
                               smooth = 0,
                               cells = "lpjcell") {
  ###### CONFIG ######
  splittingYear  <- 2014
  histName       <- "historical"
  time           <- findset("time")
  time           <- time[1:match("y2100", time)]
  ###### CONFIG ######

  x <- toolSplitSubtype(subtype,
                        list(version  = NULL, climatemodel = NULL,
                             scenario = NULL, period       = NULL,
                             variable = NULL, timeres      = NULL))

  period <- unlist(strsplit(x$period, "-|_"))

  .subtypeHist <- paste(x$version, x$climatemodel, histName,
                         paste(period[1], splittingYear, sep = "-"),
                         x$variable, sep = ":")
  .subtypeScen <- paste(x$version, x$climatemodel, x$scenario,
                         paste((splittingYear + 1), period[2], sep = "-"),
                         x$variable, sep = ":")

  y <- mbind(readSource("GCMClimate", subtype = .subtypeHist,
                          subset = x$timeres, convert = "onlycorrect"),
             readSource("GCMClimate", subtype = .subtypeScen,
                          subset = x$timeres, convert = "onlycorrect"))

  if (length(getItems(y, dim = 3)) == 1) {
    getNames(y) <- gsub("-", "_",
                        paste(x$timeres, x$variable, x$version, x$climatemodel, x$scenario,
                              sep = "_"))
  }

  if (smooth > 1) {
    y <- toolTimeAverage(y, averaging_range = smooth)
    y <- toolHoldConstant(y, time)
    y <- y[, time, ]
  } else {
    y <- y[, time, ]
  }

  unit <- switch(x$variable,
                 "tas"   = "Degree Celcius",
                 "pr"    = "mm/day",
                 "lwnet" = "watt per m2",
                 "rsds"  = "watt per m2",
                 "wet"   = "number of rainy days")

  description <- switch(x$variable,
                        "tas"   = paste0("Average ", x$timeres, " air temperature"),
                        "pr"    = "Average precipitation",
                        "lwnet" = "Long wave radiation",
                        "rsds"  = "Short wave radiation",
                        "wet"   = "number of rainy days")

  if (cells == "magpiecell") {
    y <- toolCoord2Isocell(y)
  }

  return(list(x            = y,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
