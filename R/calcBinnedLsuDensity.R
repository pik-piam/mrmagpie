#' @title calcRangeSoilCarbonHist
#' @description calculates soil carbon for rangelands
#'
#' @param breaks Binning breaks
#' @param labels Binning labels
#' @param years years where data should binned
#'
#' @return Magpie object with lsu per cell.
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("BinnedLsuDensity ", breaks, labels, years)
#' }


calcBinnedLsuDensity <- function( breaks = c(seq(0, 2, 0.1), 2.25, 2.5),
                                  labels = c(0.0, 0.2, 0.2, 0.4, 0.4, 0.6, 0.6, 0.8, 0.8, 1.0, 1.0, 1.2, 1.2, 1.4, 1.4, 1.6, 1.6, 1.8, 1.8, 2.0, 2.0, 2.5),
                                  years = 1995) {

  hist_lsu_ha <- calcOutput("LsuDensityHist", disagg_type = "grassland", aggregate = F)[,,"range"]
  hist_lsu_ha[hist_lsu_ha > 2.5] <- 2.5
  bin_hist_lsu_ha <- as.data.frame(hist_lsu_ha[,years,])
  breaks <- breaks
  labels <- labels
  bins <- cut(bin_hist_lsu_ha$Value, breaks = breaks, labels = labels, include.lowest = TRUE, right = F)
  bin_hist_lsu_ha$Value <- as.numeric(levels(bins)[bins])
  tmp <- as.magpie(bin_hist_lsu_ha[,"Value"], spatial = 1)
  tmp <- toolCell2isoCell(tmp)
  getYears(tmp) <- years
  getNames(tmp) <- "lsu_ha_hist"

  return(list(
    x = tmp,
    weight = NULL,
    unit = "Binned lsu/ha",
    description = "Historical rangelands lsu/ha ",
    isocountries = FALSE
  ))
}
