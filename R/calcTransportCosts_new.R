#' @title calcTransportCosts_new
#' @description calculates country-level transport costs from GTAP total
#' transport costs, cellular production, and cellular travel time
#'
#'#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David M Chen
#' @seealso
#' [calcTransportTime()],
#' [calcGTAPTotalTransportCosts()]
#' @examples
#' \dontrun{
#' calcOutput("TransportCosts_new")
#' }

calcTransportCosts_new <- function() {

  #load distance (travel time), production, and gtap transport costs
  distance <- calcOutput("TransportTime", minDist = 50, aggregate = FALSE)

  # read from magpie or from input data? # convert to tDM ? #production also only goes up to 2010..
  production <- calcOutput("Production", cellular = TRUE,
                           irrigation = FALSE, aggregate = FALSE)[, 2005, "dm"] * 10^6


  transportGtap <- calcOutput("GTAPTotalTransportCosts", aggregate = FALSE)[, 2004, ] * 10^6
  #transform 10^6 USD -> USD
  # this is  in 2004 and 2007 MER, don't have same year for travel time....


  #map gtap cfts to MAgPIE cfts
  cftRel <- list()
  cftRel[["pdr"]] <- c("rice_pro")
  cftRel[["wht"]] <- c("tece")
  cftRel[["gro"]] <- c("maiz", "trce", "begr", "betr")
  cftRel[["v_f"]] <- c("others", "potato", "cassav_sp", "puls_pro")
  cftRel[["osd"]] <- c("soybean", "oilpalm", "rapeseed", "sunflower", "groundnut")
  cftRel[["c_b"]] <- c("sugr_beet", "sugr_cane")
  cftRel[["ocr"]] <- c("foddr")


  #calculate transport power (amount * distance) &
  #create average transport costs per ton per distance dummy
  #calculate transport power (amount * distance)
  mapping <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")

  tmpPower <- new.magpie(0, cells_and_regions = getItems(distance, dim = 1),
                         years = "y2005",
                         names = names(cftRel))

  transportPower <- new.magpie(0, cells_and_regions = unique(mapping$iso),
                               years = "y2005",
                               names = names(cftRel))

  #create average transport costs per ton per distance dummy
  transportPerTonPerDistance <- NULL

  #sum up distances across gtap crops and aggregate to iso level, divide costs by distance
  for (i in 1:length(cftRel)) {

    if (length(cftRel[[i]]) > 1) {
      tmpPower[,,names(cftRel)[i]] <- dimSums(production[,,cftRel[[i]]], dim = 3) * distance
    } else {
      tmpPower[,,names(cftRel)[i]] <- production[,,cftRel[[i]]] * distance
    }
    transportPower[,,names(cftRel)[i]] <- toolAggregate(x = tmpPower[,,names(cftRel)[i]],
                                                        rel = mapping, from = "celliso", to = "iso")

    tP_filled <- toolCountryFill(transportPower, fill = 0) # need to fill first to divide
    transportPerTonPerDistance <- mbind(transportPerTonPerDistance,
                                        (transportGtap[,,names(cftRel)[i]]/tP_filled[,,names(cftRel)[i]]))
  }

  #fill the transport power object to use as weights at the end
  transportPower <- toolCountryFill(transportPower, fill = 0)

  #Rename GTAP to MAgPIE commodities
  magpieComms <- unlist(cftRel)

  transportMagpie <- transportPowerMagpie <-  new.magpie(0, cells_and_regions = getItems(transportPerTonPerDistance, dim = 1),
                                              years = "y2005",
                                              names = magpieComms)

  for (i in getNames(transportMagpie)) {
    transportMagpie[,, i] <- transportPerTonPerDistance[,, names(cftRel)[grep(i, cftRel)]]
    transportPowerMagpie[,, i] <- transportPower[,, names(cftRel)[grep(i, cftRel)]]
  }



  return(list(x = transportMagpie,
              weight = transportPowerMagpie,
              unit = "USD05",
              description = "Transport costs in USD per t dm per minute by country and product",
              isocountries = TRUE))

}
