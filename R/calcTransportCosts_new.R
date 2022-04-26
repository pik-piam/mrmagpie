#' @title calcTransportCosts_new
#' @description calculates country-level transport costs from GTAP total
#' transport costs, cellular production, and cellular travel time
#'
#'#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
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
  cftRel[["gro"]] <- c("maiz","trce","begr","betr")
  cftRel[["v_f"]] <- c("others","potato","cassav_sp","puls_pro")
  cftRel[["osd"]] <- c("soybean","oilpalm","rapeseed","sunflower","groundnut")
  cftRel[["c_b"]] <- c("sugr_beet","sugr_cane")
  cftRel[["ocr"]] <- c("foddr")


  #calculate transport power (amount * distance)
  transportPower <- new.magpie(0, cells_and_regions = getItems(distance, dim = 1),
                               years = "y2005",
                               names = names(cftRel))

  #create average transport costs per ton per distance dummy
  transportPerTonPerDistance <- NULL
  mapping <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")

  #sum up distances across gtap crops and aggregate to iso level, divide costs by distance
  for (i in 1:length(cftRel)) {

    transportPower <- new.magpie(0, cells_and_regions = getItems(distance, dim = 1),
                                 years = "y2005",
                                 names = names(cftRel)[i])

    if (length(cftRel[[i]])>1) {
      transportPower[,,names(cftRel)[i]] <- dimSums(production[,,cftRel[[i]]], dim = 3)*distance
    } else {
      transportPower[,,names(cftRel)[i]] <- production[,,cftRel[[i]]]*distance
    }
    transportPower <- toolAggregate(x = transportPower[,,names(cftRel)[i]],
                                    rel = mapping, from = "celliso", to = "iso")
    transportPower <- toolCountryFill(transportPower, fill = 0)

    transportPerTonPerDistance <- mbind(transportPerTonPerDistance,
                                       (transportGtap[,,names(cftRel)[i]]/transportPower[,1,names(cftRel)[i]]))
  }


  #create array with MAgPIE cft
  magpieComms <- unlist(cftRel)

  transportMagpie <- new.magpie(0, cells_and_regions = getItems(transportPerTonPerDistance, dim = 1),
                                years = getItems(transportPerTonPerDistance, dim = 2),
                                names = magpieComms)

  for (i in getNames(transportMagpie)) {
    transportMagpie[,,i] <- transportPerTonPerDistance[,, names(cftRel)[grep(i, cftRel)]]
  }


  #production weight
  productionWeight <- calcOutput("Production", cellular = FALSE,
                           irrigation = FALSE, aggregate = FALSE)[, 2005, "dm"] * 10^6

  return(list(x = transportMagpie,
              weight = productionWeight,
              unit = "USD05",
              description = "Transport costs in USD per t dm per minute by country and product",
              isocountries = TRUE))

}
