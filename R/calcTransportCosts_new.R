#' @title calcTransportCosts_new
#' @description calculates country-level transport costs from GTAP total
#' transport costs, cellular production, and cellular travel time
#' @param transport "all" or "nonlocal". "all" means all production incurs transport costs,
#' while "nonlocal" sees only production greater than local rural consumption with transport costs
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author David M Chen
#' @seealso
#' [calcTransportTime()],
#' [calcGTAPTotalTransportCosts()]
#' @examples
#' \dontrun{
#' calcOutput("TransportCosts_new")
#' }
#'
calcTransportCosts_new <- function(transport = "all") { # nolint

  # load distance (travel time), production, and gtap transport costs
  distance <- calcOutput("TransportTime", subtype = "cities50", cells = "magpiecell",  aggregate = FALSE)

  if (transport == "all") {

    production <- calcOutput("Production", cellular = TRUE,
                             irrigation = FALSE, aggregate = FALSE)[, 2005, "dm"] * 10^6
    productionLi <- calcOutput("Production", cellular = TRUE,
                               irrigation = FALSE, aggregate = FALSE,
                               products = "kli")[, 2005, "dm"] * 10^6
    production <- mbind(production, productionLi)

  } else if (transport == "nonlocal") {
    production <- calcOutput("NonLocalTransport", aggregate = FALSE)[, 2005, ] * 10^6
  } else {
    stop("only all or nonlocal production available for subtype")
  }

  transportGtap <- calcOutput("GTAPTotalTransportCosts", aggregate = FALSE)[, 2004, ] * 10^6
  # transform 10^6 USD -> USD
  # this is  in 2004 and 2007 current USD, and we don't have same year for travel time, and pretend GTAP is 2005

  # map gtap cfts to MAgPIE cfts
  cftRel <- list()
  cftRel[["pdr"]] <- c("rice_pro")
  cftRel[["wht"]] <- c("tece")
  cftRel[["gro"]] <- c("maiz", "trce", "begr", "betr")
  cftRel[["v_f"]] <- c("others", "potato", "cassav_sp", "puls_pro")
  cftRel[["osd"]] <- c("soybean", "oilpalm", "rapeseed", "sunflower", "groundnut")
  cftRel[["c_b"]] <- c("sugr_beet", "sugr_cane")
  cftRel[["ocr"]] <- c("foddr")
  cftRel[["ctl"]] <- c("livst_rum", "livst_milk")
  cftRel[["oap"]] <- c("livst_chick", "livst_egg", "livst_pig")

  # calculate transport power (amount * distance) &
  # create average transport costs per ton per distance dummy
  # calculate transport power (amount * distance)
  mapping <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")

  tmpPower <- new.magpie(0, cells_and_regions = getItems(distance, dim = 1),
                         years = "y2005",
                         names = names(cftRel))

  transportPower <- new.magpie(0, cells_and_regions = unique(mapping$iso),
                               years = "y2005",
                               names = names(cftRel))

  # create average transport costs per ton per distance dummy
  transportPerTonPerDistance <- NULL

  # sum up distances across gtap crops and aggregate to iso level, divide costs by distance
  for (i in seq_along(cftRel)) {

    if (length(cftRel[[i]]) > 1) {
      tmpPower[, , names(cftRel)[i]] <- dimSums(production[, , cftRel[[i]]], dim = 3) * distance
    } else {
      tmpPower[, , names(cftRel)[i]] <- dimSums(production[, , cftRel[[i]]], dim = 3) * distance
    }
    transportPower[, , names(cftRel)[i]] <- toolAggregate(x = tmpPower[, , names(cftRel)[i]],
                                                          rel = mapping, from = "celliso", to = "iso")

    tpFilled <- toolCountryFill(transportPower, fill = 0) # need to fill first to divide
    transportPerTonPerDistance <- mbind(transportPerTonPerDistance,
                                        (transportGtap[, , names(cftRel)[i]] / tpFilled[, , names(cftRel)[i]]))
  }

  # fill the transport power object to use as weights at the end
  transportPower <- toolCountryFill(transportPower, fill = 0)

  # Rename GTAP to MAgPIE commodities
  magpieComms <- unlist(cftRel)

  transportMagpie <- transportPowerMagpie <- new.magpie(0,
                                                         cells_and_regions = getItems(transportPerTonPerDistance,
                                                                                      dim = 1),
                                                         years = "y2005",
                                                         names = magpieComms)

  for (i in getNames(transportMagpie)) {
    transportMagpie[, , i] <- toolFillWithRegionAvg(transportPerTonPerDistance[, , names(cftRel)[grep(i, cftRel)]],
                                                    valueToReplace = Inf, warningThreshold = 0.99)
    # warning threshold is so high as there is a lack of foddr in SSA
    transportPowerMagpie[, , i] <- transportPower[, , names(cftRel)[grep(i, cftRel)]]
  }


  # add wood and woodfuel with foddr costs and pasture with 0 costs (not transported)
  transportMagpie <- add_columns(transportMagpie, addnm = c("pasture", "wood", "woodfuel"),
                                 dim = 3.1, fill = 0)
  transportMagpie[, , c("wood", "woodfuel")] <- transportMagpie[, , "foddr"]

  # what weight to add for these?
  transportPowerMagpie <- add_columns(transportPowerMagpie, addnm = c("pasture", "wood", "woodfuel"),
                                      dim = 3.1, fill = 0)
  transportPowerMagpie[, , c("wood", "woodfuel")] <- transportPowerMagpie[, , "foddr"]


  return(list(x = transportMagpie,
              weight = transportPowerMagpie,
              unit = "USD05",
              description = "Transport costs in USD per t dm per minute by country and product",
              isocountries = TRUE))

}
