#' @title calcWaterUseNonAg
#' @description This function extracts non-agricultural water demand
#'
#' @param selectyears years to be returned
#' @param source      data source to be used (e.g. WATERGAP2020)
#' @param lpjml              Defines LPJmL version for crop/grass and natveg specific inputs
#' @param seasonality        grper (default): non-agricultural water demand in growing period per year; total: non-agricultural water demand throughout the year
#' @param climatetype        switch between different climate scenarios for calcGrowingPeriod
#' @param harmon_base_time   type of time smoothing applied before harmonization of WATERGAP data: average (average over 8-year time span around baseline year) or smoothing (time smoothing of baseline and WATERGAP scenario data) or NULL (no smoothing before harmonization)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("WaterUseNonAg", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput readSource toolTimeAverage toolTimeSpline toolFillYears
#' @importFrom magclass new.magpie getYears getCells getSets
#' @importFrom mrcommons toolCell2isoCell toolCoord2Isocell toolGetMappingCoord2Country
#' @importFrom magpiesets addLocation

calcWaterUseNonAg <- function(selectyears="all", source="WATCH_ISIMIP_WATERGAP", seasonality="grper", harmon_base_time="average",
                              lpjml=c(natveg="LPJmL4_for_MAgPIE_44ac93de", crop="ggcmi_phase3_nchecks_9ca735cb"),
                              climatetype="GSWP3-W5E5:historical") {

  #######################################
  ############ Calculations  ############
  #######################################

  # Old Non-Agricultural Waterdemand data (current default, will be deleted soon):
  if (source=="WATCH_ISIMIP_WATERGAP") {

    # Read in nonagricultural water demand:
    watdem_nonagr <- readSource("WATERGAP", convert="onlycorrect", subtype=source)
    # iso cell names
    watdem_nonagr <- toolCell2isoCell(watdem_nonagr)
    # Add year 2100
    watdem_nonagr <- toolFillYears(watdem_nonagr, seq(getYears(watdem_nonagr, as.integer=TRUE)[1],2100,by=5))
  }

  # New Non-Agricultural Waterdemand data (will be new default)
  if (source=="WATERGAP2020") {

    years_harmonized <- paste0("y",seq(2010,2020))
    years_future     <- paste0("y",seq(2020,2100))

    # Read in nonagricultural water demand:
    watdem_nonagr_WATERGAP      <- readSource("WATERGAP", convert="onlycorrect", subtype="WATERGAP2020")
    watdem_nonagr_ISIMIP_hist   <- readSource("ISIMIPinputs",subtype="ISIMIP3b:water:histsoc.waterabstraction",convert="onlycorrect")
    watdem_nonagr_ISIMIP_future <- readSource("ISIMIPinputs",subtype="ISIMIP3b:water:2015soc.waterabstraction",convert="onlycorrect")

    # Subset to common cells
    watdem_nonagr_ISIMIP_hist   <- watdem_nonagr_ISIMIP_hist[getCells(watdem_nonagr_WATERGAP),,]
    watdem_nonagr_ISIMIP_future <- watdem_nonagr_ISIMIP_future[getCells(watdem_nonagr_WATERGAP),,]

    ### Combine datasets from different sources:
    # historical and future ISIMIP data:
    watdem_ISIMIP <- mbind(watdem_nonagr_ISIMIP_hist, watdem_nonagr_ISIMIP_future)

    if (!is.null(harmon_base_time) && harmon_base_time=="smoothing") {
      # Time-Smoothing of historical baseline and projected WATERGAP data
      watdem_ISIMIP          <- toolSmooth(watdem_ISIMIP)
      watdem_nonagr_WATERGAP <- toolSmooth(watdem_nonagr_WATERGAP)
    }

    # empty magpie object
    cells <- getCells(watdem_nonagr_WATERGAP)
    years <- getYears(watdem_ISIMIP)
    names <- c(getNames(watdem_nonagr_WATERGAP), paste0("ISIMIP.",getNames(watdem_ISIMIP)))
    watdem_nonagr <- new.magpie(cells, years, names, sets=c("x.y", "year", "scenario.type"))

    # historical and future ISIMIP data
    watdem_nonagr[,getYears(watdem_ISIMIP),paste0("ISIMIP.",getNames(watdem_ISIMIP))] <- watdem_ISIMIP[,getYears(watdem_ISIMIP),getNames(watdem_ISIMIP)]

    # historical data provided by ISIMIP (same for all scenarios)
    watdem_nonagr[,getYears(watdem_nonagr_ISIMIP_hist),] <- watdem_nonagr_ISIMIP_hist[,getYears(watdem_nonagr_ISIMIP_hist),]

    # future WATERGAP scenarios (adjusted to transition year of historical data)
    watdem_nonagr_WATERGAP_adjusted     <- watdem_nonagr_WATERGAP
    watdem_nonagr_WATERGAP_adjusted[,,] <- NA

    # Calibration of WATERGAP data to ISIMIP baseline:
    # historical ISIMIP data in correct format for harmonization
    baseyear <- "y2010"
    cells <- getCells(watdem_nonagr_WATERGAP)
    years <- getYears(watdem_nonagr_ISIMIP_hist)
    names <- getNames(watdem_nonagr_WATERGAP)
    if (!is.null(harmon_base_time) && harmon_base_time=="average") {
      # average around baseyear of ISIMIP baseline
      watdem_ISIMIP[,years,] <- toolTimeAverage(watdem_ISIMIP, averaging_range=8)[,baseyear,]
    }
    tmp     <- new.magpie(cells, years, names, sets=c("x.y", "year", "scenario.type"))
    tmp[,,] <- 1
    tmp     <- tmp * watdem_ISIMIP[,years,]

    # Harmonization (Ref_year: 2010 because both ISIMIP historical (available until 2014) and WATERGAP (available from 2005) data are smoothed)
    watdem_nonagr_WATERGAP_adjusted <- toolHarmonize2Baseline(x=watdem_nonagr_WATERGAP, base=tmp, ref_year=baseyear, limited=TRUE, hard_cut=FALSE)

    # WATERGAP adjusted future scenario data
    # Data follows common scenario (WATERGAP SSP2) until 2020, then scenarios diverge; Note: ISIMIP stays constant for future
    watdem_nonagr[,years_harmonized,"consumption"] <- collapseNames(watdem_nonagr_WATERGAP_adjusted[,years_harmonized,"ssp2.consumption"])
    watdem_nonagr[,years_harmonized,"withdrawal"]  <- collapseNames(watdem_nonagr_WATERGAP_adjusted[,years_harmonized,"ssp2.withdrawal"])
    watdem_nonagr[,years_future,getNames(watdem_nonagr_WATERGAP)] <- watdem_nonagr_WATERGAP_adjusted[,years_future,getNames(watdem_nonagr_WATERGAP)]

    # Correct mismatches of withdrawal and consumption (withdrawals > consumption)
    watdem_nonagr[,,"withdrawal"]  <- pmax(watdem_nonagr[,,"withdrawal"], watdem_nonagr[,,"consumption"])
    watdem_nonagr[,,"consumption"] <- pmax(watdem_nonagr[,,"consumption"], 0.01*watdem_nonagr[,,"withdrawal"])

    # Correct cellordering and naming
    map           <- toolGetMappingCoord2Country()
    watdem_nonagr <- watdem_nonagr[map$coords,,]
    getCells(watdem_nonagr)              <- paste(map$coords, map$iso, sep=".")
    getSets(watdem_nonagr, fulldim=F)[1] <- "x.y.iso"

  }

  ###########################################
  ############ Function Output  #############
  ###########################################
  if (all(selectyears!="all")) {
    years         <- sort(findset(selectyears, noset="original"))
    watdem_nonagr <- watdem_nonagr[,years,]
  }

  ### Non-agricultural water demands in Growing Period
  if (seasonality=="grper") {
    ### Note: Seasonality "grper" will be deleted when we switch to new

    # Get growing days per month
    grow_days <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = climatetype, yield_ratio = 0.1, aggregate = FALSE)

    # Growing days per year
    grow_days <- dimSums(grow_days,dim=3)

    # Adjust years
    years_watdem <- getYears(watdem_nonagr)
    years_grper  <- getYears(grow_days)
    if (length(years_watdem) >= length(years_grper)) {
      years <- years_grper
    } else {
      years <- years_watdem
    }
    rm(years_grper, years_watdem)

    # Calculate non-agricultural water demand in growing period
    out         <- watdem_nonagr[,years,] * grow_days[,years,] / 365
    description <- "Non-agricultural water demand (industry, electiricty, domestic) in growing period"
  } else if (seasonality=="total") {
    ### Total non-agricultural water demands per year
    out         <- watdem_nonagr[,,]
    description <- "Total non-agricultural water demand (industry, electiricty, domestic)"
  } else {
    stop("Specify seasonality! grper or total")
  }

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA watdem_nonagr")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
