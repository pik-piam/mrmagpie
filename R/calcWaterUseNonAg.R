#' @title calcWaterUseNonAg
#' @description This function extracts non-agricultural water demand
#'
#' @param selectyears years to be returned
#' @param source      data source to be used (e.g. WATERGAP2020)
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param lpjml              Defines LPJmL version for crop/grass and natveg specific inputs
#' @param seasonality        grper (default): non-agricultural water demand in growing period per year; total: non-agricultural water demand throughout the year
#' @param climatetype        switch between different climate scenarios (default: "CRU_4") for calcGrowingPeriod
#' @param finalcells  number of cells to be returned by this function magpiecell (59199) or lpjcell (67420)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("WaterUseNonAg", aggregate=FALSE) }
#'
#' @importFrom madrat calcOutput readSource toolTimeAverage toolTimeSpline toolFillYears
#' @importFrom magclass getYears getCells
#' @importFrom mrcommons toolCell2isoCell toolCoord2Isocell
#' @importFrom magpiesets addLocation

calcWaterUseNonAg <- function(selectyears="all", source="WATCH_ISIMIP_WATERGAP", finalcells="magpiecell",
                              time="raw", averaging_range=NULL, dof=NULL, seasonality="grper",
                              lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"),
                              climatetype="GSWP3-W5E5:historical") {

  #######################################
  ############ Calculations  ############
  #######################################

  # Old Non-Agricultural Waterdemand data (current default, will be deleted soon):
  if (source=="WATCH_ISIMIP_WATERGAP") {

    if (finalcells=="lpjcell") {
      stop("lpjcell argument not supported for old water data. Please select magpiecell in finalcells argument instead.")
    }

    # Read in nonagricultural water demand:
    watdem_nonagr <- readSource("WATERGAP", convert="onlycorrect", subtype=source)
    # iso cell names
    watdem_nonagr <- toolCell2isoCell(watdem_nonagr)
    # Add year 2100
    watdem_nonagr <- toolFillYears(watdem_nonagr, seq(getYears(watdem_nonagr, as.integer=TRUE)[1],2100,by=5))
  }

  # New Non-Agricultural Waterdemand data (will be new default)
  if (source=="WATERGAP2020") {

    if (time=="raw") {
      # Read in nonagricultural water demand:
      watdem_nonagr_WATERGAP      <- readSource("WATERGAP", convert="onlycorrect", subtype="WATERGAP2020")
      watdem_nonagr_ISIMIP_hist   <- readSource("ISIMIPinputs",subtype="ISIMIP3b:water:histsoc.waterabstraction",convert="onlycorrect")
      watdem_nonagr_ISIMIP_future <- readSource("ISIMIPinputs",subtype="ISIMIP3b:water:2015soc.waterabstraction",convert="onlycorrect")

      watdem_nonagr_ISIMIP_hist   <- watdem_nonagr_ISIMIP_hist[getCells(watdem_nonagr_WATERGAP),,]
      watdem_nonagr_ISIMIP_future <- watdem_nonagr_ISIMIP_future[getCells(watdem_nonagr_WATERGAP),,]

      ### Combine datasets from different sources:
      # historical and future ISIMIP data:
      watdem_ISIMIP <- mbind(watdem_nonagr_ISIMIP_hist, watdem_nonagr_ISIMIP_future)

      # empty magpie object
      cells <- getCells(watdem_nonagr_WATERGAP)
      years <- getYears(watdem_ISIMIP)
      names <- c(getNames(watdem_nonagr_WATERGAP),paste0("ISIMIP.",getNames(watdem_ISIMIP)))
      watdem_nonagr <- new.magpie(cells,years,names)

      # historical and future ISIMIP data
      watdem_nonagr[,getYears(watdem_ISIMIP),paste0("ISIMIP.",getNames(watdem_ISIMIP))] <- watdem_ISIMIP[,getYears(watdem_ISIMIP),getNames(watdem_ISIMIP)]

      # historical data provided by ISIMIP (same for all scenarios)
      watdem_nonagr[,getYears(watdem_nonagr_ISIMIP_hist),] <- watdem_nonagr_ISIMIP_hist[,getYears(watdem_nonagr_ISIMIP_hist),]

      # future WATERGAP scenarios (adjusted to transition year of historical data)
      watdem_nonagr_WATERGAP_adjusted     <- watdem_nonagr_WATERGAP
      watdem_nonagr_WATERGAP_adjusted[,,] <- NA

      # Calibration of WATERGAP data to ISIMIP baseline:
      # historical ISIMIP data in correct format for harmonization
      cells <- getCells(watdem_nonagr_WATERGAP)
      years <- getYears(watdem_nonagr_ISIMIP_hist)
      names <- getNames(watdem_nonagr_WATERGAP)
      tmp     <- new.magpie(cells,years,names)
      tmp[,,] <- 1
      tmp     <- tmp * watdem_nonagr_ISIMIP_hist

      # Harmonization
      watdem_nonagr_WATERGAP_adjusted <- toolHarmonize2Baseline(x=watdem_nonagr_WATERGAP, base=tmp, ref_year="y2005", limited=TRUE, hard_cut=FALSE)

      # WATERGAP adjusted future scenario data
      watdem_nonagr[,getYears(watdem_nonagr_WATERGAP),getNames(watdem_nonagr_WATERGAP)] <- watdem_nonagr_WATERGAP_adjusted[,getYears(watdem_nonagr_WATERGAP),getNames(watdem_nonagr_WATERGAP)]

    } else {
      # Time smoothing:
      x                <- calcOutput("WaterUseNonAg", selectyears="all", source=source, seasonality=seasonality,
                                     climatetype=climatetype, lpjml=lpjml, time="raw", averaging_range=NULL,
                                     dof=NULL, finalcells=finalcells, aggregate=FALSE)

      if (time=="average") {
        # Smoothing data through average:
        watdem_nonagr   <- toolTimeAverage(x, averaging_range=averaging_range)

      } else if (time=="spline") {
        # Smoothing data with spline method:
        watdem_nonagr   <- toolTimeSpline(x, dof=dof)
        # Replace value in 2100 with value from 2099
        if ("y2099" %in% getYears(watdem_nonagr)) {
          watdem_nonagr <- toolFillYears(watdem_nonagr, c(getYears(watdem_nonagr, as.integer=TRUE)[1]:2100))
        }

      } else if(time!="raw") {
        stop("Time argument not supported!")
      }
    }

    # Correct mismatches of withdrawal and consumption (withdrawals > consumption)
    watdem_nonagr[,,"withdrawal"]  <- pmax(watdem_nonagr[,,"withdrawal"], watdem_nonagr[,,"consumption"])
    watdem_nonagr[,,"consumption"] <- pmax(watdem_nonagr[,,"consumption"], 0.01*watdem_nonagr[,,"withdrawal"])

    ### Number of cells to be returned
    if (finalcells=="lpjcell") {
      # sort cells correctly and rename
      watdem_nonagr <- watdem_nonagr
    } else if (finalcells=="magpiecell") {
      getSets(watdem_nonagr, fulldim = FALSE)[1] <- "x.y"
      watdem_nonagr <- toolCoord2Isocell(watdem_nonagr)
    } else {
      stop("Cell argument for finalcells not supported. Select lpjcell for 67420 cells or magpiecell for 59199 cells")
    }
  }

  ###########################################
  ############ Function Output  #############
  ###########################################
  if (selectyears!="all") {
    years         <- sort(findset(selectyears, noset="original"))
    watdem_nonagr <- watdem_nonagr[,years,]
  }

  ### Non-agricultural water demands in Growing Period
  if (seasonality=="grper") {
    ### Note: Seasonality "grper" will be deleted when we switch to new
    ### water inputs (mrwater)
    if (finalcells=="lpjcell") {
      stop("grper not supported as seasonality argument for lpjcells. Please select seasonality total or select magpiecell in finalcells argument")
    }

    # Get growing days per month
    grow_days <- calcOutput("GrowingPeriod", lpjml=lpjml, climatetype=climatetype, stage="harmonized2020", aggregate=FALSE)

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
    out         <- watdem_nonagr[,years,]*grow_days[,years,]/365
    description <- "Non-agricultural water demand (industry, electiricty, domestic) in growing period"
  } else if (seasonality=="total") {
    ### Total non-agricultural water demands per year
    out         <- watdem_nonagr[,,]
    description <- "Total non-agricultural water demand (industry, electiricty, domestic)"
  } else {
    stop("Specify seasonality! grper or total")
  }

  # Check for NAs
  if(any(is.na(out))){
    stop("produced NA watdem_nonagr")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
