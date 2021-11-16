#' @title calcWaterUseNonAg
#' @description This function extracts non-agricultural water demand
#'
#' @param selectyears        Years to be returned
#' @param datasource         Data source to be used (e.g. WATERGAP2020)
#' @param lpjml              Defines LPJmL version for crop/grass and natveg specific inputs
#' @param seasonality        grper (default): non-agricultural water demand in growing period per year;
#'                           total: non-agricultural water demand throughout the year
#' @param climatetype        Switch between different climate scenarios for calcGrowingPeriod
#' @param harmon_base_time   Type of time smoothing applied before harmonization of WATERGAP data:
#'                           average (average over 8-year time span around baseline year) or
#'                           smoothing (time smoothing of baseline and WATERGAP scenario data) or
#'                           NULL (no smoothing before harmonization)
#' @param usetype            "total" returns the sum over different
#'                           water use types (manufacturing, industry, electricity)
#' @param cells              Number of cells to be returned
#'                           (select "magpiecell" for 59199 cells or "lpjcell" for 67420 cells)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterUseNonAg", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput readSource toolTimeAverage toolTimeSpline toolFillYears
#' @importFrom magclass new.magpie getYears getCells getSets setYears dimOrder
#' @importFrom mrcommons toolCell2isoCell toolCoord2Isocell toolGetMappingCoord2Country
#' @importFrom magpiesets addLocation findset

calcWaterUseNonAg <- function(selectyears = seq(1995, 2100, by = 5), cells = "magpiecell",
                              datasource = "WATCH_ISIMIP_WATERGAP", usetype = "all",
                              seasonality = "grper", harmon_base_time = "average",
                              lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"),
                              climatetype = "GSWP3-W5E5:historical") {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  # Cell mapping
  map         <- toolGetMappingCoord2Country()
  selectcells <- map$coords

  ### Old Non-Agricultural Waterdemand data (current default, will be deleted soon): ###
  if (datasource == "WATCH_ISIMIP_WATERGAP") {

    # Read in nonagricultural water demand:
    watdemNonAg <- readSource("WATERGAP", convert = "onlycorrect", subtype = datasource)
    # iso cell names
    watdemNonAg <- toolCell2isoCell(watdemNonAg)
    # Add year 2100
    watdemNonAg <- toolFillYears(watdemNonAg, seq(getYears(watdemNonAg, as.integer = TRUE)[1], 2100, by = 5))

  } else if (datasource == "ISIMIP") {

    # ISIMIP non-agricultural water use data (multi-model mean from H08, WaterGAP and PCR-GLOBWB)
    # Since this data is only available until 2050, the values should are kept constant from 2050 onwards.
    # Industry: electricity & domestic
    watdemISIMIPhist   <- readSource("ISIMIPinputs", subtype = "ISIMIP3b:water:histsoc.waterabstraction", convert = "onlycorrect")
    watdemISIMIPfuture <- readSource("ISIMIPinputs", subtype = "ISIMIP3b:water:2015soc.waterabstraction", convert = "onlycorrect")

    # Subset data to 67420 cells
    watdemISIMIPhist   <- watdemISIMIPhist[selectcells, , ]
    watdemISIMIPfuture <- watdemISIMIPfuture[selectcells, , ]

    # Combine historical and future ISIMIP data:
    watdemISIMIP <- mbind(watdemISIMIPhist, watdemISIMIPfuture)

    # Ref_year: 2010 because both ISIMIP historical (available until 2014) and WATERGAP (available from 2005)
    baseyear  <- "y2010"
    yearsHist <- getYears(watdemISIMIPhist)

    if (!is.null(harmon_base_time) && harmon_base_time == "smoothing") {
      # Time-Smoothing of historical baseline and projected WATERGAP data
      watdemISIMIP   <- toolSmooth(watdemISIMIP)
    }
    if (!is.null(harmon_base_time) && harmon_base_time == "average") {
      # average around baseyear of ISIMIP baseline
      watdemISIMIP[, yearsHist, ] <- toolTimeAverage(watdemISIMIP, averaging_range = 8)[, baseyear, ]
    }

    # Reduce size (cut historical years)
    watdemNonAg <- watdemISIMIP[, setdiff(yearsHist, paste0("y", c(1901:1964))), ]

  } else if (datasource == "WATERGAP2020") {

    # Read in WATERGAP non-agricultural water abstractions:
    watdemWATERGAP <- readSource("WATERGAP", subtype = "WATERGAP2020", convert = "onlycorrect")
    watdemWATERGAP <- watdemWATERGAP[selectcells, , ]
    # Read in ISIMIP non-agricultural water abstractions:
    watdemISIMIP   <- calcOutput("WaterUseNonAg", datasource = "ISIMIP",
                                  selectyears = "all", seasonality = "total", usetype = "all",
                                  harmon_base_time = harmon_base_time, lpjml = lpjml, climatetype = climatetype,
                                  aggregate = FALSE)

    ### Harmonize WATERGAP and ISIMIP data (WATERGAP trends scaled to ISIMIP historical data)
    # Ref_year: 2010 because both ISIMIP historical (available until 2014) and WATERGAP (available from 2005)
    baseyear        <- "y2010"
    yearsHarmonized <- paste0("y", seq(2010, 2020))
    yearsWATERGAP   <- getYears(watdemWATERGAP)
    yearsHist       <- setdiff(getYears(watdemISIMIP), getYears(watdemWATERGAP))

    if (!is.null(harmon_base_time) && harmon_base_time == "smoothing") {
      # Time-Smoothing of projected WATERGAP data
      watdemWATERGAP <- toolSmooth(watdemWATERGAP)
    }

    # Note: ISIMIP industry data = manufacturing + electricity
    # Store WATERGAP share of manufacturing and electricity of industry
    watdemIndustry   <- collapseNames(watdemWATERGAP[, , "manufacturing"]) +
                        collapseNames(watdemWATERGAP[, , "electricity"])
    shrManufacturing <- ifelse(watdemIndustry > 0,
                               watdemWATERGAP[, , "manufacturing"] / watdemIndustry,
                               0)
    shrManufacturing <- dimOrder(shrManufacturing, perm = c(1, 3, 2), dim = 3)
    shrManufacturing <- setNames(shrManufacturing, gsub("manufacturing", "industry", getNames(shrManufacturing)))
    shrElectricity   <- ifelse(watdemIndustry > 0,
                               watdemWATERGAP[, , "electricity"] / watdemIndustry,
                               0)
    shrElectricity   <- dimOrder(shrElectricity, perm = c(1, 3, 2), dim = 3)

    # historical data provided by ISIMIP (same for all scenarios)
    tmp <- vector(mode = "list", length = 3)
    tmp[[1]] <- watdemISIMIP[, , "domestic"]
    tmp[[2]] <- collapseNames(watdemISIMIP[, , "industry"]) * collapseNames(setYears(shrManufacturing[, baseyear, "ssp2"], NULL))
    getNames(tmp[[2]]) <- paste("industry", getNames(tmp[[2]]), sep = ".")
    tmp[[3]] <- collapseNames(watdemISIMIP[, , "industry"]) * collapseNames(setYears(shrElectricity[, baseyear, "ssp2"], NULL))
    getNames(tmp[[3]]) <- paste("electricity", getNames(tmp[[3]]), sep = ".")

    watdemNonAg <- mbind(tmp)
    watdemNonAg <- add_dimension(watdemNonAg, dim = 3.1, add = "scenario", nm = "ISIMIP")

    # scenario ISIMIP data (split up by water use)
    watdemNonAg[, yearsWATERGAP, ][, , "domestic"]    <- watdemISIMIP[, yearsWATERGAP, ][, , "domestic"]
    watdemNonAg[, yearsWATERGAP, ][, , "industry"]    <- collapseNames(watdemISIMIP[, yearsWATERGAP, ][, , "industry"]) * collapseNames(shrManufacturing[, , "ssp2"])
    watdemNonAg[, yearsWATERGAP, ][, , "electricity"] <- collapseNames(watdemISIMIP[, yearsWATERGAP, ][, , "industry"]) * collapseNames(shrElectricity[, , "ssp2"])


    tmpWATERGAP <- new.magpie(cells_and_regions = getCells(watdemWATERGAP),
                              years = yearsWATERGAP,
                              names = getNames(watdemISIMIP))
    getSets(tmpWATERGAP) <- c("x", "y", "year", "use", "type")

    scenarios  <- getNames(watdemWATERGAP, dim = "scenario")
    listMAgPIE <- vector(mode = "list", length = length(scenarios))
    i <- 0
    for (scenario in scenarios) {

      # WATERGAP data
      tmpWATERGAP[, , "industry"] <- watdemIndustry[, , scenario]
      tmpWATERGAP[, , "domestic"] <- collapseNames(watdemWATERGAP[, , "domestic"][, , scenario])

      # Harmonize WATERGAP data to ISIMIP baseline
      harmonizedWATERGAP <- toolHarmonize2Baseline(x = tmpWATERGAP, base = watdemISIMIP,
                                                   ref_year = baseyear, limited = TRUE, hard_cut = FALSE)
      harmonizedWATERGAP <- setNames(harmonizedWATERGAP, nm = paste(scenario, getNames(harmonizedWATERGAP), sep = "."))
      getSets(harmonizedWATERGAP) <- c("x", "y", "year", "scenario", "use", "type")

      # Store harmonized data in final object
      tmp <- vector(mode = "list", length = 3)

      tmp[[1]] <- harmonizedWATERGAP[, yearsWATERGAP, "domestic"]
      tmp[[2]] <- shrManufacturing[, , scenario] * collapseNames(harmonizedWATERGAP[, yearsWATERGAP, "industry"])
      tmp[[3]] <- shrElectricity[, , scenario] * collapseNames(harmonizedWATERGAP[, yearsWATERGAP, "industry"])

      # Store MAgPIE objects in list
      i <- i + 1
      listMAgPIE[[i]] <- mbind(tmp)

    }

    # Combine to one object
    watdemWATERGAP <- mbind(listMAgPIE)
    rm(listMAgPIE, tmp, tmpWATERGAP, harmonizedWATERGAP, watdemIndustry, watdemISIMIP)

    # Data follows common scenario (WATERGAP SSP2) until 2020, then scenarios diverge;
    # Note: ISIMIP stays constant for future
    watdemWATERGAP[, yearsHarmonized, ] <- collapseNames(watdemWATERGAP[, yearsHarmonized, "ssp2"])

    # Reduce number of years (for memory reasons)
    if (all(selectyears != "all")) {

      if (is.numeric(selectyears)) {
        selectyears <- paste0("y", selectyears)
      }

      yearsHist     <- intersect(yearsHist, selectyears)
      yearsWATERGAP <- intersect(yearsWATERGAP, selectyears)

      if (length(yearsWATERGAP) != 0) {
        watdemWATERGAP <- watdemWATERGAP[, yearsWATERGAP, ]

        if (length(yearsHist) != 0) {
        watdemNonAg    <- watdemNonAg[, c(yearsHist, yearsWATERGAP), ]
        }
      }
    }

    if (length(yearsHist) != 0) {
      # historical data provided by ISIMIP (same for all scenarios)
      watdemWATERGAP <- add_columns(watdemWATERGAP, dim = 2, addnm = yearsHist, fill = NA)
      watdemWATERGAP <- watdemWATERGAP[, sort(as.numeric(gsub("y", "", getYears(watdemWATERGAP)))), ]

      watdemWATERGAP[, yearsHist, "domestic"]    <- collapseNames(watdemNonAg[, yearsHist, "domestic"])
      watdemWATERGAP[, yearsHist, "industry"]    <- collapseNames(watdemNonAg[, yearsHist, "industry"]) * collapseNames(setYears(shrManufacturing[, baseyear, "ssp2"], NULL))
      watdemWATERGAP[, yearsHist, "electricity"] <- collapseNames(watdemNonAg[, yearsHist, "industry"]) * collapseNames(setYears(shrElectricity[, baseyear, "ssp2"], NULL))
    }
    rm(shrElectricity, shrManufacturing)

    # Merge to final object
    watdemNonAg <- mbind(watdemNonAg, watdemWATERGAP)
    rm(watdemWATERGAP)

    # Correct mismatches of withdrawal and consumption (withdrawals > consumption)
    watdemNonAg[, , "withdrawal"]  <- pmax(watdemNonAg[, , "withdrawal"], watdemNonAg[, , "consumption"])
    watdemNonAg[, , "consumption"] <- pmax(watdemNonAg[, , "consumption"], 0.01 * watdemNonAg[, , "withdrawal"])

  }

  ###########################################
  ############ Function Output  #############
  ###########################################

  if (datasource != "WATCH_ISIMIP_WATERGAP") {
    # Number of cells to be returned
    if (cells == "magpiecell") {

      watdemNonAg <- toolCoord2Isocell(watdemNonAg)

    } else if (cells == "lpjcell") {

      # Correct cell naming
      getCells(watdemNonAg)                    <- paste(map$coords, map$iso, sep = ".")
      getSets(watdemNonAg, fulldim = FALSE)[1] <- "x.y.iso"

    }
  }

  ### Non-agricultural water demands in Growing Period
  if (seasonality == "grper") {
    ### Note: Seasonality "grper" will be deleted when we switch to new mrwater preprocessing

    # Get growing days per month
    growDays <- calcOutput("GrowingPeriod", aggregate = FALSE,
                           lpjml = lpjml, climatetype = climatetype, yield_ratio = 0.1)

    # Growing days per year
    growDays <- dimSums(growDays, dim = 3)

    # Adjust years
    yearsWatdem <- getYears(watdemNonAg)
    yearsGrper  <- getYears(growDays)
    if (length(yearsWatdem) >= length(yearsGrper)) {
      years <- yearsGrper
    } else {
      years <- yearsWatdem
    }
    rm(yearsGrper, yearsWatdem)

    # Calculate non-agricultural water demand in growing period
    out         <- watdemNonAg[, years, ] * growDays[, years, ] / 365
    description <- "Non-agricultural water demand (industry, electiricty, domestic) in growing period"

  } else if (seasonality == "total") {

    ### Total non-agricultural water demands per year
    out         <- watdemNonAg
    description <- "Total non-agricultural water demand (industry, electiricty, domestic)"

  } else {
    stop("Specify seasonality! grper or total")
  }

  # Sum up over all non-agricultural water uses (domestic, industry, manufacturing)
  if (usetype == "total") {
    out <- dimSums(out, dim = "use")
  }

  # Check for NAs
  if (any(is.na(out))) {
    stop("produced NA watdemNonAg")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
