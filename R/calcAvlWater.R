#' @title calcAvlWater
#' @description This function calculates water availability for MAgPIE retrieved from LPJmL
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param seasonality grper (default): water available in growing period per year;
#'                    total: total water available throughout the year;
#'                    monthly: monthly water availability (for further processing, e.g. in calcEnvmtlFlow)
#'
#' @import magclass
#' @import madrat
#' @importFrom mrcommons toolHarmonize2Baseline toolLPJmLVersion
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Kristine Karstens, Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' calcOutput("AvlWater", aggregate = FALSE)
#' }
#'
calcAvlWater <- function(lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"),
                         climatetype = "GSWP3-W5E5:historical", stage = "harmonized2020", seasonality = "grper") {

  cfg <- toolLPJmLVersion(version = lpjml["natveg"], climatetype = climatetype)

  ######################################################
  ############ Water availability per cell #############
  # Runoff is distributed across the river basin cells #
  # based on discharge-weighted algorithm              #
  ######################################################
  if (stage %in% c("raw", "smoothed")) {
    ### Monthly Discharge (unit (after calcLPJmL): mio. m^3/month)
    monthDischargeMAG <- toolCoord2Isocell(calcOutput("LPJmL_new", subtype = "mdischarge",
                                                      stage = "raw",
                                                      version = lpjml["natveg"], climatetype = climatetype,
                                                      aggregate = FALSE))
    # Transform to array (faster calculation)
    monthDischargeMAG <- as.array(collapseNames(monthDischargeMAG))

    ### Monthly Runoff (unit (after calcLPJmL): mio. m^3/month)
    monthRunoffMAG    <- toolCoord2Isocell(calcOutput("LPJmL_new", subtype = "mrunoff",
                                                      stage = "raw",
                                                      version = lpjml["natveg"], climatetype = climatetype,
                                                      aggregate = FALSE))
    # Transform to array (faster calculation)
    monthRunoffMAG    <- as.array(collapseNames(monthRunoffMAG))

    ### Calculate available water per month (monthAvlWat)
    # Empty array
    monthAvlWat     <- monthRunoffMAG
    monthAvlWat[, , ] <- NA

    ## River basin water allocation algorithm:
    # River basin information
    basinCode <- toolGetMapping("rivermapping.csv", type = "cell")
    basinCode <- basinCode$basincode

    # Sum the runoff in all basins and allocate it to the basin cells with discharge as weight
    for (basin in unique(basinCode)) {
      basinCells     <- which(basinCode == basin)
      basinRunoff    <- colSums(monthRunoffMAG[basinCells, , , drop = FALSE])
      basinDischarge <- colSums(monthDischargeMAG[basinCells, , , drop = FALSE])
      for (month in dimnames(monthAvlWat)[[3]]) {
        monthAvlWat[basinCells, , month] <- t(basinRunoff[, month] *
                                                t(monthDischargeMAG[basinCells, , month]) / basinDischarge[, month])
      }
    }
    # Remove no longer needed objects
    rm(basinDischarge, basinRunoff)

    # monthAvlWat contain NA's wherever basinDischarge was 0 -> Replace NA's by 0
    monthAvlWat[is.nan(monthAvlWat)] <- 0
    monthAvlWat <- as.magpie(monthAvlWat)

    if (stage == "smoothed") {
      monthAvlWat <- toolSmooth(monthAvlWat)
    }

    #######################
    ##### Aggregation #####
    #######################
    ### Available water per cell per month
    if (seasonality == "monthly") {
      # Check for NAs
      if (any(is.na(monthAvlWat))) {
        stop("produced NA water availability")
      }
      out <- monthAvlWat

      ### Total water available per cell per year
    } else if (seasonality == "total") {
      # Sum up over all month:
      totalAvlWat <- dimSums(monthAvlWat, dim = 3)
      # Check for NAs
      if (any(is.na(totalAvlWat))) {
        stop("produced NA water availability")
      }
      out <- totalAvlWat

      ### Water available in growing period per cell per year
    } else if (seasonality == "grper") {
      # magpie object with days per month with same dimension as monthAvlWat
      tmp <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      monthDAYS       <- new.magpie(names = dimnames(monthAvlWat)[[3]])
      monthDAYS[, , ] <- tmp
      monthDayMAG     <- as.magpie(monthAvlWat)
      monthDayMAG[, , ] <- 1
      monthDayMAG       <- monthDayMAG * monthDAYS

      # Daily water availability
      dailyAvlWat <- monthAvlWat / monthDayMAG

      # Growing days per month
      growDAYS <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = climatetype,
                              stage = stage, yield_ratio = 0.1, aggregate = FALSE)

      # Adjust years
      yearsWAT <- getYears(dailyAvlWat)
      yearsGRPER  <- getYears(growDAYS)
      if (length(yearsWAT) >= length(yearsGRPER)) {
        years <- yearsGRPER
      } else {
        years <- yearsWAT
      }
      rm(yearsGRPER, yearsWAT)

      # Available water in growing period per month
      grperAvlWat <- dailyAvlWat[, years, ] * growDAYS[, years, ]
      # Available water in growing period per year
      grperAvlWat <- dimSums(grperAvlWat, dim = 3)

      # Check for NAs
      if (any(is.na(grperAvlWat))) {
        stop("produced NA water availability")
      }
      out <- grperAvlWat
    } else {
      stop("Please specify seasonality: monthly, total or grper")
    }

  } else if (stage == "harmonized") {
    # load smoothed data
    baseline <- calcOutput("AvlWater", lpjml = lpjml, climatetype = cfg$baseline_hist, seasonality = seasonality,
                           aggregate = FALSE, stage = "smoothed")

    if (climatetype == cfg$baseline_hist) {

      out <- baseline

    } else {

      x   <- calcOutput("AvlWater", lpjml = lpjml, climatetype = climatetype, seasonality = seasonality,
                        aggregate = FALSE, stage = "smoothed")
      # Harmonize to baseline
      out <- toolHarmonize2Baseline(x = x, base = baseline, ref_year = cfg$ref_year_hist)
    }

  } else if (stage == "harmonized2020") {
    # read in historical data for subtype
    baseline2020 <- calcOutput("AvlWater", lpjml = lpjml, climatetype = cfg$baseline_gcm, seasonality = seasonality,
                               aggregate = FALSE, stage = "harmonized")

    if (climatetype == cfg$baseline_gcm) {
      out <- baseline2020

    } else {

      x   <- calcOutput("AvlWater", stage = "smoothed",
                        lpjml = lpjml, climatetype = climatetype,
                        seasonality = seasonality,
                        aggregate = FALSE)
      out <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$ref_year_gcm)
    }

  } else {
    stop("Stage argument not supported!")
  }

  description <- paste0("Available water in ", seasonality)

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = description,
              isocountries = FALSE))
}
