#' @title calcEnvmtlFlow
#' @description This function calculates environmental flow requirements (EFR) for MAgPIE retrieved from LPJmL monthly discharge and water availability
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param LFR_val Strictness of environmental flow requirements
#' @param HFR_LFR_less10 High flow requirements (share of total water for cells) with LFR<10percent of total water
#' @param HFR_LFR_10_20 High flow requirements (share of total water for cells) with 10percent < LFR < 20percent of total water
#' @param HFR_LFR_20_30 High flow requirements (share of total water for cells) with 20percent < LFR < 30percent of total water
#' @param HFR_LFR_more30 High flow requirements (share of total water for cells) with LFR>30percent of total water
#' @param seasonality grper (default): EFR in growing period per year; total: EFR throughout the year; monthly: monthly EFRs
#'
#' @import magclass
#' @import madrat
#' @importFrom stats quantile
#' @importFrom mrcommons toolHarmonize2Baseline toolLPJmLVersion
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Abhijeet Mishra
#'
#' @examples
#' \dontrun{
#' calcOutput("EnvmtlFlow", aggregate = FALSE)
#' }
#'
calcEnvmtlFlow <- function(lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"),
                           climatetype = "GSWP3-W5E5:historical", stage = "harmonized2020",
                           LFR_val = 0.1, HFR_LFR_less10 = 0.2, HFR_LFR_10_20 = 0.15, HFR_LFR_20_30 = 0.07, HFR_LFR_more30 = 0.00,
                           seasonality = "grper") {

  # Create settings for LPJmL from version and climatetype argument
  cfg <- toolLPJmLVersion(version = lpjml["natveg"], climatetype = climatetype)

  if (stage %in% c("raw", "smoothed")) {

    ############################################################
    # Step 1 Determine monthly discharge low flow requirements #
    #        (LFR_monthly_discharge)                           #
    ############################################################

    ### Monthly Discharge
    monthly_discharge_magpie <- toolCoord2Isocell(calcOutput("LPJmL_new", version = lpjml["natveg"], climatetype = climatetype,
                                                             subtype = "mdischarge", aggregate = FALSE, stage = "raw"))
    # Extract years for quantile calculation
    years <- getYears(monthly_discharge_magpie, as.integer = TRUE)
    years <- seq(years[1] + 7, years[length(years)], by = 1)
    # Transform to array (faster calculation)
    monthly_discharge_magpie <-  as.array(collapseNames(monthly_discharge_magpie))
    # Empty array with magpie object names
    LFR_quant <- array(NA, dim = c(dim(monthly_discharge_magpie)[1], length(years)),
                       dimnames = list(dimnames(monthly_discharge_magpie)[[1]], paste("y", years, sep = "")))

    ### Calculate LFR_quant
    ## Note: LFRs correspond to the Q90-value (i.e. to the discharge that is exceeded in nine out of ten months)
    ## (Bonsch et al. 2015). This is calculated via the 10%-quantile of monthly discharge.

    # Quantile calculation: Yearly LFR quantile value
    for (year in years) {
      # get the LFR_val quantile in range of 8 years for each year for all cells
      needed_years <- seq(year - 7, year, by = 1)
      LFR_quant[, paste("y", year, sep = "")] <- apply(monthly_discharge_magpie[, paste("y", needed_years, sep = ""), ], MARGIN = c(1), quantile, probs = LFR_val)
    }
    # Time-smooth LFR_quant
    LFR_quant <- as.magpie(LFR_quant)
    LFR_quant <- toolFillYears(LFR_quant, getYears(monthly_discharge_magpie, as.integer = TRUE))

    if (stage == "smoothed") LFR_quant <- toolSmooth(LFR_quant)

    # Raw monthly discharge no longer needed at this point
    rm(monthly_discharge_magpie)

    ### Read in smoothed monthly discharge
    monthly_discharge_magpie <- toolCoord2Isocell(calcOutput("LPJmL_new", version = lpjml["natveg"], climatetype = climatetype,
                                                             subtype = "mdischarge", aggregate = FALSE, stage = "smoothed"))

    # Transform to array (faster calculation)
    LFR_quant <- as.array(collapseNames(LFR_quant))
    monthly_discharge_magpie <- as.array(collapseNames(monthly_discharge_magpie))

    ### Calculate LFR discharge values for each month
    # If LFR_quant < magpie_discharge: take LFR_quant
    # Else: take magpie_discharge
    LFR_monthly_discharge <- monthly_discharge_magpie
    for (month in 1:12) {
      tmp1 <- as.vector(LFR_quant)
      tmp2 <- as.vector(monthly_discharge_magpie[, , month])
      LFR_monthly_discharge[, , month] <- pmin(tmp1, tmp2)
    }
    # Remove no longer needed objects
    rm(LFR_quant)


    ################################################
    # Step 2 Determine low flow requirements (LFR) #
    #        from available water per month        #
    ################################################
    ### Available water per month (smoothed)
    avl_water_month <- calcOutput("AvlWater", lpjml = lpjml, climatetype = climatetype,
                                  seasonality = "monthly", aggregate = FALSE, stage = "smoothed")

    # Transform to array for faster calculation
    avl_water_month <- as.array(collapseNames(avl_water_month))

    # Empty array
    LFR     <- avl_water_month
    LFR[, , ] <- NA

    ### Calculate LFRs
    LFR <- avl_water_month * (LFR_monthly_discharge / monthly_discharge_magpie)
    # There are na's where monthly_discharge_magpie was 0, replace with 0:
    LFR[is.nan(LFR)] <- 0

    ###################################################################
    # Step 3 Determie monthly high flow requirements (HFR)            #
    #        based on the ratio between LFR_month and avl_water_month #
    ###################################################################
    ## Note: "For rivers with low Q90 values, high-flow events are important
    ## for river channel maintenance, wetland flooding, and riparian vegetation.
    ## HFRs of 20% of available water are therefore assigned to rivers with a
    ## low fraction of Q90 in total discharge. Rivers with a more stable flow
    ## regime receive a lower HFR." (Bonsch et al. 2015)
    HFR     <- LFR
    HFR[, , ] <- NA

    HFR[LFR < 0.1 * avl_water_month]  <- HFR_LFR_less10 * avl_water_month[LFR < 0.1 * avl_water_month]
    HFR[LFR >= 0.1 * avl_water_month] <- HFR_LFR_10_20  * avl_water_month[LFR >= 0.1 * avl_water_month]
    HFR[LFR >= 0.2 * avl_water_month] <- HFR_LFR_20_30  * avl_water_month[LFR >= 0.2 * avl_water_month]
    HFR[LFR >= 0.3 * avl_water_month] <- HFR_LFR_more30 * avl_water_month[LFR >= 0.3 * avl_water_month]
    HFR[avl_water_month <= 0]       <- 0

    EFR <- LFR + HFR
    EFR <- as.magpie(EFR)

    ### aggregation to grper, total
    ### EFR per cell per month
    if (seasonality == "monthly") {

      # Check for NAs
      if (any(is.na(EFR))) {
        stop("produced NA EFR")
      }
      out <- EFR

      ### Total water available per cell per year
    } else if (seasonality == "total") {

      # Sum up over all month:
      EFR_total <- dimSums(EFR, dim = 3)

      # Read in available water (for Smakthin calculation)
      avl_water_total <- calcOutput("AvlWater", lpjml = lpjml, climatetype = climatetype,
                                    seasonality = "total", aggregate = FALSE, stage = "smoothed")

      # Reduce EFR to 50% of available water where it exceeds this threshold (according to Smakhtin 2004)
      EFR_total[which(EFR_total / avl_water_total > 0.5)] <- 0.5 * avl_water_total[which(EFR_total / avl_water_total > 0.5)]

      # Check for NAs
      if (any(is.na(EFR_total))) {
        stop("produced NA EFR_total")
      }
      out <- EFR_total

      ### Water available in growing period per cell per year
    } else if (seasonality == "grper") {
      # magpie object with days per month with same dimension as EFR
      tmp <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      month_days <- new.magpie(names = dimnames(EFR)[[3]])
      month_days[, , ] <- tmp
      month_day_magpie <- as.magpie(EFR)
      month_day_magpie[, , ] <- 1
      month_day_magpie <- month_day_magpie * month_days

      # Daily water availability
      EFR_day   <- EFR / month_day_magpie

      # Growing days per month
      grow_days <- calcOutput("GrowingPeriod", lpjml = lpjml, climatetype = climatetype,
                              stage = "smoothed", yield_ratio = 0.1, aggregate = FALSE)

      # Available water in growing period
      EFR_grper <- EFR_day * grow_days
      # Available water in growing period per year
      EFR_grper <- dimSums(EFR_grper, dim = 3)
      # Read in available water (for Smakthin calculation)
      avl_water_grper <- calcOutput("AvlWater", lpjml = lpjml, climatetype = climatetype,
                                    seasonality = "grper", aggregate = FALSE, stage = "smoothed")

      # Reduce EFR to 50% of available water where it exceeds this threshold (according to Smakhtin 2004)
      EFR_grper[which(EFR_grper / avl_water_grper > 0.5)] <- 0.5 * avl_water_grper[which(EFR_grper / avl_water_grper > 0.5)]

      # Check for NAs
      if (any(is.na(EFR_grper))) {
        stop("produced NA EFR_grper")
      }
      out <- EFR_grper
    } else {
      stop("Specify seasonality: monthly, grper or total")
    }

  } else if (stage == "harmonized") {

    # Load baseline and climate EFR:
    baseline <- calcOutput("EnvmtlFlow", lpjml = lpjml, climatetype = cfg$baseline_hist,
                           seasonality = seasonality, aggregate = FALSE, stage = "smoothed")

    if (climatetype == cfg$baseline_hist) {

      out <- baseline

    } else {

      x   <- calcOutput("EnvmtlFlow", lpjml = lpjml, climatetype = climatetype,
                        seasonality = seasonality, aggregate = FALSE, stage = "smoothed")
      # Harmonize to baseline
      out <- toolHarmonize2Baseline(x = x, base = baseline, ref_year = cfg$ref_year_hist)
    }

  } else if (stage == "harmonized2020") {

    baseline2020 <- calcOutput("EnvmtlFlow", lpjml = lpjml, climatetype = cfg$baseline_gcm,
                               seasonality = seasonality, aggregate = FALSE, stage = "harmonized")

    if (climatetype == cfg$baseline_gcm) {

      out <- baseline2020

    } else {

      x        <- calcOutput("EnvmtlFlow", lpjml = lpjml, climatetype = climatetype,
                             seasonality = seasonality, aggregate = FALSE, stage = "smoothed")
      out      <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfg$ref_year_gcm)
    }

  } else {
 stop("Stage argument not supported!")
 }

  description <- paste0("EFR in ", seasonality)

  return(list(
    x = out,
    weight = NULL,
    unit = "mio. m^3",
    description = description,
    isocountries = FALSE))
}
