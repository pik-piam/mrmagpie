#' @title calcEFRRockstroem
#' @description This function calculates environmental flow requirements (EFR) for MAgPIE
#'              retrieved from LPJmL monthly discharge and water availability
#'              following the definition of the planetary boundary in Rockström et al. 2023
#'
#' @param lpjml       Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param stage       Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param seasonality grper (default): EFR in growing period per year;
#'                    total: EFR throughout the year;
#'                    monthly: monthly EFRs
#'
#' @import magclass
#' @import madrat
#' @importFrom stats quantile
#' @importFrom mstools toolHarmonize2Baseline
#' @importFrom mrlandcore toolLPJmLVersion
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("EFRRockstroem", aggregate = FALSE)
#' }
#'
calcEFRRockstroem <- function(lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                        crop = "ggcmi_phase3_nchecks_9ca735cb"),
                              climatetype = "GSWP3-W5E5:historical", stage = "harmonized2020",
                              seasonality = "grper") {
  # Create settings for LPJmL from version and climatetype argument
  cfgNatveg <- toolLPJmLVersion(version = lpjml["natveg"], climatetype = climatetype)
  cfgCrop   <- toolLPJmLVersion(version = lpjml["crop"],   climatetype = climatetype)

  lpjmlReadin  <- c(natveg = unname(cfgNatveg$readin_version),
                    crop   = unname(cfgCrop$readin_version))

  lpjmlBaseline <- c(natveg = unname(cfgNatveg$baseline_version),
                     crop   = unname(cfgCrop$baseline_version))

  #############################################################################
  # Definition of planetary boundary (PB) according to Rockström et al. 2023: #
  # less than 20% magnitude monthly surface flow alteration                   #
  # in all grid cells                                                         #
  # Translation to EFR:                                                       #
  # only 20% of monthly water flow can be withdrawn,                          #
  # i.e. 80% need to stay in the river in each grid cell                      #
  #############################################################################

  if (stage %in% c("raw", "smoothed")) {
    # Available water per month (smoothed)
    avlWaterMonth <- calcOutput("AvlWater", lpjml = lpjmlReadin, climatetype = climatetype,
                                seasonality = "monthly", stage = "smoothed",
                                aggregate = FALSE, cells = "lpjcell")

    # Monthly EFR: 80% of monthly available water
    efr <- 0.8 * avlWaterMonth

    ### aggregation to grper, total
    ### efr per cell per month
    if (seasonality == "monthly") {
      # Check for NAs
      if (any(is.na(efr))) {
        stop("calcEFRRockstroem produced NA monthly EFR")
      }
      out <- efr

      ### Total water available per cell per year
    } else if (seasonality == "total") {
      # Sum up over all month:
      efrTotal <- dimSums(efr, dim = 3)

      # Read in available water (for Smakthin calculation)
      avlWaterTotal <- calcOutput("AvlWater", lpjml = lpjmlReadin, climatetype = climatetype,
                                  seasonality = "total", stage = "smoothed",
                                  aggregate = FALSE, cells = "lpjcell")

      # Reduce EFR to 80% of available water where it exceeds this threshold
      efrTotal[which(efrTotal / avlWaterTotal > 0.8)] <-
        0.8 * avlWaterTotal[which(efrTotal / avlWaterTotal > 0.8)]

      # Check for NAs
      if (any(is.na(efrTotal))) {
        stop("calcEFRRockstroem produced NA efrTotal")
      }
      out <- efrTotal

      ### Water available in growing period per cell per year
    } else if (seasonality == "grper") {
      # magpie object with days per month with same dimension as EFR
      tmp <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      monthDays <- new.magpie(names = dimnames(efr)[[3]])
      monthDays[, , ] <- tmp
      monthDayMagpie <- as.magpie(efr)
      monthDayMagpie[, , ] <- 1
      monthDayMagpie <- monthDayMagpie * monthDays

      # Daily environmental flow requirements
      efrDay   <- efr / monthDayMagpie

      # Growing days per month
      growDays <- calcOutput("GrowingPeriod", lpjml = lpjmlReadin, climatetype = climatetype,
                             stage = "smoothed", yield_ratio = 0.1,
                             aggregate = FALSE, cells = "lpjcell")

      # Available water in growing period
      efrGrper <- efrDay * growDays
      # Available water in growing period per year
      efrGrper <- dimSums(efrGrper, dim = 3)
      # Read in available water (for Smakthin calculation)
      avlWaterGrper <- calcOutput("AvlWater", lpjml = lpjmlReadin, climatetype = climatetype,
                                  seasonality = "grper", stage = "smoothed",
                                  aggregate = FALSE, cells = "lpjcell")

      # Reduce EFR to 80% of available water where it exceeds this threshold
      efrGrper[which(efrGrper / avlWaterGrper > 0.8)] <-
        0.8 * avlWaterGrper[which(efrGrper / avlWaterGrper > 0.8)]

      # Check for NAs
      if (any(is.na(efrGrper))) {
        stop("calcEFRRockstroem produced NA efrGrper")
      }
      out <- efrGrper
    } else {
      stop("Specify seasonality: monthly, grper or total")
    }

  } else if (stage == "harmonized") {
    # Load baseline and climate EFR:
    baseline <- calcOutput("EFRRockstroem", lpjml = lpjmlBaseline, climatetype = cfgNatveg$baseline_hist,
                           seasonality = seasonality, stage = "smoothed",
                           aggregate = FALSE)

    if (climatetype == cfgNatveg$baseline_hist) {

      out <- baseline

    } else {

      x   <- calcOutput("EFRRockstroem", lpjml = lpjmlReadin, climatetype = climatetype,
                        seasonality = seasonality, stage = "smoothed",
                        aggregate = FALSE)
      # Harmonize to baseline
      out <- toolHarmonize2Baseline(x = x, base = baseline, ref_year = cfgNatveg$ref_year_hist)
    }

  } else if (stage == "harmonized2020") {

    baseline2020 <- calcOutput("EFRRockstroem", lpjml = lpjmlBaseline, climatetype = cfgNatveg$baseline_gcm,
                               seasonality = seasonality, stage = "harmonized",
                               aggregate = FALSE)

    if (climatetype == cfgNatveg$baseline_gcm) {

      out <- baseline2020

    } else {

      x        <- calcOutput("EFRRockstroem", lpjml = lpjmlReadin, climatetype = climatetype,
                             seasonality = seasonality, stage = "smoothed",
                             aggregate = FALSE)
      out      <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfgNatveg$ref_year_gcm)
    }

  } else {
    stop("Stage argument not supported!")
  }

  description <- paste0("EFRs according to water planetary boundary in ", seasonality)

  return(list(x = out,
              weight = NULL,
              unit = "mio. m^3",
              description = description,
              isocountries = FALSE))
}
