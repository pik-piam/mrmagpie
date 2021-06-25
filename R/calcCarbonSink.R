#' @title calcCarbonSink
#' @description This function calculates carbon sink per year and smooths the data
#'              and calculates carbon from it for MAgPIE again (to avoid jumps in carbon sink estimates)
#'
#' @param version Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param pool Switch to select specific pool, or return all three pools (vegc, litc, soilc).
#'             (Here also soilc_layer might be selected.)
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("Carbon", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass add_dimension

calcCarbonSink <- function(version     = "LPJmL4_for_MAgPIE_44ac93de",
                           climatetype = "GSWP3-W5E5:historical",
                           stage       = "harmonized2020",
                           pool        = "all") {

  # Create settings for LPJmL from version and climatetype argument
  cfgLPJmL <- toolLPJmLVersion(version = version, climatetype = climatetype)

  if (stage %in% c("raw", "smoothed")) {

    .getLPJmLCFlows <- function(pool, cfg) {
      out <- calcOutput("LPJmL_new", version = cfg$lpjml,
                        climatetype = cfg$climatetype,
                        subtype = pool, stage = "raw",
                        aggregate = FALSE)
      if (is.null(getNames(out))) out <- setNames(out, pool)
      out   <- toolCoord2Isocell(out)
      years <- getYears(out, as.integer = TRUE)
      out   <- out[, years[-1], ] - setYears(out[, years[-length(years)], ], years[-1])
      return(out)
    }

    # Load raw data on carbon stocks and
    # calculate flows from stocks (carbon sink per year from carbon densities)
    cfg         <- list(lpjml = version, climatetype = climatetype)

    if (pool == "all") {

      out <- mbind(.getLPJmLCFlows("vegc", cfg),
                   .getLPJmLCFlows("soilc", cfg),
                   .getLPJmLCFlows("litc", cfg))

    } else if (pool == "all_grass") {

      out <- mbind(.getLPJmLCFlows("vegc_grass", cfg),
                   .getLPJmLCFlows("soilc_grass", cfg),
                   .getLPJmLCFlows("litc_grass", cfg))
    } else {

      out <- .getLPJmLCFlows(pool, cfg)
    }

    if (stage == "smoothed") out <- toolSmooth(out)

  } else if (stage == "harmonized") {

    #read in historical data for subtype
    baseline <- calcOutput("CarbonSink", version = version, climatetype = cfgLPJmL$baseline_hist,
                           stage = "smoothed", pool = pool, aggregate = FALSE)

    if (climatetype == cfgLPJmL$baseline_hist) {

      out    <- baseline

    } else {

      x      <- calcOutput("CarbonSink", version = version, climatetype = climatetype,
                           stage = "smoothed", pool = pool, aggregate = FALSE)
      out    <- toolHarmonize2Baseline(x, baseline, ref_year = cfgLPJmL$ref_year_hist)
    }

  } else if (stage == "harmonized2020") {

    #read in historical data for subtype
    baseline2020    <- calcOutput("CarbonSink", version = version, climatetype = cfgLPJmL$baseline_gcm,
                                  stage = "harmonized", pool = pool, aggregate = FALSE)

    if (climatetype == cfgLPJmL$baseline_gcm) {
      out <- baseline2020

    } else {

      x      <- calcOutput("CarbonSink", version = version, climatetype = climatetype,
                           stage = "smoothed", pool = pool, aggregate = FALSE)
      out    <- toolHarmonize2Baseline(x, baseline2020, ref_year = cfgLPJmL$ref_year_gcm)
    }

  } else {

    stop("Stage argument not supported!")
  }

  return(list(
    x            = out,
    weight       = NULL,
    unit         = "tC per ha per year",
    description  = "Carbon sink in tons per hectar per year for natural vegetation.",
    isocountries = FALSE))
}
