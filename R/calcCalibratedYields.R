#' @title calcCalibratedYields
#' @description This functions calibrates extracted yields from LPJmL to FAO levels on country level
#'
#' @param source Defines LPJmL version for main crop inputs and isimip replacement.
#'               For isimip choose crop model/gcm/rcp/co2 combination formatted like this: "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b"
#' @param climatetype Switch between different climate scenarios
#' @param refYear reference year for calibration
#' @return magpie object in cellular resolution from reference year on
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("CalibratedYields", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass getYears add_columns dimSums time_interpolate
#' @importFrom madrat toolFillYears
#' @importFrom mrcommons toolLPJmLVersion

calcCalibratedYields <- function(source = c(lpjml = "ggcmi_phase3_nchecks_9ca735cb", isimip = NULL),
                                 climatetype = "GSWP3-W5E5:historical", refYear = "y1995") {

    sizelimit <- getOption("magclass_sizeLimit")
    options(magclass_sizeLimit = 1e+12)
    on.exit(options(magclass_sizeLimit = sizelimit))

    crops           <- setdiff(findset("kcr"), c("betr", "begr"))

    yieldFAO_iso    <- calcOutput("FAOYield", cut = 0.98, aggregate = FALSE)[, refYear, crops]
    yieldLPJmL_grid <- calcOutput("Yields", source = source, climatetype = climatetype,
                                  aggregate = FALSE, supplementary = TRUE)

    years           <- getYears(yieldLPJmL_grid$x, as.integer = TRUE)
    years           <- years[years >= as.integer(gsub("y", "", refYear))]
    otherYields     <- yieldLPJmL_grid$x[, years, setdiff(getNames(yieldLPJmL_grid$x, dim = 1), crops)]
    weight          <- yieldLPJmL_grid$weight
    yieldLPJmL_grid <- yieldLPJmL_grid$x[, years, crops]

    areaMAG_grid    <- calcOutput("Croparea", sectoral = "kcr", physical = TRUE, cellular = TRUE,
                                  irrigation = TRUE, aggregate = FALSE)[, refYear, crops]
    CountryToCell   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
    areaMAG_iso     <- toolAggregate(dimSums(areaMAG_grid, dim = 3.1), rel = CountryToCell,
                                     from = "celliso", to = "iso", dim = 1)
    cropMAG_grid    <- dimSums(areaMAG_grid, dim = 3.2)

    yieldLPJmL_iso  <- toolAggregate(dimSums(yieldLPJmL_grid[, refYear, ] * areaMAG_grid, dim = 3.2),
                                     rel = CountryToCell, from = "celliso", to = "iso", dim = 1) / areaMAG_iso

    yieldLPJmL_iso[areaMAG_iso == 0] <- (toolAggregate(dimSums(yieldLPJmL_grid[, refYear, ] * cropMAG_grid, dim = 3.2),
                                                       rel = CountryToCell, from = "celliso", to = "iso", dim = 1) /
                                           dimSums(areaMAG_iso, dim = 3))[areaMAG_iso == 0]

    yieldLPJmL_iso <- toolConditionalReplace(yieldLPJmL_iso, "is.na()", 0)
    yieldLPJmL_iso <- toolIso2CellCountries(yieldLPJmL_iso)
    yieldFAO_iso   <- toolIso2CellCountries(yieldFAO_iso)

    out <- toolPatternScaling(yieldLPJmL_grid, yieldLPJmL_iso, yieldFAO_iso, ref_year = refYear)
    out <- mbind(out, otherYields)

  return(list(x            = out,
              weight       = weight,
              unit         = "t DM per ha physical area",
              description  = "Calibrated crop yields by plant type and irrigation",
              isocountries = FALSE)
  )
}
