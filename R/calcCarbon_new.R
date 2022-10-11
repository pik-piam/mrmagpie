#' @title calcCarbon_new
#' @description This function extracts carbon densities from LPJ to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#' @param fromFlows TRUE, if calculated from harmonized flows
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("Carbon", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass add_dimension

calcCarbon_new <- function(lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop = "ggcmi_phase3_nchecks_9ca735cb"),
                           climatetype = "GSWP3-W5E5:historical", fromFlows = FALSE) {

  .getLPJmLCPools <- function(pool, cfg) {
    out <- calcOutput("LPJmL_new", version = cfg$lpjml,
                      climatetype = cfg$climatetype,
                      subtype = pool, stage = "harmonized2020",
                      aggregate = FALSE)
    out <- toolCoord2Isocell(out)
    out <- setNames(out, pool)
    return(out)
  }

  cfg    <- list(lpjml = lpjml["natveg"], climatetype = climatetype)
  natveg <- mbind(.getLPJmLCPools("vegc", cfg),
                  .getLPJmLCPools("soilc", cfg),
                  .getLPJmLCPools("litc", cfg))


  cfg    <- list(lpjml = lpjml["crop"], climatetype = climatetype)
  grass  <- mbind(.getLPJmLCPools("vegc_grass", cfg),
                  .getLPJmLCPools("soilc_grass", cfg),
                  .getLPJmLCPools("litc_grass", cfg))

  getNames(grass) <- getNames(natveg)

  if (fromFlows) {

    .getCPoolsFromFlows <- function(pool, flow, refYear) {
      # calculate carbon pools from carbon flows
      years <- getYears(pool, as.integer = TRUE)
      out   <- pool
      for (y in years[years > refYear]) out[, y, ] <- setYears(pool[, y - 1, ], y) + flow[, y, ]
      out   <- toolConditionalReplace(out, "<0", 0)
      return(out)
    }

    flowNatveg <- calcOutput("CarbonSink", version = lpjml["natveg"], climatetype = climatetype,
                              stage = "harmonized2020", aggregate = FALSE)
    natveg     <- .getCPoolsFromFlows(natveg, flowNatveg, 1995)

    flowGrass  <- calcOutput("CarbonSink", version = lpjml["crop"], climatetype = climatetype,
                             stage = "harmonized2020", pool = "all_grass", aggregate = FALSE)
    getNames(flowGrass) <- getNames(flowNatveg)
    grass      <- .getCPoolsFromFlows(grass, flowGrass, 1995)
  }

  topsoilc       <- calcOutput("TopsoilCarbon_new", lpjml = lpjml, climatetype = climatetype,
                               fromFlows = fromFlows, aggregate = FALSE)
  cshare         <- calcOutput("SOCLossShare", aggregate = FALSE, years = "y1995")

  ####################################################
  #Create the output object
  ####################################################

  carbonStocks <- new.magpie(cells_and_regions = getCells(natveg),
                              years = getYears(natveg),
                              names = getNames(natveg))

  carbonStocks <- add_dimension(carbonStocks, dim = 3.1, add = "landtype",
                                nm = c("crop", "past", "forestry", "primforest", "secdforest", "urban", "other"))

  ####################################################
  #Calculate the appropriate values for all land types and carbon types.
  ####################################################

  #Factor 0.012 is based on the script subversion/svn/tools/carbon_cropland, executed at 30.07.2013
  carbonStocks[, , "crop.vegc"]       <- 0.012 * natveg[, , "vegc"]
  carbonStocks[, , "crop.litc"]       <- 0 # does not make sense
  carbonStocks[, , "crop.soilc"]      <- cshare * topsoilc + (natveg[, , "soilc"] - topsoilc)

  carbonStocks[, , "past"]            <- grass
  carbonStocks[, , "past.soilc"]      <- natveg[, , "soilc"]

  carbonStocks[, , "forestry"]        <- natveg
  carbonStocks[, , "primforest"]      <- natveg
  carbonStocks[, , "secdforest"]      <- natveg
  carbonStocks[, , "urban"]           <- 0
  carbonStocks[, , "other"]           <- natveg #or grass?

  # Check for NAs
  if (any(is.na(carbonStocks))) {
    stop("produced NA Carbon")
  }

  weight <- calcOutput("CellArea", aggregate=FALSE)

  return(list(
    x            = carbonStocks,
    weight       = weight,
    unit         = "t per ha",
    description  = "Carbon in tons per hectar for different land use types.",
    note         = "Pasture soil carbon stocks are based on natveg run.",
    isocountries = FALSE))
}
