#' @title calcCollectEnvironmentData_new
#' @description Calculate climate, CO2 and soil environmental conditions on cellular level
#' @param subtype Switch between different climate scenarios (default: "CRU_4")
#'                eg. "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100"
#' @param sar Average range for smoothing annual variations
#' @param sel_feat features names to be included in the output file
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("CollectEnvironmentData_new", subtype, sar = 20, sel_feat = "temp")
#' }
#'
#' @import madrat
#' @import magclass
#' @import mstools
#' @importFrom raster rasterFromXYZ
#' @importFrom raster area
#' @importFrom raster "crs<-"
#' @importFrom magpiesets findset
#'

calcCollectEnvironmentData_new <- function(subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 20, sel_feat = c( # nolint
  "tas",
  "pr",
  "lwnet",
  "rsds",
  "CO2",
  "Ks",
  "Sf",
  "w_pwp",
  "w_fc",
  "w_sat",
  "hsg",
  "wet"
)) {
  ##### CONFIG ######
  climateVariables <- c("tas", "pr", "lwnet", "rsds", "wet")
  fullSimulationPeriod <- "1850-2100"
  ##### CONFIG ######

  x <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL, scenario = NULL, period = NULL))
  years <- as.numeric(unlist(strsplit(x$period, "_|-")))
  syear <- years[1]
  fyear <- years[2]
  sar   <- 0 # test mush be erased after

  # read in the individual climate variables, then smooth the dataset with
  # toolAverage and extend the standardize the number of years)
  gcmVariables <- list()
  for (climateVariable in climateVariables) {
    gcmVariables[[climateVariable]] <- calcOutput("GCMClimate", aggregate = FALSE,
                                                  subtype = paste(x$version, x$climatemodel, x$scenario,
                                                                  fullSimulationPeriod, climateVariable,
                                                                  "annual_mean", sep = ":"))
    gcmVariables[[climateVariable]] <- toolHoldConstant(gcmVariables[[climateVariable]],
                                                        seq((max(getYears(gcmVariables[[climateVariable]],
                                                                          as.integer = TRUE)) + 1), 2150, 5))
  }
  variables <- mbind(gcmVariables)
  co2 <- calcOutput("CO2Atmosphere_new", cells = "lpjcell", aggregate = FALSE,
                    subtype = paste(x$version, x$scenario, sep = ":"),
                    co2Evolution = "rising")[, (syear - sar / 2):fyear, ]
  co2 <- toolHoldConstant(co2,  seq((max(getYears(co2, as.integer = TRUE)) + 1), 2150, 5))
  soil <- calcOutput("SoilCharacteristics", aggregate = FALSE)[, getYears(co2), ]          ### To Do (Alex K., Marcos, Kristine, Feli): adjust to 67k 

  constants <- mbind(co2, soil)
  constants <- constants[, getYears(variables), ]

  env <- mbind(variables, constants)
  features <- paste0(sel_feat, collapse = "+|")
  select <- grepl(pattern = features, getItems(env, dim = 3), ignore.case = TRUE)
  env <- env[, , select]

  # Calculate weight: cellarea
  mapping <- toolGetMappingCoord2Country()
  cb <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                       type = "cell", where = "mrcommons")
  cellArea <- (111e3 * 0.5) * (111e3 * 0.5) * cos(cb$lat / 180 * pi)
  cellArea <- as.magpie(cellArea, spatial = 1)
  getItems(cellArea, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")
  getSets(cellArea) <- c("x", "y", "iso", "year", "data")

  return(list(
    x = env,
    weight = cellArea,
    unit =
      "temperature: Degree Celcius,
    precipitation: mm3 per year,
    longwave_radiation: watt per m2,
    shortwave_radiation: watt per m2,
    wetdays: day,
    Ks: mm/h, Sf: mm ,
    w_pwp: % ,
    w_fc: % ,
    w_sat: % ,
    tdiff0: mm^2/s ,
    tdiff15: tmm^2/s ,
    tdiff100: mm^2/s ,
    cond_pwp:W/m^2/K) ,
    cond_100: W/m^2/K) ,
    cond_100_ice: W/m^2/K) ,
    wet: number of wet days",
    description = "Climate, CO2 and soil characteristics on cellular level",
    isocountries = FALSE
  ))
}
