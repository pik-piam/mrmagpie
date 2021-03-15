#' @title calcCollectEnvironmentData
#' @description Calculate climate, CO2 and soil environmental conditions on cellular level
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param sar Average range for smoothing annual variations
#' @param sel_feat features names to be included in the output file
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("CollectEnvironmentData", climatetype = "HadGEM:rcp8p5:co2", sar = 20, sel_feat = "temp")
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

calcCollectEnvironmentData <- function(climatetype = "HadGEM2_ES:rcp8p5:co2", sar = 20, sel_feat = c(
                                         "lsu_ha",
                                         "temperature",
                                         "precipitation",
                                         "longwave_radiation",
                                         "shortwave_radiation",
                                         "wetdays",
                                         "CO2ATMconcentration",
                                         "Ks",
                                         "Sf",
                                         "w_pwp",
                                         "w_fc",
                                         "w_sat",
                                         "hsg",
                                         "soilc"
                                       )) {

  # Calculating weights
  landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
  landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
  landcoords <- raster::rasterFromXYZ(landcoords)
  crs(landcoords) <- "+proj=longlat"
  cell_size <- raster::area(landcoords)
  weight <- cell_size * landcoords
  weight <- as.magpie(weight)
  weight <- toolOrderCells(collapseDim(addLocation(weight), dim = c("x", "y")))

  type <- strsplit(climatetype, split = "\\:")
  GCMModel <- unlist(type)[1]
  rcp <- unlist(type)[2]
  co2 <- c(co2 = "rising", noco2 = "static")
  co2 <- co2[unlist(type)[3]]

  climate_variables <- c("temperature", "precipitation", "longwave_radiation", "shortwave_radiation", "wetdays")

  # read in the individual climate variables, then smooth the dataset with toolAverage and extend the standardize the number of years)
  GCMVariables <- list()
  for (climate_variable in climate_variables) {
    GCMVariables[[climate_variable]] <- calcOutput("GCMClimate", aggregate = F, subtype = paste0(GCMModel, ":", rcp, ".", climate_variable))[, 1951:2099, ]
    GCMVariables[[climate_variable]] <- toolTimeAverage(GCMVariables[[climate_variable]], averaging_range = sar)
    GCMVariables[[climate_variable]] <- toolHoldConstant(GCMVariables[[climate_variable]], (max(getYears(GCMVariables[[climate_variable]], as.integer = TRUE)) + 1):2150)
  }
  variables <- mbind(GCMVariables)
  co2 <- calcOutput("CO2Atmosphere", aggregate = F, rcp = rcp, co2_evolution = co2)
  soil <- calcOutput("SoilCharacteristics", aggregate = F)

  constants <- mbind(co2, soil)
  constants <- constants[, getYears(variables), ]

  env <- mbind(variables, constants)
  features <- paste0(sel_feat, collapse = "+|")
  select <- grepl(pattern = features, getItems(env,dim = 3), ignore.case = TRUE)
  env <- env[,,select]


  return(list(
    x = env,
    weight = weight,
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
    cond_100_ice: W/m^2/K)",
    description = "Climate, CO2 and soil characteristics on cellular level",
    isocountries = FALSE
  ))
}
