#' @title calcPastureSuit
#' @description Calculate glassland suitable for pasture management based on population and aridity criteria.
#' @param subtype Select version, climate model and period.
#' @param smoothPrecipitation Smooth precipitation climate data over time
#' @param smoothOut smooth the Pasture suitability areas variations over time
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' calcOutput("PastureSuit")
#' }
#' @importFrom raster area rasterFromXYZ

calcPastureSuit <- function(datasource = "ISIMIP3bv2",climatetype = "MRI-ESM2-0:ssp126", smoothPrecipitation = 10, smoothOut = 10) {
  x <- toolSplitSubtype(climatetype, list(climatemodel = NULL, scenario = NULL))
  period = "1850-2100"
  # Drivers of managed pastures
  subtype <- paste(datasource, x$climatemodel, x$scenario, period, "pr", "annual_mean", sep = ":")
  precipitation <- calcOutput("GCMClimate", subtype = subtype, smooth = smoothPrecipitation, aggregate = FALSE)
  population <- calcOutput("GridPop", subtype = "all", cellular = TRUE, FiveYear = TRUE,
                           harmonize_until = 2015, aggregate = FALSE)[,,toupper(substring(x$scenario,first = 0,last = 4))]
  evapotranspiration <- calcOutput("Evapotranspiration", subtype = "H08:mri-esm2-0", aggregate = FALSE)[,,x$scenario]
  # Need new function for evapt. Should be something like this. So it gets LPJMl evap for the same climate model and spp as the prec
  # evapotranspiration <- calcOutput("Evapotranspiration", datasource = "LPJmL"?, climatetype = climatetype, aggregate = FALSE)

  # Cell area calculation
  landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell", where = "mappingfolder"))
  landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
  landcoords <- raster::rasterFromXYZ(landcoords)
  crs(landcoords) <- "+proj=longlat" # outputs cell are in km2
  cellSize <- raster::area(landcoords)
  cellSize <- cellSize * landcoords
  cellSize <- as.magpie(cellSize)
  cellSize <- toolOrderCells(collapseDim(addLocation(cellSize), dim = c("x", "y")))

  # population density
  popDensity <- (population * 1e6) / cellSize # population density in number of people per km2
  popDensity[is.infinite(popDensity)] <- 0
  popDensity[is.nan(popDensity)] <- 0

  yearsCom <- intersect(getYears(popDensity), getYears(precipitation))

  aridity <- precipitation[, yearsCom, ] / evapotranspiration[, yearsCom, ]
  aridity[is.infinite(aridity) | is.nan(aridity)] <- 0
  # 0.5 aridity threshold for managed pastures. Same from HYDE 3.2.
  aridity[aridity < 0.5] <- 0
  aridity[aridity >= 0.5] <- 1

  # pasture suitability check
  pastureSuit <- aridity
  popDensity <- popDensity[, getYears(pastureSuit), ]
  pastureSuit[popDensity < 5] <- 0 # 5 hab km2 population threshold for managed pastures. Same from HYDE 3.2.

  pastureSuitArea <- (pastureSuit * cellSize * 100) / 1e6 # (from km2 (x100) to mha (/1e6))
  pastureSuitArea <- collapseDim(pastureSuitArea)
  pastureSuitArea <- toolHoldConstantBeyondEnd(pastureSuitArea)

  # calibration to historical values

  histPastr <- setNames(calcOutput("LanduseInitialisation", cellular = TRUE,
                                   nclasses = "nine", aggregate = FALSE)[, , c("past")], "pastr")
  pastAll <- intersect(getYears(histPastr), getYears(pastureSuitArea))

  pastLy <- findset("past")
  pastLy <- pastLy[length(pastLy)] # past last year
  future <- setdiff(getYears(pastureSuitArea), pastAll)

  map <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  pastureSuitAreaReg <- toolAggregate(pastureSuitArea, rel = map, from = "celliso", to = "iso")
  histPastrReg <- toolAggregate(histPastr, rel = map, from = "celliso", to = "iso")
  calibReg <- histPastrReg[, pastLy, ] / pastureSuitAreaReg[, pastLy, ]
  calibReg[is.infinite(calibReg)] <- 1
  calibReg[is.nan(calibReg)] <- 0
  pastureSuitArea[, future, ] <- toolAggregate(calibReg, rel = map, from = "iso", to = "celliso") * pastureSuitArea[, future, ]

  pastureSuitArea[is.infinite(pastureSuitArea) | is.nan(pastureSuitArea) | is.na(pastureSuitArea)] <- 0
  pastureSuitArea[pastureSuitArea < 0] <- 0
  pastureSuitArea[, pastAll, ] <- histPastr[, pastAll, ]

  if (smoothOut > 1) {
    pastureSuitArea <- toolTimeAverage(pastureSuitArea, averaging_range = smoothOut)
  }
  y <- intersect(pastAll, getYears(pastureSuitArea))
  pastureSuitArea[, y, ] <- histPastr[, intersect(pastAll, getYears(pastureSuitArea)), ]
  pastureSuitArea <- toolHoldConstant(pastureSuitArea, findset("time"))
  pastureSuitArea <- collapseNames(pastureSuitArea)
  pastureSuitArea[, pastAll, ] <- histPastr[, pastAll, ]

  return(list(
    x = pastureSuitArea,
    weight = NULL,
    unit = "Mha",
    description = "Area suitable for pasture management in mha",
    isocountries = FALSE
  ))
}


    pet    <- calcOutput("LPJmL_new", version = "LPJmL4_for_MAgPIE_44ac93de",
                         climatetype = "MRI-ESM2-0:ssp370", subtype = "mpet",
                         stage = "smoothed", aggregate = FALSE)
    prec   <- calcOutput("LPJmLClimateInput", lpjmlVersion = "LPJmL4_for_MAgPIE_44ac93de",
                         climatetype  = "GSWP3-W5E5:historical",
                         variable = "precipitation:monthlySum",
                         stage = "smoothed", aggregate = FALSE)