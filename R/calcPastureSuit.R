#' @title calcPastureSuit
#' @description Calculate glassland suitable for pasture management based on population and aridity criteria.
#' @param subtype Select version, climate model and period.
#' @param smooth_precipitation Smooth precipitation climate data over time
#' @param smooth_out smooth the Pasture suitability areas variations over time
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' calcOutput("PastureSuit")
#' }
#' @importFrom raster area rasterFromXYZ

calcPastureSuit <- function(subtype = "ISIMIP3b:IPSL-CM6A-LR:1850-2100", smooth_precipitation = 10, smooth_out = 10) {
  x <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL, period = NULL))

  # pasture drivers
  population <- calcOutput("GridPop_new", subtype = "all", cellular = TRUE, FiveYear = TRUE, harmonize_until = 2015, aggregate = F)

  precipitation <- list()
  scenarios <- c("ssp126", "ssp245", "ssp370", "ssp460", "ssp585") # Current ISIMIP3bv2 scenarios
  for (scenario in scenarios) {
    subtype <- paste(x$version, x$climatemodel, scenario, x$period, "pr", sep = ":")
    precipitation[[scenario]] <- setNames(calcOutput("GCMClimate_new", subtype = subtype, smooth = smooth_precipitation, aggregate = F), scenario)
  }
  precipitation <- collapseNames(mbind(precipitation))

  evapotranspiration <- calcOutput("Evapotranspiration", subtype = "H08:mri-esm2-0", aggregate = F)

  # temporary mapping of evapotranspiration RCP scenarios unavailable in ISIMIP3bv2
  evapotranspiration <- add_columns(evapotranspiration, addnm = "ssp245", dim = 3.1, fill = NA)
  evapotranspiration[, , "ssp245"] <- evapotranspiration[, , "ssp370"]
  evapotranspiration <- add_columns(evapotranspiration, addnm = "ssp460", dim = 3.1, fill = NA)
  evapotranspiration[, , "ssp460"] <- evapotranspiration[, , "ssp370"]

  evapotranspiration <- evapotranspiration[, , getItems(precipitation, dim = 3)]

  # matching available ssps scenarios
  regex <- paste0("[", paste0("+", strtrim(getItems(evapotranspiration, dim = 3), 4), collapse = "|"), "]", "{4}$")
  avl_ssps <- grep(regex, getNames(population), ignore.case = T)
  population <- population[, , avl_ssps]

  # Cell area calculation
  landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
  landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
  landcoords <- raster::rasterFromXYZ(landcoords)
  crs(landcoords) <- "+proj=longlat" # outputs cell are in km2
  cell_size <- raster::area(landcoords)
  cell_size <- cell_size * landcoords
  cell_size <- as.magpie(cell_size)
  cell_size <- toolOrderCells(collapseDim(addLocation(cell_size), dim = c("x", "y")))

  # population density
  pop_density <- (population * 1e6) / cell_size # population density in number of people per km2
  pop_density[is.infinite(pop_density)] <- 0
  pop_density[is.nan(pop_density)] <- 0


  years_com <- intersect(getYears(pop_density), getYears(precipitation))

  # Aridity (the real aridity is measured as the ratio between evapotranspiration and precipitarion (I have complete this calculation))
  aridity <- precipitation[, years_com, ] / (evapotranspiration[, years_com, ])
  aridity[is.infinite(aridity) | is.nan(aridity)] <- 0
  # 0.5 aridity threshold for managed pastures. Same from HYDE 3.2.
  aridity[aridity < 0.5] <- 0
  aridity[aridity >= 0.5] <- 1

  # pasture suitability check
  pasture_suit <- aridity
  pop_density <- pop_density[, getYears(pasture_suit), ]
  pasture_suit[pop_density < 5] <- 0 # 5 hab km2 population threshold for managed pastures. Same from HYDE 3.2.

  pasture_suit_area <- (pasture_suit * cell_size * 100) / 1e6 # (from km2 (x100) to mha (/1e6))
  pasture_suit_area <- collapseDim(pasture_suit_area)
  pasture_suit_area <- toolHoldConstantBeyondEnd(pasture_suit_area)

  # calibration to historical values

  hist_pastr <- calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE)[, , "pastr"]
  past_all <- intersect(getYears(hist_pastr), getYears(pasture_suit_area))

  past_ly <- findset("past")
  past_ly <- past_ly[length(past_ly)] # past last year
  future <- setdiff(getYears(pasture_suit_area), past_all)

  # map <- toolGetMapping("clustermapping.csv", type = "regional")
  map <- getConfig("regionmapping")
  pasture_suit_area_reg <- toolAggregate(pasture_suit_area, rel = map, from = "cell", to = "region")
  hist_pastr_reg <- toolAggregate(hist_pastr, rel = map, from = "cell", to = "region")
  corr_reg <- hist_pastr_reg[, past_ly, ] / pasture_suit_area_reg[, past_ly, ]
  pasture_suit_area[, future, ] <- toolAggregate(corr_reg, rel = map, from = "region", to = "cell") * pasture_suit_area[, future, ]

  pasture_suit_area[is.infinite(pasture_suit_area) | is.nan(pasture_suit_area)] <- 0
  pasture_suit_area[pasture_suit_area < 0] <- 0
  pasture_suit_area[, past_all, ] <- hist_pastr[, past_all, ]

  if (smooth_out > 1) {
    pasture_suit_area <- toolTimeAverage(pasture_suit_area, averaging_range = smooth_out)
  }

  pasture_suit_area[, intersect(past_all, getYears(pasture_suit_area)), ] <- hist_pastr[, intersect(past_all, getYears(pasture_suit_area)), ]
  pasture_suit_area <- toolHoldConstant(pasture_suit_area, findset("time"))
  pasture_suit_area <- collapseNames(pasture_suit_area)
  pasture_suit_area[, past_all, ] <- hist_pastr[, past_all, ]

  return(list(
    x = pasture_suit_area,
    weight = NULL,
    unit = "Mha",
    description = "Area suitable for pasture management in mha",
    isocountries = FALSE
  ))
}