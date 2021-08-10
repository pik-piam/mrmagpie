#' @title calcPastureSuit
#' @description Calculate glassland suitable for pasture management based on population and aridity criteria.
#' @param subtype Select version, climate model and period.
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves
#' @examples
#'
#' \dontrun{
#' calcOutput("PastureSuit")
#' }
#' @importFrom raster area rasterFromXYZ

calcPastureSuit <- function(subtype = "ISIMIP3b:IPSL-CM6A-LR:1850-2100"){
  x <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL, period = NULL))

  # pasture drivers
  population <- calcOutput("GridPop_new", subtype="all", cellular=TRUE,FiveYear=TRUE, harmonize_until=2015, aggregate=F)

  precipitation <- list()
  scenarios <- c("ssp126","ssp370","ssp585") # Current ISIMIP3b scenarios
  for (scenario in scenarios) {
    # write a script that reads different SPPs scenarios
    subtype = paste(x$version, x$climatemodel,scenario,x$period, "pr", sep = ":")
    # subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1850-2100:pr"
    precipitation[[scenario]] <- setNames(calcOutput("GCMClimate_new", subtype = subtype, aggregate = F),paste0("past_suit.",scenario))
  }
  precipitation <- mbind(precipitation)

  #matching available ssps scenarios
  regex <- paste0("+",strtrim(scenarios, 4), "+", collapse = "|")
  avl_ssps <- grep(regex, getNames(population), ignore.case = T)
  population <- population[,,avl_ssps]


  # Cell area calculation
  landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
  landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
  landcoords <- raster::rasterFromXYZ(landcoords)
  crs(landcoords) <- "+proj=longlat"
  cell_size <- raster::area(landcoords)
  cell_size <- cell_size * landcoords
  cell_size <- as.magpie(cell_size)
  cell_size <- toolOrderCells(collapseDim(addLocation(cell_size), dim = c("x", "y")))

  # population density
  pop_density <- population*1e6/cell_size
  pop_density[is.infinite(pop_density)] <- 0
  pop_density[is.nan(pop_density)] <- 0
       # 5 hab km2 population threshold for managed pastures. Same from HYDE 3.2.
  # pop_density[pop_density<5] <- 0
  # pop_density[pop_density>=5] <- 1

  # Aridity (the real aridity is measured as the ratio between evapotranspiration and precipitarion (I have complete this calculation))
  aridity <- precipitation[, intersect(getYears(pop_density), getYears(precipitation)),]
  aridity[aridity<2] = 0
  aridity[aridity>=2] = 1

  # pasture suitability check
  pasture_suit <- aridity
  pop_density <- pop_density[,getYears(pasture_suit),]
  pasture_suit[pop_density<5] <- 0
  pasture_suit_area = pasture_suit * cell_size/1e6*100
  pasture_suit_area <- collapseDim(pasture_suit_area, dim = 3.3)
  pasture_suit_area <- toolHoldConstantBeyondEnd(pasture_suit_area)

  # calibration to historical values

  hist_pastr <- calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE)[,,"pastr"]
  past_all <- intersect(getYears(hist_pastr), getYears(pasture_suit_area))

  past_ly <- findset("past")
  past_ly <- past_ly[length(past_ly)] #past last year
  future <- setdiff(getYears(pasture_suit_area),past_all)
  # pasture_suit_area[,future,] <- hist_pastr[,past_ly,] - pasture_suit_area[,past_ly,] + pasture_suit_area[,future,]
  # pasture_suit_area[,future,] <- (hist_pastr[,past_ly,] / pasture_suit_area[,past_ly,]+ 1e-6) * (pasture_suit_area[,future,] + 1e-6)
  pasture_suit_area[,future,] <- (dimSums(hist_pastr[,past_ly,], dim =1) / dimSums(pasture_suit_area[,past_ly,], dim = 1)) *  pasture_suit_area[,future,]
  pasture_suit_area[is.infinite(pasture_suit_area) | is.nan(pasture_suit_area)] <- 0
  pasture_suit_area[pasture_suit_area < 0] <- 0
  pasture_suit_area[,past_all,] <- hist_pastr[,past_all,]

  pasture_suit_area <- toolTimeAverage(pasture_suit_area, averaging_range = 10 , annual = F)
  print(past_all)
  print(getYears(pasture_suit_area))
  print(intersect(past_all, getYears(pasture_suit_area)))
  pasture_suit_area[,intersect(past_all, getYears(pasture_suit_area)),] <- hist_pastr[,intersect(past_all, getYears(pasture_suit_area)),]
  pasture_suit_area <- toolHoldConstant(pasture_suit_area, findset("time"))
  pasture_suit_area <- collapseNames(pasture_suit_area)

  return(list(
    x = pasture_suit_area,
    weight = NULL,
    unit = "Mha",
    description = "Area suitable for pasture management in mha",
    isocountries = FALSE
  ))
}

