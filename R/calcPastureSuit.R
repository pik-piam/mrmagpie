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
  ssps <- c("ssp126","ssp230","ssp340","ssp460","ssp585")
  for (ssp in ssps) {
    #write a script that reads different SPPs scenarios
    # subtype = paste(x$version, x$climatemodel,ssps,x$period, "pr", sep = ":")
    subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1850-2100:pr"
    precipitation[[ssp]] <- setNames(calcOutput("GCMClimate_new", subtype = subtype, aggregate = F),paste0("past_suit.",ssp))
  }
  precipitation <- mbind(precipitation)

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
  aridity <- precipitation[,getYears(pop_density),]
  aridity[aridity<1.3] = 0
  aridity[aridity>=1.3] = 1

  #pasture suitability check
  pasture_suit <- aridity
  pasture_suit[pop_density<5] <- 0
  pasture_suit_area = pasture_suit * cell_size/1e6*100
  pasture_suit_area <- collapseDim(pasture_suit_area, dim = 3.3)

  return(list(
    x = pasture_suit_area,
    weight = NULL,
    unit = "Mha",
    description = "Area suitable for pasture management in mha",
    isocountries = FALSE
  ))
}

