#' @title calcAgeClassDistribution
#' @description This function calculates the share of each age class in secondary forests in each MAgPIE simulation cluster based on Global Forest Age Dataset from Poulter et al. 2019
#'
#' @return magpie object in cluster resolution
#' @author Abhijeet Mishra, Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AgeClassDistribution", aggregate = FALSE) }
#'

calcAgeClassDistribution <- function(){

  poulter_dataset <- readSource("GFAD", convert="onlycorrect")  ## Poulter data is fraction of each cell

  #Area of cells
  mapping   <- toolGetMapping(type="cell",name="CountryToCellMapping.csv")
  lon <- mapping$lon
  lat <- mapping$lat

  cb <- as.data.frame(magpie_coord) ## magpie_coord is loaded automatically with package -- not when running line by line
  cell_area  <- (111e3*0.5)*(111e3*0.5)*cos(cb$lat/180*pi)

  cell_area        <- as.data.frame(cell_area)
  cell_area$cell   <- mapping$celliso
  cell_area_magpie <- as.magpie(cell_area[,c(2,1)], filter=FALSE)
  getNames(cell_area_magpie) <- NULL

  ######################

  getCells(poulter_dataset) <- mapping$celliso

  forest_area <- poulter_dataset*cell_area_magpie

  forest_area <- dimSums(forest_area,dim=3.1)

  #getNames(forest_area) <- paste0("ac",1:15*10)
  getNames(forest_area) <- gsub(pattern = "X",replacement = "class",x = getNames(forest_area))

  zero_forest_area <- dimSums(forest_area,dim=3)

  ac_distribution <- forest_area/dimSums(forest_area,dim=3)

  ## Set age classes to 0 where forest does not exist
  ac_distribution[where((setYears(zero_forest_area,"y2000"))==0)$true$regions,,] <- 0 ## Only checking where zero forest area exists

  out <- ac_distribution

  names(dimnames(out))[1] <- "ISO.cell"

  return(list(
    x=out,
    weight=cell_area_magpie,
    unit="1",
    description="Fraction of each age class in secondary forest from each spatially explicit cell as described in Poulter age classes",
    isocountries=FALSE))
}
