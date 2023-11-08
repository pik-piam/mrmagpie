#' @title readLeifeld2018
#' @description read potential peatland area from Leifeld2018
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#'
#' \dontrun{
#'   readSource("Leifeld2018", convert="onlycorrect")
#' }
#' @importFrom magclass as.magpie

readLeifeld2018 <- function(){

  x <- terra::rast("Degradation_raster_homolosine_hires_rev4.tif")  #"+proj=igh"
  #re-project to regular grid
  r <- terra::rast(res=0.5)
  rp2 <- suppressWarnings(terra::project(x,r))
  #get cell area
  a <- terra::cellSize(rp2[[1]], unit="ha", mask = TRUE) * 1e-6
  # get spatial mapping
  map <- mrcommons::toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  x <- as.magpie(terra::extract(a, map[c("lon", "lat")])[, -1], spatial = 1)
  # set dimension names
  #dimnames(x) <- list("x.y.iso" = paste(map$coords, map$iso, sep = "."), "t" = NULL, "data" = NULL)
  dimnames(x) <- list("coords" = map$coords, "t" = NULL, "d3" = NULL)

  return(x)
}
