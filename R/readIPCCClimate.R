#' @title readIPCCClimate
#' @description Read IPCC climate classification
#'
#' @return Magpie object with results on cellular level for 12 IPCC climate zone types
#' @author  Kristine Karstens
#' @examples
#'
#' \dontrun{
#' readSource("IPCCClimate", convert="onlycorrect")
#' }
#'
#' @importFrom raster raster aggregate extract

readIPCCClimate <-  function(){

  raster_1d12   <- raster("CLIMATE_ZONE.rst")
  zone_names    <- as.character(levels(raster_1d12)[[1]]$Class_name)
  raster_1d2    <- aggregate(raster_1d12, fact=6, fun=max) # to avoid gaps (since 0 is NA)

  map           <- as.data.frame(magpie_coord)
  mag           <- clean_magpie(as.magpie(extract(raster_1d2,map), spatial=1))
  #map[mag==0]   <- 6
  cellNames     <- toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)$celliso
  getNames(mag) <- "NA"
  getYears(mag) <- NULL
  getCells(mag) <- cellNames
  getSets(mag)    <- c("country.cell","t","climatezone")

  out   <- add_columns(mag, dim=3.1, addnm=zone_names)
  out[] <- 0
  for(zone in c(zone_names)){out[,,zone][which(mag==which(zone_names==zone))] <- 1}
  out[,,"NA"][which(dimSums(out, dim=3)==0)] <- 1
  out <- mbind(out[,,zone_names],out[,,"NA"])

  return(out)
}
