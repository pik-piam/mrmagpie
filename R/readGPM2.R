#' @title readGPM2
#' @description read peatland area from GPM2
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' readSource("GPM2", convert = "onlycorrect")
#' }
#' @importFrom magclass as.magpie

readGPM2 <- function() {
  terra::terraOptions(tempdir = withr::local_tempdir(tmpdir = getConfig("tmpfolder")),
                      todisk = TRUE, memfrac = 0.5)
  withr::defer(terra::terraOptions(tempdir = tempdir()))

  # Note: terra::project(r, r05, method = "sum") is currently not working on the cluster.
  # Therefore, the object r2 has been pre-calculated and stored as peatMAY22_05deg_mw_RUS30.tif
  if (file.exists("peatMAY22_05deg_mw_RUS30.tif")) {
    r2 <- terra::rast("peatMAY22_05deg_mw_RUS30.tif")
  } else {
    # read-in file
    r <- terra::rast("peatMAY22_1x1_mw_RUS30.tif")

    # 0.5 deg raster object
    r05 <- terra::rast(res = 0.5)

    # project r to 0.5 deg raster
    r2 <- terra::project(r, r05, method = "sum")

    # save
    terra::writeRaster(r2, "peatMAY22_05deg_mw_RUS30.tif", overwrite = TRUE)
  }

  # get cell area
  a <- terra::cellSize(r2[[1]], unit = "ha", mask = TRUE) * 1e-6

  # get spatial mapping
  map <- mrcommons::toolGetMappingCoord2Country(pretty = TRUE)

  # transform raster to magpie object
  x <- as.magpie(terra::extract(a, map[c("lon", "lat")])[, -1], spatial = 1)

  # set dimension names
  dimnames(x) <- list("coords" = map$coords, "t" = NULL, "d3" = NULL)

  return(x)
}
