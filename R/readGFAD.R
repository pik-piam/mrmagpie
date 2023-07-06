#' @title readGFAD
#' @description Read GLobal Forest Age Dataset derived from MODIS and COPENICUS satellite data
#'
#' @return magpie object in cellular resolution
#' @author Abhijeet Mishra, Felicitas Beier
#' @examples
#' \dontrun{
#' readSource("GFAD", convert = "onlycorrect")
#' }
#' @importFrom raster brick subset as.matrix t
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom madrat toolGetMapping
#' @importFrom magclass clean_magpie add_dimension setYears getSets
#' @export

readGFAD <- function() {
  poulterData <- "GFAD_V1-1.nc"

  gfad <- nc_open(poulterData)

  mapping   <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv", where = "mappingfolder")

  lon <- ncvar_get(gfad, "lon")
  lat <- ncvar_get(gfad, "lat")

  ageClass <- paste0("X", 1:15)
  forestPoulter <- c("NEEV", "NEDE", "BREV", "BRDC")

  temp <- NULL

  for (forestType in seq_along(forestPoulter)) {
    for (i in ageClass) {
      b <- suppressWarnings(brick(poulterData, level = forestType))
      b <- subset(b, i)

      cellNames <- mapping$celliso

      # Change longitude and latitude
      matrix <- t(as.matrix(b))

      # Create array for 59199 isocells, 1 year and 1 data dimension
      mag   <- array(NA, dim = c(59199, 1, 1), dimnames = list(cellNames, "y2010", i))

      # Fill array with data from raster object (note magpie_coord are

      for (j in 1:59199) {
        mag[j, , ] <- matrix[which(magpie_coord[j, 1] == lon),
                           which(magpie_coord[j, 2] == lat)]
      }

      # Convert array to magpie object and rename set
      x          <- clean_magpie(as.magpie(mag))
      getSets(x) <- c("cell", "t", "ac")
      x <- add_dimension(x = x, dim = 3.1, add = "type", nm = forestPoulter[forestType])

      temp <- mbind(temp, x)
    }
  }

  out <- setYears(temp, NULL)


  return(out)
}
