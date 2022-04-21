#' @title readTravelTimeNelson2019
#' @description Read minimum travel time to cities and ports and
#' ports of various size, see metadata file in source folder
#' @param subtype cities or ports data
#' @return gridded magpie object for 2015, minimum travel time to cities in minutes
#' @author David M Chen
#' @importFrom terra aggregate project rast classify focal
#' @importFrom raster brick


readTravelTimeNelson2019 <- function(subtype = "cities") {

  files <- list.files(path = "./TravelTimeNelson_unzipped", pattern = ".tif", full.names = TRUE)
  files <- files[grep(pattern = subtype, files)]

  r <- rast(res = 0.5)

  x <- rast(files)
  x <- terra::classify(x, cbind(65534, 65536, NA), right = FALSE)
  x <- aggregate(x, fact = 60, fun = "mean")
  x <- terra::project(x, r)

# fill NAs with focal function (neighbour mean)
  x <- terra::focal(x, w = 3, mean, na.only = TRUE, na.rm = TRUE)

  x <- raster::brick(x)

  out <- as.magpie(x)

 # convert to magpie cells and add missing cells as NA
  out <- toolCoord2Isocell(out, fillMissing = NA)

  #fill with value(s) of cells i away, first try using the average of the 2 sides, if one doesn't exist, use the other
  .fillNeighbours <- function(fill, tofill, i) {

    neighbour <- ifelse(is.na((tofill[which(is.na(tofill)) + i] + tofill[which(is.na(tofill)) - i]) / 2),
                    ifelse(is.na(tofill[which(is.na(tofill)) + i]),
                    tofill[which(is.na(tofill)) - i],
                    tofill[which(is.na(tofill)) + i]),
           (tofill[which(is.na(tofill)) + i] + tofill[which(is.na(tofill)) - i]) / 2)

  fills <- ifelse(is.na(fill), ifelse(is.na(neighbour), NA, neighbour), fill)

  print(length(which(is.na(fills))))
  return(fills)
  }


  #fill NAs with value of cell beside
  fill <- vector(length = length(which(is.na(out))))
  fill[] <- NA
  # run a couple times but still some NAs hard to get rid of
t <- .fillNeighbours(fill = fill, tofill = out, i = 1)
t1 <- .fillNeighbours(fill = t, tofill = out, i = 2)
t2 <- .fillNeighbours(fill = t1, tofill = out, i = 3)
t3 <- .fillNeighbours(fill = t2, tofill = out, i = 4)
t4 <- .fillNeighbours(fill = t3, tofill = out, i = 5)

out[is.na(out)] <- t4

#fill rest with avg for that slice
avgs <- dimSums(out, dim = 1, na.rm = TRUE)/59199

for (i in c(seq(getItems(out, dim=3)))) {
  out[,,i][which(is.na(out[,,i]))] <- as.numeric(avgs[,,i])
}

return(out)
}
