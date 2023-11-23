#' @title Apply region names
#'
#' @description This tool function replaces country names with region names
#' in the spatial dimension of the object. To avoid mixing up
#' of cache files with different regional aggregation the
#' regioncode needs to supplied and checked as well.
#' Only if the supplied regions code agrees with the
#' region mapping currently chosen the function will return
#' the data.
#'
#' @param cdata       a cluster data file as produced by cluster_base
#' @param regionscode regionscode of the regional mapping to be used.
#'                    Must agree with the regionscode of the mapping mentioned
#'                    in the madrat config!
#'                    Can be retrieved via \code{regionscode()}.
#' @return the cluster data file with region names in spatial dimension
#'         rather than country names
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @seealso \code{\link{calcClusterKMeans}}, \code{\link{calcClusterBase}}
#' @importFrom madrat toolGetMapping regionscode
#' @importFrom mrland spatial_header

toolApplyRegionNames <- function(cdata, regionscode) {
  ### APPLY REGIONS HERE ON SPATIAL NAMING OF CDATA INSTEAD OF COUNTRIES ###
  ### regionscode needs to be checked and provided as argument to ensure
  ### that caching is not mixing up aggregations with different regional
  ### mapping.
  map <- toolGetMapping(type = "regional", where = "mappingfolder", name = getConfig("regionmapping"))

  if (regionscode != regionscode(map)) {
    stop("Provided regionscode does not match regionscode of regional mapping!")
  }

  # Get countries from magpie object and extend mapping
  isocountries <- getItems(cdata, dim = 1.3, full = TRUE)
  isoMap       <- data.frame(CountryCode = isocountries)
  map          <- base::merge(isoMap, map, by = "CountryCode",
                        all.x = TRUE, sort = FALSE, no.dups = TRUE)
  # correct cell order
  map          <- map[match(isocountries, map$CountryCode), ]

  # Add regional information to magpie object
  getItems(cdata, dim = 1, raw = TRUE) <- paste(gsub(".*\\.", "", getItems(cdata, dim = 1)),
                                                map$RegionCode,
                                                as.character(seq_along(isocountries)),
                                                sep = ".")
  getSets(cdata, fulldim = FALSE)[1] <- "country.region.cell"

  return(cdata)
}
