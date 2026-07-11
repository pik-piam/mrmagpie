#' @title readGAMI
#' @description Read the Global Age Mapping Integration (GAMI) v2.1 forest-age dataset
#' (Besnard et al. 2024, GFZ Data Services, doi:10.5880/GFZ.1.4.2023.006), 0.5-degree
#' `class_fraction` product. GAMI provides the *within-forest* age distribution: for every
#' grid cell the fraction of the forested area in each of 12 age classes (sums to 1 where
#' forest exists), as a 20-member ensemble for 2010 and 2020. This read returns the
#' ensemble mean on the 67420 lpj-cell grid; the area weighting and the mapping onto the
#' 15 GFAD-style MAgPIE age classes happen in \code{calcAgeClassDistribution}.
#'
#' @return magpie object on the 67420 lpj-cell grid: cells x 2 years (2010, 2020) x 12 age classes
#' @author Florian Humpenoeder
#' @seealso \code{\link{calcAgeClassDistribution}}, \code{readGFAD}
#' @examples
#' \dontrun{
#' readSource("GAMI", convert = "onlycorrect")
#' }
#' @importFrom magclass new.magpie getSets
readGAMI <- function() {

  file <- "GAMIv2-1_2010-2020_class_fraction_0deg50.nc"

  nc <- ncdf4::nc_open(file)
  withr::defer(ncdf4::nc_close(nc))
  glon     <- ncdf4::ncvar_get(nc, "longitude")
  glat     <- ncdf4::ncvar_get(nc, "latitude")
  ageClass <- ncdf4::ncvar_get(nc, "age_class")           # "0-20", ..., "200-299", ">299"

  # forest_age dims (stored order): time(2), longitude(720), latitude(360), age_class(12), members(20)
  arr <- ncdf4::ncvar_get(nc, "forest_age")
  arr <- apply(arr, c(1, 2, 3, 4), mean, na.rm = TRUE)    # ensemble mean -> [time, lon, lat, age]
  arr[is.na(arr)] <- 0

  # coordinate join onto the 67420 lpj cells (value-based, orientation-proof)
  mapping <- mstools::toolGetMappingCoord2Country()
  parts   <- strsplit(mapping$coords, ".", fixed = TRUE)
  lon     <- as.numeric(gsub("p", ".", vapply(parts, `[`, character(1), 1)))
  lat     <- as.numeric(gsub("p", ".", vapply(parts, `[`, character(1), 2)))
  li      <- match(round(lon, 2), round(glon, 2))
  la      <- match(round(lat, 2), round(glat, 2))
  if (anyNA(li) || anyNA(la)) stop("readGAMI: some lpj cells did not match the GAMI 0.5-degree grid")

  years   <- c("y2010", "y2020")
  classNm <- paste0("gami", seq_along(ageClass))          # gami1..gami12 (labels in getComment)
  out <- new.magpie(cells_and_regions = mapping$coords, years = years, names = classNm, fill = 0)
  for (tt in seq_len(2)) {
    for (k in seq_along(ageClass)) {
      out[, years[tt], classNm[k]] <- arr[tt, , , k][cbind(li, la)]
    }
  }
  getSets(out) <- c("x", "y", "year", "ageClass")
  # keep the human-readable class edges (0-20, ..., >299) for the calc's remap
  attr(out, "ageClassLabels") <- ageClass

  return(out)
}
