#' @title readZabel2014
#' @description Reads crop suitability data published in Zabel, F., Putzenlechner, B., & Mauser, W. (2014). Global Agricultural Land Resources – A High Resolution Suitability Evaluation and Its Perspectives until 2100 under Climate Change Conditions. PLOS ONE, 9(9), e107522. https://doi.org/10.1371/journal.pone.0107522 and extracts the share of suitable cropland per grid cell, depending on different suitability thresholds.
#' @param subtype The different options are:
#' \itemize{
#' \item \code{"all_marginal"}: Of the total marginal land (suitability index = 0.0 - 0.33), areas with an index of 0.1 and lower are excluded.
#' \item \code{"half_marginal"}: Areas with an suitability index of 0.16 and lower are excluded.
#' \item \code{"no_marginal"}: Areas with an suitability index of 0.33 and lower are excluded.
#' }
#' @return Returns magpie objects with the share of suitable cropland per grid cell
#' @author Patrick v. Jeetze
#' @examples
#' \dontrun{
#' readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
#' }
#'
#'

readZabel2014 <- function(subtype = "all_marginal") {

  # read Zabel data (make sure that .hdr file is also in folder)
  zabel_raw <- raster("./cropsuitability_rainfed_and_irrigated/1981-2010/overall_cropsuit_i_1981-2010/overall_cropsuit_i_1981-2010.bil")
  crs(zabel_raw) <- "+proj=longlat +datum=WGS84 +no_defs"
  # minimum and maximum crop suitability value in the raw data
  zabel_min <- 0
  zabel_max <- 100

  # rescale the data to 0 - 1
  zabel_rescaled <- zabel_si <- (zabel_raw - zabel_min) * ((1 - 0) / (zabel_max - zabel_min))

  # define suitability threshold for crop suitability in MAgPIE at original resolution of 30 arc seconds
  # In Zabel et al. (2014) marginal land is defined by a suitability index <= 0.33

  zabel_si <- zabel_rescaled
  if (subtype == "all_marginal") {
    # for consistency with older magpie versions this is the same threshold that was applied to the previous
    # data set by Ramankutty et al. (2002)
    zabel_si[zabel_si <= 0.1] <- 0
    zabel_si[zabel_si > 0.1] <- 1
  } else if (subtype == "half_marginal") {
    # the suitability threshold is set at half the range of marginal land
    zabel_si[zabel_si <= 0.2] <- 0
    zabel_si[zabel_si > 0.2] <- 1
  } else if (subtype == "no_marginal") {
    # marginal land is fully excluded
    zabel_si[zabel_si <= 0.33] <- 0
    zabel_si[zabel_si > 0.33] <- 1
  }

  # exclude all NA's and set other cells to 1 to count total available land cells
  zabel_landcells <- zabel_rescaled
  zabel_landcells[!is.na(zabel_landcells)] <- 1

  # aggregate and sum up suitable pixels
  # in effect this means counting the pixels that are suitable (1) per 0.5 degree grid cell
  # aggregation factor from 30 arc sec to 0.5 degree: 60
  zabel_si_0.5 <- aggregate(zabel_si, fact = 60, fun = sum)
  # aggregate and sum to obtain total land cells
  zabel_landcells_0.5 <- aggregate(zabel_landcells, fact = 60, fun = sum, na.rm = T)

  # divide by number of land cells to obtain share of suitable crop land per 0.5 degree cell
  zabel_si_share_0.5 <- zabel_si_0.5 / zabel_landcells_0.5
  # in some cells -> 0/0=NA
  zabel_si_share_0.5[is.na(zabel_si_share_0.5)] <- 0

  ### Create magpie object

  # get spatial mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  # transform raster to magpie object
  out <- as.magpie(extract(zabel_si_share_0.5, map[c("lon", "lat")]), spatial = 1)
  # set dimension names
  dimnames(out) <- list("x.y.iso"=paste(map$coords, map$iso, sep = "."), "t"="y1985", "data"=paste0("si0_",subtype))

  return(out)
}
