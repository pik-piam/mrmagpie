#' @title calcAvlCropland
#' @description Calculates the total available cropland per grid cell, based on physical
#' cropland suitability data or other criteria, such as constraints on cropland expansion
#'
#' @param cropland_scen different options are
#' \itemize{
#' \item \code{"allmarginal_0pNonCropVeg"}: All marginal land, 0 \% conservation of non-cropland vegetation
#' \item \code{"allmarginal_10pNonCropVeg"}: All marginal land, 10 \% conservation of non-cropland vegetation
#' \item \code{"allmarginal_20pNonCropVeg"}: All marginal land, 20 \% conservation of non-cropland vegetation
#' \item \code{"allmarginal_30pNonCropVeg"}: All marginal land, 30 \% conservation of non-cropland vegetation
#' \item \code{"halfmarginal_0pNonCropVeg"}: Half of the marginal land excluded, 0 \% conservation of non-cropland vegetation
#' \item \code{"halfmarginal_10pNonCropVeg"}: Half of the marginal land excluded, 10 \% conservation of non-cropland vegetation
#' \item \code{"halfmarginal_20pNonCropVeg"}: Half of the marginal land excluded, 20 \% conservation of non-cropland vegetation
#' \item \code{"halfmarginal_30pNonCropVeg"}: Half of the marginal land excluded, 30 \% conservation of non-cropland vegetation
#' \item \code{"nomarginal_0pNonCropVeg"}: No marginal land, 0 \% conservation of non-cropland vegetation
#' \item \code{"nomarginal_10pNonCropVeg"}: No marginal land, 10 \% conservation of non-cropland vegetation
#' \item \code{"nomarginal_20pNonCropVeg"}: No marginal land, 20 \% conservation of non-cropland vegetation
#' \item \code{"nomarginal_30pNonCropVeg"}: No marginal land, 30 \% conservation of non-cropland vegetation
#' \item \code{"all"}: Returns all information of the above options
#' }
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze
#'
#' @examples
#' \dontrun{
#' calcOutput("AvlCropland", cropland_scen = "all", cells = "magpiecell", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass dimSums getCells getYears getNames mbind collapseDim as.magpie
#' @importFrom mrcommons toolCoord2Isocell toolGetMappingCoord2Country
#' @importFrom magpiesets addLocation
#'

calcAvlCropland <- function(cropland_scen="all", cells = "magpiecell"){

  # read luh data
  luh <- calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995")
  # sum land area per grid cell
  luh <- collapseDim(addLocation(luh), dim=c("N","cell"))
  landarea <- dimSums(luh, dim = 3)
  # calculate crop share in the land use initialisation data
  crop_shr_luh <- luh[, , "crop"] / landarea
  crop_shr_luh[is.na(crop_shr_luh)] <- 0

  x <- as.magpie(NULL)

  if (any(grepl("allmarginal", cropland_scen)) | cropland_scen == "all") {
    cropsuit <- readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
    # cropland suitability is corrected where LUH reports cropland
    cropsuit <- pmax(cropsuit, crop_shr_luh)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuit_area <- cropsuit * landarea

    if (any(grepl("allmarginal_0pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      tmp <- cropsuit_area
      getNames(tmp) <- "allmarginal_0pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("allmarginal_10pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 10 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.1 * cropsuit_area
      getNames(tmp) <- "allmarginal_10pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("allmarginal_20pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 20 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.2 * cropsuit_area
      getNames(tmp) <- "allmarginal_20pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("allmarginal_30pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 30 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.3 * cropsuit_area
      getNames(tmp) <- "allmarginal_30pNonCropVeg"
      x <- mbind(x, tmp)
    }
  }

  if (any(grepl("halfmarginal", cropland_scen)) | cropland_scen == "all") {
    cropsuit <- readSource("Zabel2014", subtype = "half_marginal", convert = "onlycorrect")
    # cropland suitability is corrected where LUH reports cropland
    cropsuit <- pmax(cropsuit, crop_shr_luh)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuit_area <- cropsuit * landarea

    if (any(grepl("halfmarginal_0pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      tmp <- cropsuit_area
      getNames(tmp) <- "halfmarginal_0pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("halfmarginal_10pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 10 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.1 * cropsuit_area
      getNames(tmp) <- "halfmarginal_10pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("halfmarginal_20pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 20 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.2 * cropsuit_area
      getNames(tmp) <- "halfmarginal_20pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("halfmarginal_30pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 30 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.3 * cropsuit_area
      getNames(tmp) <- "halfmarginal_30pNonCropVeg"
      x <- mbind(x, tmp)
    }
  }

  if (any(grepl("nomarginal", cropland_scen)) | cropland_scen == "all") {
    cropsuit <- readSource("Zabel2014", subtype = "no_marginal", convert = "onlycorrect")
    # cropland suitability is corrected where LUH reports cropland
    cropsuit <- pmax(cropsuit, crop_shr_luh)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuit_area <- cropsuit * landarea

    if (any(grepl("nomarginal_0pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      tmp <- cropsuit_area
      getNames(tmp) <- "nomarginal_0pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("nomarginal_10pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 10 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.1 * cropsuit_area
      getNames(tmp) <- "nomarginal_10pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("nomarginal_20pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 20 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.2 * cropsuit_area
      getNames(tmp) <- "nomarginal_20pNonCropVeg"
      x <- mbind(x, tmp)
    }

    if (any(grepl("nomarginal_30pNonCropVeg", cropland_scen)) | cropland_scen == "all") {
      # substract 30 percent of suitable cropland for non-cropland vegetation
      tmp <- cropsuit_area - 0.3 * cropsuit_area
      getNames(tmp) <- "nomarginal_30pNonCropVeg"
      x <- mbind(x, tmp)
    }
  }

  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(x)
  } else if (cells == "lpjcell") {
    out <- x
  } else {
    stop("Please specify cells argument")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = "Cropland suitability based on Zabel et al. (2014) with different suitability thresholds ('all_marginal', 'half_marginal', 'no_marginal') and different minimal requirements regarding non-cropland vegetation (0%, 10%, 20% and 30%)",
    isocountries = FALSE
  ))
}
