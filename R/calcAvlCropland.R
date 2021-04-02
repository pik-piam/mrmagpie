#' @title calcAvlCropland
#' @description Calculates the total available cropland per grid cell, based on physical
#' cropland suitability data or other criteria, such as constraints on cropland expansion
#'
#' @param cropland_scen different options are
#' \itemize{
#' \item \code{"all_marginal"}: Include all marginal land
#' \item \code{"half_marginal"}: Half of the marginal land is excluded
#' \item \code{"no_marginal"}: Marginal land is fully excluded
#' \item \code{"all"}: Returns all of the above options
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

  if (any(grepl("all_marginal", cropland_scen)) | cropland_scen == "all") {
    cropsuit <- readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
    # cropland suitability is corrected where LUH reports cropland
    cropsuit <- pmax(cropsuit, crop_shr_luh)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuit_area <- cropsuit * landarea

    tmp <- cropsuit_area
    getNames(tmp) <- "all_marginal"
    x <- mbind(x, tmp)
  }

  if (any(grepl("half_marginal", cropland_scen)) | cropland_scen == "all") {
    cropsuit <- readSource("Zabel2014", subtype = "half_marginal", convert = "onlycorrect")
    # cropland suitability is corrected where LUH reports cropland
    cropsuit <- pmax(cropsuit, crop_shr_luh)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuit_area <- cropsuit * landarea

    tmp <- cropsuit_area
    getNames(tmp) <- "half_marginal"
    x <- mbind(x, tmp)
  }

  if (any(grepl("no_marginal", cropland_scen)) | cropland_scen == "all") {
    cropsuit <- readSource("Zabel2014", subtype = "no_marginal", convert = "onlycorrect")
    # cropland suitability is corrected where LUH reports cropland
    cropsuit <- pmax(cropsuit, crop_shr_luh)
    # calculate suitable cropland area (Mha) per grid cell
    cropsuit_area <- cropsuit * landarea

    tmp <- cropsuit_area
    getNames(tmp) <- "no_marginal"
    x <- mbind(x, tmp)
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
    description = "Cropland suitability based on Zabel et al. (2014) with different suitability thresholds ('all_marginal', 'half_marginal', 'no_marginal').",
    isocountries = FALSE
  ))
}
