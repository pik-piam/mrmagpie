#' @title readBendingTheCurve
#' @description Read bending the curve data
#' @param subtype Data used in the Bending the Curve initiative. Type "rr_layer" for the range-size rarity layer and "luh2_side_layers" for the LUH2 Side Layers.
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Patrick v. Jeetze
#' @examples
#'
#' \dontrun{
#'   readSource("BendingTheCurve", subtype="rr_layer", convert="onlycorrect")
#' }
#'

readBendingTheCurve <- function(subtype) {
  
  # coordinate mapping
  map <- toolGetMappingCoord2Country(pretty = TRUE)

  if (subtype == "rr_layer") {
    x <- terra::rast("./RangeRarityLayer/table_weights_30Nov2017.nc")
    x <- x[["weighted.rescaled.logTransCstBase"]]

    out <- as.magpie(extract(x, map[c("lon", "lat")])[, 2], spatial = 1)
    dimnames(out) <- list(
      "x.y.iso" = paste(map$coords, map$iso, sep = "."),
      "t" = NULL,
      "data" = NULL
    )
    return(out)
  }

  if (subtype == "luh2_side_layers") {
    x <- read.magpie("./LUHSideLayers/table_LUH_side_data_16Nov2017.nc")
    getYears(x) <- NULL

    manpast <- collapseNames(x[,,"is_pasture1ORrRangeland0"])
    manpast[is.na(manpast)] <- 0 #assume rangeland in case of NA
    rangeland <- -(manpast-1)
    getNames(manpast) <- "manpast"
    getNames(rangeland) <- "rangeland"

    primveg <- collapseNames(x[,,"is_PrimVeg1ORSecoVeg0"])
    primveg[is.na(primveg)] <- 0 #assume secdveg in case of NA
    secdveg <- -(primveg-1)
    getNames(primveg) <- "primveg"
    getNames(secdveg) <- "secdveg"

    #p <- a[,,c("MaskFvsNF_aggval")]
    forested <- collapseNames(x[,,"MaskFvsNF_aggval"])
    forested[is.na(forested)] <- 0 #assume nonforested in case of NA
    nonforested <- -(forested-1)
    getNames(forested) <- "forested"
    getNames(nonforested) <- "nonforested"

    all <- mbind(manpast,rangeland,primveg,secdveg,forested,nonforested)

    out <- all[map$coords,,]

    return(out)
  }

  if (subtype != "luh2_side_layers" || subtype != "rr_layer") {
    stop("Not a Valid Subtype")
  }
}
