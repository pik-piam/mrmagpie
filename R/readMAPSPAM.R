#' @title readMAPSPAM
#' @description Reads the MAP-SPAM crop data per year (mapping each year different)
#' @return magpie object with croparea data in ha
#' @author Edna J. Molina Bacca, Felicitas Beier
#' @param subtype It can be either "harvested" or "physical" area
#' @importFrom terra rast values crds aggregate
#' @importFrom mstools toolGetMappingCoord2Country
#' @importFrom magpiesets findset
#' @importFrom magclass new.magpie
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("MAPSPAM")
#' }
readMAPSPAM <- function(subtype = "harvested") {

  type    <- subtype
  mapping <- toolGetMappingCoord2Country(pretty = TRUE)
  kcr     <- findset("kcr")
  kcr     <- c(kcr, "remaining")
  out     <- new.magpie(cells_and_regions = paste(mapping$coords, mapping$iso, sep = "."),
                        years = c(2000, 2005, 2010),
                        names = c(paste0(kcr, ".rainfed"), paste0(kcr, ".irrigated")),
                        fill = NA)

  for (year in c(2000, 2005, 2010)) {
    if (year == 2000) {
      spam2Magpie <- toolGetMapping("SPAMtoMAGPIE2000.csv",
                                    type = "sectoral", where = "mrcommons")
    } else {
      spam2Magpie <- toolGetMapping("SPAMtoMAGPIE2005.csv",
                                    type = "sectoral", where = "mrcommons")
    }
    colnames(spam2Magpie) <- c("crop", "name", "SPAM", "Magpie")
    cropsSpam <- spam2Magpie[, "SPAM"]

    if (type == "harvested") {
      ty <- "harv_area"
    } else if (type == "physical") {
      ty <- "phys_area"
    } else {
      stop("Not a valid type")
    }
    # factor for aggregation
    factor <- 6

    hisT <- NULL
    hisI <- NULL

    for (i in seq_len(length(cropsSpam))) {

      if (year == 2000) {
        # Reads raster data from SPAM
        tyArea <- if (ty == "harv_area") "H" else if (ty == "phys_area") "P"
        rasterAscT <- paste0("spam2000v3r7_global_", ty, ".geotiff/spam2000V3r107_global_", tyArea, "_",
                             cropsSpam[i], "_A.tif")
        rasterAscI <- paste0("spam2000v3r7_global_", ty, ".geotiff/spam2000V3r107_global_", tyArea, "_",
                             cropsSpam[i], "_I.tif")

      } else if (year == 2005) {
        # Reads raster data from SPAM
        tyArea <- if (ty == "harv_area") "H" else if (ty == "phys_area") "A"

        rasterAscT <- paste0("spam2005v3r2_global_", ty, ".geotiff/geotiff_global_", ty, "/SPAM2005V3r2_global_",
                             tyArea, "_TA_", cropsSpam[i], "_A.tif")
        rasterAscI <- paste0("spam2005v3r2_global_", ty, ".geotiff/geotiff_global_", ty, "/SPAM2005V3r2_global_",
                             tyArea, "_TI_", cropsSpam[i], "_I.tif")

      } else if (year == 2010) {

        tyArea <- if (ty == "harv_area") "H" else if (ty == "phys_area") "A"

        rasterAscT <- paste0("spam2010v2r0_global_", ty, ".geotiff/spam2010V2r0_global_", tyArea, "_",
                             cropsSpam[i], "_A.tif")
        rasterAscI <- paste0("spam2010v2r0_global_", ty, ".geotiff/spam2010V2r0_global_", tyArea, "_",
                             cropsSpam[i], "_I.tif")

      }

      .valuesExtract <- function(rasterAscT) {
        # load raster
        dataAscT <- rast(rasterAscT)
        # aggregate to 0.5 resolution
        dataAscT <- terra::aggregate(dataAscT, fact = factor, fun = sum, na.rm = TRUE)

        # extract values from raster
        valuesAscT <- values(dataAscT, mat = FALSE, dataframe = TRUE)
        valuesAscT[is.na(valuesAscT)] <- 0
        # extract coordinate information from rster
        coorAscT <- round(crds(dataAscT, na.rm = FALSE), 2)

        # combine coordinate info and values in data frame
        rasDataAsc <- as.data.frame(cbind(coorAscT, valuesAscT))
        colnames(rasDataAsc) <- c("lon", "lat", "Value")

        # merge mapping with raster data
        historicalT <- merge(rasDataAsc, mapping, by = c("lon", "lat"))
        # transform to magpie object
        historicalT <- historicalT[, c("coords", "Value")]
        historicalT <- as.magpie(historicalT, spatial = 1)

        # add dimension names
        getItems(historicalT, dim = 2) <- year
        getItems(historicalT, dim = 3) <- paste0(cropsSpam[i])

        # name and reorder first dimension
        getItems(historicalT, dim = 1, raw = TRUE) <- gsub("_", ".", getItems(historicalT, dim = 1))
        historicalT <- historicalT[mapping$coords, , ]
        getItems(historicalT, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")

        # rename sets
        getSets(historicalT) <- c("x", "y", "iso", "year", "Value")

        return(historicalT)
      }

      if (file.exists(rasterAscT) && file.exists(rasterAscI)) {

        historicalT <- .valuesExtract(rasterAscT)
        hisT        <- mbind(hisT, historicalT)

        historicalI <- .valuesExtract(rasterAscI)
        hisI        <- mbind(hisI, historicalI)

      }
    }

    .convertMag <- function(x) {
      spam2Magpie <- spam2Magpie[spam2Magpie$SPAM %in% getNames(x), ]
      x           <- toolAggregate(x, rel = spam2Magpie, from = "SPAM", to = "Magpie", dim = 3)
      return(x)
    }

    magObjSPAMT <- .convertMag(hisT)
    magObjSPAMI <- .convertMag(hisI)
    magObjSPAMR <- magObjSPAMT - magObjSPAMI

    magObjSPAM <- mbind(setNames(magObjSPAMI, paste0(getNames(magObjSPAMI), ".irrigated")),
                        setNames(magObjSPAMR, paste0(getNames(magObjSPAMR), ".rainfed")))

    out[, intersect(getYears(out), getYears(magObjSPAM)),
        intersect(getNames(out), getNames(magObjSPAM))] <- magObjSPAM[, intersect(getYears(out), getYears(magObjSPAM)),
                                                                      intersect(getNames(out), getNames(magObjSPAM))]
  }

  return(out)
}
