#' @title readMAPSPAM
#' @description Reads the MAP-SPAM crop data per year (mapping each year different)
#' @return magpie object with croparea data in ha
#' @author Edna J. Molina Bacca
#' @param subtype It can be either "harvested" or "physical" area
#' @importFrom terra rast values crds aggregate
#' @importFrom luscale speed_aggregate
#' @importFrom madrat toolGetMapping
#' @importFrom luscale speed_aggregate
#' @importFrom magpiesets findset
#' @importFrom magclass new.magpie
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("MAPSPAM")
#' }
#'
readMAPSPAM <- function(subtype = "harvested") {

  type <- subtype
  mapping <- toolGetMapping("CountryToCellMapping.rds", where = "mrcommons")
  kcr <- findset("kcr")
  kcr <- c(kcr, "remaining")
  out <- new.magpie(cells_and_regions = mapping[, "celliso"], years = c(2000, 2005, 2010),
                    names = c(paste0(kcr, ".rainfed"), paste0(kcr, ".irrigated")), fill = NA)

  for (year in c(2000, 2005, 2010)) {
  Spam2Magpie <- if (year == 2000) toolGetMapping("SPAMtoMAGPIE2000.csv",
                                                  type = "sectoral", where = "mrcommons") else
                                                    toolGetMapping("SPAMtoMAGPIE2005.csv",
                                                                   type = "sectoral", where = "mrcommons")
  colnames(Spam2Magpie) <- c("crop", "name", "SPAM", "Magpie")
  cropsSpam <- Spam2Magpie[, "SPAM"]

  ty <- if (type == "harvested") "HA" else if (type == "physical") "PA" else stop("Not a valid type")
  factor <- 6

    hisT <- NULL
    hisI <- NULL

    for (i in seq_len(length(cropsSpam))) {

        if (year == 2000) {
        # Reads raster data from SPAM
        tyArea <- if (ty == "HA") "harvested-area" else if (ty == "PA") "physical-area"
        rasterAscT <- paste0("spam2000v3r7_global_", ty, "_geotiff/spam2000v3r7_", tyArea, "_", cropsSpam[i], ".tif")
        rasterAscI <- paste0("spam2000v3r7_global_", ty, "_geotiff/spam2000v3r7_", tyArea, "_", cropsSpam[i], "_I.tif")
      } else if (year == 2005) {
        # Reads raster data from SPAM
        tyArea <- if (ty == "HA") "H" else if (ty == "PA") "A"
        tyArea1 <- if (ty == "HA") "harv_area" else if (ty == "PA") "phys_area"

        rasterAscT <- paste0("spam2005v3r2_global_", ty, "_geotiff/geotiff_global_", tyArea1, "/SPAM2005V3r2_global_",
                             tyArea, "_TA_", cropsSpam[i], "_A.tif")
        rasterAscI <- paste0("spam2005v3r2_global_", ty, "_geotiff/geotiff_global_", tyArea1, "/SPAM2005V3r2_global_",
                             tyArea, "_TI_", cropsSpam[i], "_I.tif")
      } else if (year == 2010) {
        tyArea <- if (ty == "HA") "H" else if (ty == "PA") "A"

        rasterAscT <- paste0("spam2010v2r0_global_", ty, "_geotiff/spam2010V2r0_global_", tyArea, "_",
                             cropsSpam[i], "_A.tif")
        rasterAscI <- paste0("spam2010v2r0_global_", ty, "_geotiff/spam2010V2r0_global_", tyArea, "_",
                             cropsSpam[i], "_I.tif")
      }

      .ValuesExtract <- function(rasterAscT) {

        dataAscT <- rast(rasterAscT)
        dataAscT <- aggregate(dataAscT, fact = factor, fun = sum)
        valuesAscT <- values(dataAscT, mat = FALSE, dataframe = TRUE)
        valuesAscT[is.na(valuesAscT)] <- 0
        coorAscT <- round(crds(dataAscT, na.rm = FALSE), 2)
        rasDataAsc <- as.data.frame(cbind(coorAscT, valuesAscT))
        colnames(rasDataAsc) <- c("lon", "lat", "Value")
        historicalT <- merge(rasDataAsc, mapping, by = c("lon", "lat"))
        historicalT$Year <- year
        historicalT$crop <- paste0(cropsSpam[i])

        return(historicalT[, c("celliso", "Year", "crop", "Value")])
      }


      if (file.exists(rasterAscT) & file.exists(rasterAscI)) {

        historicalT <- .ValuesExtract(rasterAscT)
        hisT <- rbind(hisT, historicalT)

        historicalI <- .ValuesExtract(rasterAscI)
        hisI <- rbind(hisI, historicalI)


      }
    }

    .convertMag <- function(hisT) {

      MagObjSPAMT <- as.magpie(hisT)
      Spam2Magpie <- Spam2Magpie[Spam2Magpie$SPAM %in% getNames(MagObjSPAMT), ]
      MagObjSPAMT <- speed_aggregate(MagObjSPAMT, rel = Spam2Magpie, from = "SPAM", to = "Magpie", dim = 3)
      getCells(MagObjSPAMT) <- gsub("_", "\\.", getCells(MagObjSPAMT))

      return(MagObjSPAMT)
    }

    MagObjSPAMT <- .convertMag(hisT)
    MagObjSPAMI <- .convertMag(hisI)
    MagObjSPAMR <- MagObjSPAMT - MagObjSPAMI

    MagObjSPAM <- mbind(setNames(MagObjSPAMI, paste0(getNames(MagObjSPAMI), ".irrigated")),
               setNames(MagObjSPAMR, paste0(getNames(MagObjSPAMR), ".rainfed")))

    out[, intersect(getYears(out), getYears(MagObjSPAM)), intersect(getNames(out), getNames(MagObjSPAM))] <- MagObjSPAM
  }

   return(out)
}
