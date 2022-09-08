#' @title readGridPop_new
#' @description Reads in past and future (SSP1-5) gridded population data, from ISIMIP database, Past data is based on HYDE3.2, while future SSPs are based on projections from Jones & O'Neill 2016
#' @return A MAgPIE object, cellular 0.5deg resolution, of population (millions)
#' @param subtype past (1965-2005) or future (2010-2100)
#' @author David Chen, Marcos Alves
#' @importFrom raster brick rasterToPoints
#' @import magclass
#' @importFrom dplyr left_join
#' @importFrom raster extent rasterToPoints brick
#' @import magpiesets


readGridPop_new <- function(subtype) {

  mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")

  if (subtype=="past"){
    ini_past  <- as.numeric(gsub("y", "", findset("past")))[1]
    past_file <- "population_histsoc_0p5min_annual_1861-2005.nc"
    years     <- as.numeric(unlist(regmatches(past_file, gregexpr("\\d{4}", past_file))))
    y_years   <- paste0("y", seq(years[1], years[2], 1))
    x         <- suppressWarnings(brick(past_file))
    names(x)  <- y_years
    x <- subset(x,paste0("y",seq(ini_past,years[2],1)))
    raster::extent(x) <- c(-180,180,-90,90)
    x         <- rasterToPoints(x)
    colnames(x)[c(1, 2)] <- c("lon", "lat")
    x         <- left_join(mapping, x, by = c("lat", "lon"), copy = TRUE)
    drop      <- c("^iso$|lpj|^cell$|lon|lat")
    x         <- as.magpie(x[, !grepl(drop, colnames(x))])
    getCells(x) <- gsub("_", ".", getCells(x))
    x         <- x[, ini_past:years[2], ]
  }

  if(subtype=="future"){

    files <- c("population_ssp1soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp2soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp3soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp4soc_0p5deg_annual_2006-2100.nc4",
               "population_ssp5soc_0p5deg_annual_2006-2100.nc4")

    read <- function(file){
      years     <- as.numeric(unlist(regmatches(file, gregexpr("\\d{4}", file))))
      y_years   <- paste0("y", seq(years[1], years[2], 1))
      x         <- suppressWarnings(brick(file))
      names(x)  <- y_years
      x         <- rasterToPoints(x)
      colnames(x)[c(1, 2)] <- c("lon", "lat")
      x         <- left_join(mapping, x, by = c("lat", "lon"), copy = TRUE)
      drop      <- c("^iso$|lpj|^cell$|lon|lat")
      x         <- as.magpie(x[, !grepl(drop, colnames(x))])
      getCells(x) <- gsub("_", ".", getCells(x))
      return(x)
    }

    x <- list()
    ssp_naming <- NULL
    for (file in files) {
      ssp_naming <- append(ssp_naming,paste0("pop_", toupper(unlist(regmatches(file,gregexpr("ssp[0-9]", file))))))
      x[[file]] <- read(file)
    }
    x <- mbind(x)
    getNames(x) <- ssp_naming
  }
  return(x)
}
