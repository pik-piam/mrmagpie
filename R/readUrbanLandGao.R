#' @title readUrbanLandGao
#' @description Read gridded urban land, from Gao O'Neill and JOnes SEDAC dataset,https://sedac.ciesin.columbia.edu/data/set/ssp-1-8th-urban-land-extent-projection-base-year-ssp-2000-2100
#' @return magpie object of 2000-2100 urban land in Mha, in 10 year intervals
#' @author David M Chen
#' @importFrom terra aggregate project rast
#' @importFrom raster brick
#' @import magclass


readUrbanLandGao <- function(){

  mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")

  ssps <- c("SSP1", "SSP2","SSP3", "SSP4", "SSP5")
  years <- seq(2010, 2100, 10)

  area <- rast("ancillary-layers-netcdf/land_area_km2.nc")

 .read <- function(years, ssps){

   x <- NULL

    for (year in years){
      for (ssp in ssps) {
        if (year == 2000) {
          t <- rast(paste0("byr-netcdf/urb_frac_2000.nc"))
          }else {
          t <- rast(paste0(tolower(ssp), "-netcdf/", tolower(ssp), "_", year, ".nc"))
          }

        #multiply by land area to get km2 of urban land

        t <- t*area

        #aggregate and reproject
        r <- rast(res=0.5)
        t<- aggregate(t, fact = 4, fun="sum")

        t <- (project(t,r))

        t <- as.magpie(brick(t))
        t <- toolCoord2Isocell(t, fillMissing = 0)

        #proper names
        getYears(t) <- year
        getNames(t) <- ssp
        getSets(t) <- getSets(t)[1:4]

        x <- mbind(x, t)
      }
    }
    return(x)
  }


  # read base year
  base <- .read(years=2000, ssps = "SSP1")
  base <- add_columns(base, addnm=c("SSP2","SSP3","SSP4","SSP5"), dim=3.1)
  base[,,c("SSP2","SSP3","SSP4","SSP5")] <- base[,,"SSP1"]

  # do the big loop with years as list (can't mbind both list and 3rd dim internally)
  out <-lapply(years, FUN=.read, ssps = ssps)
  out <- mbind(out)

  #bind base
  x <- mbind(base, out)

  #convert km2 to Mha
  x <- x/10000
  return(x)
}

