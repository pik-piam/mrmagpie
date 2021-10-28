#' @title readGridPopGao
#' @description Read gridded population, by urban and rural, from Gao O'Neill and JOnes dataset, see https://www.cgd.ucar.edu/iam/modeling/spatial-population-scenarios.html https://doi.org/10.7927/m30p-j498
#' @author David Chen
#' @importFrom raster brick aggregate projectRaster
#' @import magclass


readGridPopGao <- function(){

 mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")

  ssps <- c("SSP1", "SSP2","SSP3", "SSP4", "SSP5")
  years <- seq(2010, 2100, 10)
  urbans <- c("rural", "urban")

 .read <- function(year, urbans, ssps){
    x <- NULL
      for (urb in urbans){
      for (ssp in ssps) {

     if (urb =="rural"){ur <- "rur"} else if(urb=="urban") {ur <- "urb"}

    if (year == 2000) {
    t <- raster(paste0("Base_year_data_2000_NetCDF/2000",
                       urb, ".nc"))}else {
    t <- raster(paste0(ssp, "_NetCDF/", urb, "/NetCDF/", tolower(ssp), ur, year, ".nc"))
    }

   #aggregate and reproject
   r <- raster(res=0.5)
   t<- aggregate(t, fact = 4, fun=sum)
   t <- suppressWarnings(projectRaster(t,r,over=TRUE))

   t <- as.magpie(t)
   t <- toolCoord2Isocell(t)

   #create new magpie to fill in missing cells
   mapping <- toolGetMapping(name="CountryToCellMapping.csv", type="cell")
   new <- new.magpie(cells_and_regions = setdiff(mapping$celliso, getItems(t, dim=1)),
                    years=getYears(t), fill = 0, names=getNames(t))
   t <- mbind(t, new)

  #sort the cells as usual
  t <- t[mapping$celliso,,]

  #proper names
  getYears(t) <- year
  getNames(t, dim=1) <- ssp
  getNames(t, dim=2) <- urb

  x <- mbind(x, t)
    }
    }
  return(x)
  }


# read base year
  base <- .read(year=2000, urbans=urbans, ssps = "SSP1")
  base <- add_columns(base, addnm=c("SSP2","SSP3","SSP4","SSP5"), dim=3.1)
  base[,,c("SSP2","SSP3","SSP4","SSP5")] <- base[,,"SSP1"]

# do the big loop with years as list (can't mbind both list and 3rd dim internally)
  out <-lapply(years, FUN=.read, urbans=urbans, ssps = ssps)
  out <- mbind(out)

#bind base
  x <- mbind(base, out)

return(x)
}

