#' @title calcISIMIP3bYields
#' @description reads and cleans up ISIMIP3b crop yield data
#' @param subtype subtype of yield based on readISIMIPoutputs, for crop yields
#' @param time spline or average, if spline specify dof, if average specify averaging range
#' @param dof degrees of freedom for spline
#' @param cells magpie or lpjcell
#'  @return magpie object in cellular resolution
#' @author David Meng-Chuen Chen
#' @import mrcommons
#' @examples
#' \dontrun{ calcOutput("ISIMIP3bYields", aggregate = FALSE) }
#'
#' @importFrom mstools toolHoldConstant


calcISIMIP3bYields <-function(subtype = "yields:EPIC-IIASA:ukesm1-0-ll:ssp585:default:3b", cells="magpiecells",
                            time="spline", dof=4){

if (grepl("historical", subtype)){
  stop ("Can only read full future scenarios for now, with historical already added")
}

  st <- toolSplitSubtype(subtype, list(dataset = "yields",
                                      model   = c("LPJmL", "EPIC-IIASA", "pDSSAT", "CYGMA1p74"),
                                      gcm     = c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll"),
                                      scen    = c("historical", "ssp126", "ssp370", "ssp585"),
                                      co2     = c("default", "2015co2"),
                                      version = c("2a","2b","3a","3b")))

  past_subtype <- paste(st$dataset, st$model, st$gcm, "historical", st$co2, st$version, sep=":")
  past<- readSource("ISIMIP", subtype=past_subtype, convert=FALSE)
  scen <- readSource("ISIMIP", subtype=subtype, convert=FALSE)

  ### last year of both are chopped off due to GGCMI processing, dont use and rather interpolate
  past <- past[,2014,inv=T]
  scen <- scen[,2100, inv=T]

  x <- mbind(past,scen)
  x <- toolCoord2Isocell(x, cells=cells)

# spline or average before interpolating the 2014 and 2100 values

  if (time=="spline"){
  x <- toolTimeSpline(x, dof=dof)
}
  if (time=="average"){
    x <- toolTimeAverage(x)
  }

# toolTimeSpline creates very small values, set these to 0
  x[x<=0.01] <- 0

  x <- time_interpolate(x, interpolated_year = 2014, integrate_interpolated_years = TRUE)
  x <- toolHoldConstant(x, 2100)

 # spline first then take higher yielding wheat and rice variety?
  wheat <- ifelse(x[,,"springwheat",]>x[,,"winterwheat",], x[,,"springwheat",], x[,,"winterwheat",])
  wheat <- add_dimension(collapseNames(wheat), dim=3.1, nm = "tece")
  rice <- ifelse(x[,,"riceA",]>x[,,"riceB",], x[,,"riceA",], x[,,"riceB",])
  rice <- add_dimension(collapseNames(rice), dim=3.1, nm = "rice_pro")

  x <- x[,,c("riceA", "riceB", "springwheat", "winterwheat"), inv=T]

  x <- mbind(x, wheat, rice)
  getNames(x, dim=1)[1] <- "maiz"
  getNames(x, dim=2) <- c("irrigated", "rainfed")

  crop_area_weight     <- dimSums(calcOutput("Croparea", sectoral="kcr", physical=TRUE, irrigation=FALSE,
                                                                cellular=TRUE, cells=cells, aggregate = FALSE, years="y1995", round=6)[,,getNames(x)], dim=3)

  return(list(
    x=x,
    weight=crop_area_weight,
    unit="none",
    description="ISIMIP3b GGCMI yields for soy rice wheat maize",
    isocountries=FALSE))
}
