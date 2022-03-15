#' @title readEvapotranspiration
#' @description Read evapotranspiration data
#' @param subtype Switch between different inputs
#' @return MAgPIE objects with results on cellular level.
#' @author Marcos Alves
#' @seealso
#' \code{\link{readEvapotranspiration}}
#' @examples
#' \dontrun{
#' readSource("Evapotranspiration", subtype, convert = "onlycorrect")
#' }
#'
#' @importFrom terra rast
#' @importFrom raster brick
#' @importFrom magclass as.magpie
#' @export

readEvapotranspiration <-
  function(subtype="H08:mri-esm2-0:historical") {

    # I ran cdo yearmonmean to reduce computational costs at the preprocessing. the commented lines are obsolete for now.

    x <- toolSplitSubtype(subtype, list(water_model=NULL, climate_model=NULL, scenario = NULL))

    files <- Sys.glob("*.nc")
    file_name <- grep(x$scenario,files,value = T)
    r <-  brick(file_name)

    y <- as.magpie(r, temporal = 1)
    y <- toolCoord2Isocell(y)

    if (x$scenario == "historical") {
      getItems(y, dim = 2) <- paste0("y",seq(1850,2014,1))
    } else {
      getItems(y, dim = 2) <- paste0("y",seq(2015,2100,1))
    }
    y <- setNames(y, x$scenario)

    return(y)

#     years <- length(names(r))
#     y <- rast(r,nlyrs = max(years))
#
#     for (year in years) {
#       end_month <- year * 12
#       avg_month <- seq(end_month-11,end_month,1)
#       y[[year]] <- sum(r[[avg_month]])/12 #unit: kg m-2 s-1 (mothly average)
#     }
#     y <- raster::brick(r)
#     names(y) <- gsub("\\.","_",names(y))
#     y <- as.magpie(r, temporal = 1)
#     y <- toolCoord2Isocell(y)
#
#     if (x$scenario == "historical") {
#       getItems(y, dim = 2) <- paste0("y",seq(1850,2014,1))
#     } else {
#       getItems(y, dim = 2) <- paste0("y",seq(2015,2100,1))
#     }
#
#     y <- setNames(y, x$scenario)


  }
