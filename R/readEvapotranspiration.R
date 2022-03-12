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
    x <- toolSplitSubtype(subtype, list(water_model=NULL, climate_model=NULL, scenario = NULL))
    file_name <- Sys.glob("*.nc")
    r <-  rast(file_name)
    months <- length(names(r))
    years <- seq(1,months/12,1)
    y <- rast(r,nlyrs = max(years))


    for (year in years) {
      end_month <- year * 12
      avg_month <- seq(end_month-11,end_month,1)
      y[[year]] <- sum(r[[avg_month]])/12
    }
    y <- raster::brick(y)
    y <- as.magpie(y, temporal = 1)
    y <- toolCoord2Isocell(y)

    if (x$scenario == "historical") {
      getItems(y, dim = 2) <- seq(1850,2014,1)
    } else {
      getItems(y, dim = 2) <- seq(2015,2100,1)
    }

    y <- setNames(y, x$scenario)
    return(y)
  }

