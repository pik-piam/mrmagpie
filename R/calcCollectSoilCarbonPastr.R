#' @title calcCollectSoilCarbonPastr
#' @description calculates soil carbon content for pasture areas
#' @param past_mngmt pasture areas management option
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param sar Average range for smoothing annual variations
#' @param scenario scenario specifications (eg. ssp126_co2_limN)
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("CollectSoilCarbonPastr", past_mngmt = "me2")
#' }
#'
#'
#' @import madrat
#' @import magclass
#' @importFrom raster rasterFromXYZ
#' @importFrom raster area
#' @importFrom raster "crs<-"
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'

calcCollectSoilCarbonPastr <-
  function(past_mngmt = "me2", lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN", sar = 1) {

    .subtype <- paste(lpjml, climatetype,paste0(scenario,"_", past_mngmt),sep = ":")
    hist <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "soilc_past_hist", sep = ":"), convert = F))
    scen <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "soilc_past_scen", sep = ":"), convert = F))
    y <- mbind(hist,scen)
    y <- toolTimeAverage(y, averaging_range = sar)
    y <- toolHoldConstant(y, (max(getYears(y, as.integer = TRUE)) + 1):2150)
    getNames(y) <- "pastr"

    unit_transform <- 0.01               # Transformation factor gC/m^2 --> t/ha
    y <- y * unit_transform

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
    landcoords <- cbind(landcoords, rep(1,nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cell_size <- raster::area(landcoords)
    weight <- cell_size*landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight),dim=c("x","y")))

    return(
      list(
        x = y,
        weight = weight,
        unit = NULL,
        description = "Soil carbon stocks for managed pastures areas",
        isocountries = FALSE
      )
    )
  }

