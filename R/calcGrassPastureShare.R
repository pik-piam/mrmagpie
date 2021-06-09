#' @title calcGrassPastureShare
#' @description Calculate glassland shareas os pasture managed lands.
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves
#' @examples
#'
#' \dontrun{
#' calcOutput("GrassPastureShare")
#' }
#' @importFrom raster rasterFromXYZ
#' @importFrom raster area

calcGrassPastureShare <- function(){

  # historical values
  LUH2v2 <- calcOutput("LUH2v2", landuse_types = "LUH2v2", cellular=TRUE, aggregate = F)

  #Shares
  t_past <- findset("past")
  t_past <- t_past[length(t_past)]
  grass_area <-  setNames(LUH2v2[,t_past,"pastr"] + LUH2v2[,t_past,"range"], "grasslands")
  pasture_hist_share <- setNames(LUH2v2[,t_past,"pastr"]/grass_area, "past_share")
  pasture_hist_share[is.nan(pasture_hist_share)] <- 0
  pasture_hist_share[is.infinite(pasture_hist_share)] <- 0


  # Cell area calculation
  landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell"))
  landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
  landcoords <- raster::rasterFromXYZ(landcoords)
  crs(landcoords) <- "+proj=longlat"
  cell_size <- raster::area(landcoords)
  cell_size <- cell_size * landcoords
  cell_size <- as.magpie(cell_size)
  cell_size <- toolOrderCells(collapseDim(addLocation(cell_size), dim = c("x", "y")))

  return(list(
    x = pasture_hist_share,
    weight = cell_size,
    unit = "",
    description = "Share or grasslands managed as pastures",
    isocountries = FALSE
  ))
}

