#' @title calcPastrMngtLevels
#' @description Calculates managed pasture potential yields for different combinations of SSP+RCP and grassland management options
#' @param climatetype SSP+RCP combination
#' @param options Management options simulated by LPJml
#' @param cost_level level cost for different past management options
#' @return magpie object in 0.5 degree resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("PastrMngtLevels", ssps, options)
#' }
#'
#' @import madrat
#' @importFrom raster crs


calcPastrMngtLevels  <- function(climatetype = "MRI-ESM2-0:ssp370", options = c("brazil_1","brazil_2","brazil_4"), cost_level = c(1,2,3) ) {
  
  x  <- toolSplitSubtype(climatetype, list(model = NULL, type =NULL ))

    gCm2yTotDMy <- (10000 * 2.21 / 1e6)
    y <- list()
    
    for(opt in options) {
          y[[opt]] <- setNames(calcOutput("Pastr_new", past_mngmt = paste0(x$type,"/", opt), lpjml ="lpjml5p2_pasture", climatetype = "MRI_ESM2_0:CardosoScenarios", scenario = "", aggregate = F)[,,"rainfed"], paste0(x$type,".",opt))
     }
    y <- mbind(y)
    y <- y * gCm2yTotDMy
    
    #costs calculation
    c <- y
    for(i in seq_along(getItems(y, dim =3))){
      c[,,i] <- cost_level[i]
    }
    getItems(c, dim = 3.1)  <- paste0(rep("cost",length(cost_level)))
    y <- mbind(y,c)

    # Calculating weights
    landcoords <- as.data.frame(toolGetMapping("magpie_coord.rda", type = "cell", where = "mappingfolder"))
    landcoords <- cbind(landcoords, rep(1, nrow(landcoords)))
    landcoords <- raster::rasterFromXYZ(landcoords)
    crs(landcoords) <- "+proj=longlat"
    cell_size <- raster::area(landcoords)
    weight <- cell_size * landcoords
    weight <- as.magpie(weight)
    weight <- toolOrderCells(collapseDim(addLocation(weight), dim = c("x", "y")))
    # weight <- calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE)[,1995,c("range", "pastr")]

    return(
      list(
        x = y,
        weight = weight,
        unit = "t/DM/y",
        description = paste("Maximum grasslands yields obtained with rangelands and managed pastures yields for", x$type),
        isocountries = FALSE
      )
    )
  }
