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


# Marcos / AlexK: Is this still used? Only called with development flag.

calcPastrMngtLevels  <- function(climatetype = "MRI-ESM2-0:ssp370",
                                 options = c("brazil_1","brazil_2","brazil_4"),
                                 cost_level = c(1,2,3) ) { # nolint
 
  x <- toolSplitSubtype(climatetype, list(model = NULL, type = NULL))

  gCm2yTotDMy <- (10000 * 2.21 / 1e6)
  y <- list()
    
  for (opt in options) {
    y[[opt]] <- setNames(calcOutput("Pastr_new", past_mngmt = paste0(x$type,"/", opt),
                                    lpjml = "lpjml5p2_pasture", 
                                    climatetype = "MRI_ESM2_0:CardosoScenarios", # Marcos/AlexK: weird climate scenario
                                    scenario = "", # Marcos/AlexK: empty scenario?
                                    cells = cells, 
                                    aggregate = FALSE)[,,"rainfed"], paste0(x$type,".",opt))
  }
  y <- mbind(y)
  y <- y * gCm2yTotDMy
  
  # Costs calculation
  c <- y
  for(i in seq_along(getItems(y, dim = 3))){
    c[, , i] <- cost_level[i]
  }
  getItems(c, dim = 3.1) <- paste0(rep("cost", length(cost_level)))
  y <- mbind(y,c)

  # Land area as weights
  weight <- calcOutput("LandArea", cells = cells)

  return(
    list(
      x = y,
      weight = weight,
      unit = "t/DM/y",
      description = paste0("Maximum grasslands yields obtained with rangelands ", 
                            "and managed pastures yields for ", x$type),
      isocountries = FALSE
    )
  )
}
