#' Disaggregate grassland areas
#'
#' Disaggregate grassland areas and move values based on a maximum theorical regional productivity
#' @usage toolGrassProdDisagg(grass_prod_lr,land_hr, weight_factor, map_file, lpjml_yields, ite)
#' @param grass_prod_lr grassland production in low resolution to be disaggregated
#' @param land_hr Magpie object with celular cost for each pasture management type
#' @param weight_factor Magpie object with celular yields for each management type
#' @param map_file cluster mappint to cells and regions
#' @param lpjml_yields Lpjml yeilds used for magpie optimization
#' @param ite number of redistribution iterations
#' @return Mapie object with the total pasture areas optimized for fulfulling pasture demand in the available areas. An extra dimention is added to
#' capture cells that are infeasible.
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' toolGrassProdDisagg(grass_prod_lr,land_hr, weight_factor, map_file, lpjml_yields, ite)
#' }
#'

#' @import magpiesets
#' @export


toolGrassProdDisagg <- function(grass_prod_lr,land_hr, weight_factor, map_file, lpjml_yields, ite = 10000) {

  poten_prod <- lpjml_yields * land_hr

  # poten_prod <- land_hr[,years,"pastr"] * lpjml_yields[,years,"pastr"]
  name <- getItems(land_hr, dim = 3)
  if(length(name) > 1){
    stop("Cannot handle more than one type of grassland at the same time (land_hr)..")
  }
  
  if(dim(weight_factor)[3] > 1){
    stop("Cannot handle more than one type of weight factors at the same time.")
  }
  
  if(dim(grass_prod_lr)[3] > 1){
    stop("Cannot handle more than one type of grassland at the same time (grass_prod_lr).")
  }
  
  if(dim(lpjml_yields)[3] > 1){
    stop("Cannot handle more than one type of grassland at the same time (lpjml_yields).")
  }

  if(!getItems(grass_prod_lr, dim = 3) == getItems(land_hr, dim = 3)){
    stop("Datasets with different names")
    if(!getItems(land_hr, dim = 3) == getItems(lpjml_yields, dim = 3)){
      stop("Datasets with different names")
    }
  }
  
  if (name == "pastr") {
    weight <- land_hr * weight_factor # pasture suitability
    print("Pasture")
  } else {
    weight <- land_hr / weight_factor # accessibility
    weight[is.nan(weight) | is.infinite(weight)] <- 0
    print("rangelands")
  }

  for (i in getYears(weight)) {
    weight[, i, ] <- (weight[, i, ] - min(weight[, i, ])) / (max(weight[, i, ]) - min(weight[, i, ]))
  }
  weight[is.nan(weight) | is.infinite(weight)] <- 0

  grass_prod_hr <- toolAggregate(grass_prod_lr, rel = map_file, weight = weight, from = "cluster", to = "cell")

  # adding location
  poten_prod    <- addLocation(poten_prod)
  grass_prod_hr <- addLocation(grass_prod_hr)
  land_hr  <-  addLocation(land_hr)
  
  # biomass adjustment
  excess_prod <- grass_prod_hr - poten_prod
  excess_prod[excess_prod < 0] <- 0
  count <- 0

  print(paste("Excess poll"," -> ","Regular pool", " -> ","Variation"))
  remain_prod <- grass_prod_hr - excess_prod
  print(paste(sum(excess_prod)," -> ",sum(remain_prod), " -> ", 0))
  while (sum(excess_prod) > 1 & count < ite) {
    red_prod_cell <- list()
    erased_cell <- list()
    for(year in  getYears(remain_prod)) {
      print(year)
        x  <-  excess_prod[,year,]
        y  <-  (x != 0)
        r  <-   poten_prod[,year,] / remain_prod[,year,]
        r[is.na(r) | is.infinite(r)] <- 0
        z  <-  (((x == 0) * (r > 1))) # all areas where there is production but no excess
        red_prod_cell[[year]] <- toolMoveValues(x,y,z)[["redistributed"]]
    }
    red_prod_cell <- mbind(red_prod_cell)
    grass_prod_hr_tmp <- red_prod_cell + remain_prod
    excess_prod <- grass_prod_hr_tmp - poten_prod
    excess_prod[excess_prod < 0] <- 0
    remain_prod <- grass_prod_hr_tmp - excess_prod

    print(paste(sum(excess_prod)," -> ",sum(remain_prod), " -> ", sum(grass_prod_hr - grass_prod_hr_tmp)))
    
    count <- count + 1
  }
    return(grass_prod_hr_tmp)
}
