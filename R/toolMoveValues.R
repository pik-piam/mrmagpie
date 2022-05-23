#' toolMoveValues
#'
#' Move values in an undesirable cell to the nearest desirable neighbor (Euclidian distance). 
#'
#' @param x Unidimensional magpie object (one time and one data dimension) with location information caring for the 
#' values that must be checked and moved if necessary.
#' @param y Unidimensional magpie object (one time and one data dimension) that has a binary or logical 
#' mapping (see \link[base]{as.logical}) of the unsuitable areas for the values in x
#' @param z Unidimensional magpie object (one time and one data dimension) that has a binary or logical  
#' (see \link[base]{as.logical}) mapping of the areas that can receive the values from x.
#' @param w Unidimensional magpie object (one time and one data dimension) that has a binary or logical  
#' (see \link[base]{as.logical}) mapping of the areas that have to be zeroed. If left empty, the inverse of `z` is assumed.
#' @description Distances are calculated from the lat and lon coordinates. Therefore, all magpie objects must have 
#' location information (see \link[magpiesets]{addLocation}). Values are only moved within a country. If no suitable cell is 
#' available in the same country, the undesirable values are discarded. 
#' This function takes only magpie objects with only one time and data dimensions to allow for more flexibility. 
#' Whenever more than one dimension is available in the magpie objects, 
#' I suggest using a loop (see \link[base]{for} and \link[base]{apply}).
#' @return Unidimensional magpie object with summed values of the moved values to the nearest suitale neighbor. All the unmoved 
#' and discarded values are set to 0.
#' @author Marcos Alves
#' @importFrom magclass getCoords
#' @importFrom class knn1
#' @importFrom tidyr unite
#' @import dplyr
#' @export

toolMoveValues <- function(x, y, z, w = NULL){

    coords.x <- NULL
    coords.y <- NULL
    coords.z <- NULL
    try(coords.x <- getCoords(x), silent = T)
    try(coords.y <- getCoords(y), silent = T)
    try(coords.z <- getCoords(z), silent = T)

    if(is.null(coords.x) | is.null(coords.y) | is.null(coords.z))  {
        stop("At least one of the objects has no coordinates. Add coordinates for Euclidian distance calculations")
    }
    
    if(!(all(is.null(coords.x) == is.null(coords.y)) | all(is.null(coords.z) == is.null(coords.y))))  {
        stop("All inputs must have the same coordinates")
    }

    isoOut <- x
    isoOut[,,] <- 0
    missInfo <- NULL

    for(i in getRegions(x)) {

        iso      <- x[i,,]
        isoWrong <- y[i,,]
        isoAllo  <- z[i,,]

        if(all(any(as.logical(isoWrong)),any(as.logical(isoAllo)))) { #check if there is any missiplaces data and if there is an available place in the region
        coords.y <- getCoords(iso[as.logical(isoWrong),,])
        coords.z <- getCoords(iso[as.logical(isoAllo),,])
        cell     <- factor(getItems(iso[as.logical(isoAllo),,], dim = 1))
        nc       <- class::knn1(coords.z, coords.y, cell)
        
        isoNcDf     <- magclass::as.data.frame(iso[nc,,], rev =2)
        isoWrongDf  <- magclass::as.data.frame(iso[as.logical(isoWrong),,], rev =2)
        names(isoNcDf) <- paste0("nc",".",names(isoNcDf))
        names(isoWrongDf) <- paste0("wr",".",names(isoWrongDf))
        wr..value = NULL
        aggCells  <- cbind(isoNcDf, isoWrongDf)
        aggCells  <-  group_by(aggCells, across(1:2)) %>% 
                               summarise(value = sum(wr..value)) %>% 
                               unite("cell", 1:2, sep= "_", remove = FALSE)

        tmp       <- as.magpie(aggCells[,c("cell", "value")], spatial =1)
        getCells(tmp) <- gsub("_", ".", getCells(tmp))
        tmp <- addLocation(tmp)
        isoOut[getCells(tmp),,]  <- tmp
        } else {
            missInfo <- append(missInfo, i)
        }

    }
        if(is.null(w)) {
            out <- x
            out[!z,,]  <- 0 #setting all non - available areas to 0
            out  <-  isoOut + out
        } else {
            out <- x
            out[w,,]  <- 0 #setting all non - available areas to 0
            out  <-  isoOut + out
        }

    return(out)
}
