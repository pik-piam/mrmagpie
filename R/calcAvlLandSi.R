#' @title calcAvlLandSi
#' @description Extracts si0 and nsi0 areas based on Ramankutty dataset
#'
#' @param dataprocessing just a temporary argument until read Ramankutty is fully functional (original or mrmagpie)
#' @param cell magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AvlLandSi", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#'

calcAvlLandSi <-function(dataprocessing="original", cell="magpiecell"){

  if (dataprocessing=="original") {
    x <- readSource("AvlLandSi", convert="onlycorrect")

  } else if (dataprocessing=="mrmagpie") {
    # input data (Ramankutty)
    x        <- readSource("Ramankutty", convert="onlycorrect")

    ##### how to make dimensions match?
    ##### can I use 67k as standard here?
    ##### should I also return 59k cells? (as subset or with alternative argument?)

    # cell area according to LUH in initialization year (1995)
    cellArea <- dimSums(calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995"), dim=3)

    ##### use cell area in 1995 or 2000???? (see script in other/rev23...)


    # temporary!! (AS SOON AS READY FOR 67k cells: use addLocation); or even better: transform all data to coordinate inputs
    coordinates        <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))$coordinates
    getCells(cellArea) <- coordinates

    # empty magpie object with correct dimension
    out           <- new.magpie(cells_and_regions = getCells(cellArea), fill=0)
    getCells(out) <- coordinates

    #### Calculations
    # cell is either suitable or not suitable for cropland
    si0_binary                    <- x
    si0_binary[si0_binary > 0.1]  <- 1
    si0_binary[si0_binary <= 0.1] <- 0
    getNames(si0_binary) <- NULL

    # suitable
    si0  <- si0_binary * cellArea
    # nonsuitable
    nsi0 <- cellArea - si0

  } else {
    stop("Please specify the datapreprocessing argument: original or mrmagpie")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="Mha",
    description="si and nsi0 areas based on Ramankutty suitability information and LUH area information from initialization year",
    isocountries=FALSE))
}
