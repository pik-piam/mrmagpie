#' @title calcAvlLandSi
#' @description Extracts si0 and nsi0 areas based on Ramankutty dataset
#'
#' @param dataprocessing just a temporary argument until read Ramankutty is fully functional (original or mrmagpie)
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AvlLandSi", aggregate = FALSE) }
#'
#' @import magclass
#' @import magpiesets
#'

calcAvlLandSi <-function(dataprocessing="original", cells="magpiecell") {

  if (dataprocessing=="original") {
    x <- readSource("AvlLandSi", convert="onlycorrect")

  } else if (dataprocessing=="mrmagpie") {
    # input data (Ramankutty)
    x        <- readSource("Ramankutty", convert="onlycorrect")

    # cell area according to LUH in initialization year (1995) [not: landarea is constant, so the year does not matter]
    landarea <- dimSums(calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995"), dim=3)
    # next two lines are temporary!! (AS SOON AS READY FOR 67k cells: use addLocation)
    coordinates        <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))$coordinates
    getCells(landarea) <- coordinates

    # add missing cells to Ramankutty data (fill with 0)
    tmp <- new.magpie(cells_and_regions = getCells(landarea)[getCells(landarea) %in% getCells(x)==FALSE], years = getYears(x), names = getNames(x), fill = 0)
    x   <- mbind(x, tmp)

    #### Calculations
    # cell is either suitable or not suitable for cropland
    si0_binary                    <- x
    si0_binary[si0_binary > 0.1]  <- 1
    si0_binary[si0_binary <= 0.1] <- 0
    getNames(si0_binary) <- NULL

    # suitable
    si0  <- si0_binary[getCells(landarea),,] * landarea
    # nonsuitable
    nsi0 <- landarea - si0

    out <- mbind(setNames(si0,"si0"),setNames(nsi0,"nsi0"))

    if (cells=="magpiecell") {
      out <- out[magclassdata$cellbelongings$LPJ_input.Index,,]
    } else if (cells=="lpjcell") {
      out <- out
    } else {
      stop("Please specify cells argument")
    }

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
