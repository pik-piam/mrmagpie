#' @title calcAvlLandSi
#' @description Extracts si0 and nsi0 areas based on Ramankutty dataset
#'
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AvlLandSi", aggregate = FALSE) }
#'
#' @importFrom madrat readSource calcOutput
#' @importFrom magclass dimSums getCells getYears getNames mbind collapseDim
#' @importFrom mrcommons toolCoord2Isocell toolGetMappingCoord2Country
#' @importFrom magpiesets addLocation
#'

calcAvlLandSi <-function(cells="magpiecell") {

  # input data (Ramankutty)
  x        <- readSource("Ramankutty", convert="onlycorrect")

  # land area according to LUH in initialization year (1995) [note: landarea is constant, so the year does not matter]
  landarea <- calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995")
  landarea <- collapseDim(addLocation(landarea), dim=c("N","cell"))
  croparea <- landarea[,,"crop"]
  landarea <- dimSums(landarea, dim=3)

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
  si0  <- landarea * si0_binary[getCells(landarea),,]
  # correction of suitable land to LUH croparea (land is declared as suitable where LUH reports cropland)
  si0  <- pmax(croparea, si0)
  # not suitable for cropland
  nsi0 <- landarea - si0

  out  <- mbind(setNames(si0,"si0"), setNames(nsi0,"nsi0"))

  # rename dimensions and names to standard x.y.iso
  map                     <- toolGetMappingCoord2Country()
  out                     <- out[map$coords,,]
  getCells(out)           <- paste(map$coords, map$iso, sep=".")
  names(dimnames(out))[1] <- "x.y.iso"

  if (cells=="magpiecell") {
    out <- toolCoord2Isocell(out)
  } else if (cells=="lpjcell") {
    out <- out
  } else {
    stop("Please specify cells argument")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="Mha",
    description="si and nsi0 areas based on Ramankutty suitability information and LUH area information from initialization year",
    isocountries=FALSE))
}
