#' @title readCO2Atmosphere
#' @description Read CO2 global atmospheric concentration
#' @param subtype Switch between different inputs
#' @return Magpie objects with results on global level
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readCO2Atmosphere}}
#' @examples
#'
#' \dontrun{
#' readSource("CO2Atmosphere", subtype="rcp8p5", convert=FALSE)
#' }
#'
#' @import madrat
#' @importFrom utils read.table

readCO2Atmosphere <-
  function(subtype = "rcp8p5") {

    files <- c(rcp8p5 = "rcp85_CO2_1765-2200.dat",
               rcp6p0 = "rcp60_CO2_1765-2200.dat",
               rcp4p5 = "rcp45_CO2_1765-2200.dat",
               rcp2p6 = "rcp26_CO2_1765-2200.dat")

    file  <- toolSubtypeSelect(subtype,files)

    years <-seq(1900,2150, 1)


    x  <- array(NA, dim = c(1, length(years),1), dimnames = list(1, years, "co2"))
    y  <- read.table(file)
    id <- match(years, y[, 1])
    y  <- y[id, 2]

    for (i in 1:length(years)) {
      x[, i, ] <- y[i]
    }

    x  <- clean_magpie(collapseNames(as.magpie(x, spatial = 1)))
    getNames(x) <- paste("CO2ATMconcentration",subtype, sep = "_")
    getCells(x) <- "GLO"

    return(x)

  }


