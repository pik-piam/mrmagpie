#' @title calcBphEffect
#' @description Biogeophysical temperature change of afforestation (degree C).
#'              File is based on observation datasets of Bright et al. 2017
#'              and Duveiller et al. 2018
#' @param cells lpjcell for 67420 cells or magpiecell for 59199 cells
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("BphEffect", aggregate = FALSE)
#' }
#'
#' @importFrom madrat readSource calcOutput

calcBphEffect <- function(cells = "lpjcell") {

  # load BphEffect data
  bph   <- readSource("Windisch2021", subtype = "refordefor_BPHonly_05_new",
                    convert = "onlycorrect")

  # prepare filled (0) plain for the nobgp case
  x <- new.magpie(cells_and_regions = getItems(bph, dim = 1),
                  years = NULL,
                  names = c("nobgp", "ann_bph"),
                  fill = 0)
  x[, , "ann_bph"] <- bph[, , 1]

  # read in Koeppen data and cell area weight
  k      <- setYears(readSource("Koeppen", subtype = "cellular",
                                convert = "onlycorrect")[, "y1976", ],
                     NULL)

  # mapping to connect cell names with latitudes
  map <- toolGetMappingCoord2Country(pretty = TRUE)
  map$isocoord <- paste(map$coords, map$iso, sep = ".")

  # assuming 0 was NA before.
  x[, , "ann_bph"][x[, , "ann_bph"] == 0] <- NA

  # extract climate class names
  cclass <- getNames(k)

  # select dominating climate class
  ccl <- apply(k, 1, function(k) {
    cclass[which(k == max(k))]
  })
  # add to mapping
  map <- cbind(map, ccl)

  # global mean, used in case of NA for all cells for a give lat
  mGlo <- mean(x[, , "ann_bph"], na.rm = TRUE)

  # Loop over climate classes
  for (sel in cclass) {
    # get the magpie cells corresponding to cl
    gridcells <- map[ccl %in% sel, "isocoord"]
    # find gridcells with NA
    gridcellsNA <- gridcells[is.na(x[gridcells, , "ann_bph"])]
    # If all gridcells are NA, use mGlo, otherwise calc mean based on the non NA gridcells.
    if (identical(gridcells, gridcellsNA)) {
      x[gridcellsNA, , "ann_bph"] <- mGlo
    } else {
      m <- mean(x[gridcells, , "ann_bph"], na.rm = TRUE)
      x[gridcellsNA, , "ann_bph"] <- m
    }
  }

  # clean naming
  getSets(x) <- c("x", "y", "iso", "year", "data")

  # weight for clustering
  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

  # reduce to required number of cells
  if (cells == "magpiecell") {
    x      <- toolCoord2Isocell(x, cells = cells)
  }

  return(list(x = x,
              weight = weight,
              unit = "degC",
              description = "Biogeophysical temp change of afforestation in degC",
              isocountries = FALSE))
}
