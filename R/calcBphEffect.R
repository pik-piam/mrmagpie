#' @title calcBphEffect
#' @description Biogeophysical temperature change of afforestation (degree C).
#'              File is based on observation datasets of Bright et al. 2017
#'              and Duveiller et al. 2018
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch, Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("BphEffect", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset
#' @importFrom madrat readSource

calcBphEffect <- function() {

  cells <- "magpiecell"

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
                       convert = "onlycorrect")[, 1995, ], NULL)

  if (cells == "magpiecell") k <- toolCoord2Isocell(k)
  weight <- calcOutput("LandArea", cells = cells, aggregate = FALSE)

  # mapping to connect cell names with latitudes
  map <- toolGetMapping(type = "cell", name = "CountryToCellMapping.csv")
  # map <- toolGetMappingCoord2Country(pretty = TRUE) # mapping for 67420 # nolint

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
    cells <- map[ccl %in% sel, "celliso"]
    # find cells with NA
    cellsNA <- cells[is.na(x[cells, , "ann_bph"])]
    # If all cells are NA, use mGlo, otherwise calc mean based on the non NA cells.
    if (identical(cells, cellsNA)) {
      x[cellsNA, , "ann_bph"] <- mGlo
    } else {
      m <- mean(x[cells, , "ann_bph"], na.rm = TRUE)
      x[cellsNA, , "ann_bph"] <- m
    }
  }

  return(list(
    x = x,
    weight = weight,
    unit = "degC",
    description = "Biogeophysical temp change of afforestation in degC",
    isocountries = FALSE))
}
