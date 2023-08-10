#' @title convertGPD2022
#' @description convert GPD2022
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on iso level, weight, unit and description.
#' @author Florian Humpenoeder
#' @examples
#' \dontrun{
#' readSource("GPD2022", convert = TRUE)
#' }
#'
convertGPD2022 <- function(x) {
  # convert to Mha
  x <- x / 1000

  # create output object for filling
  out <- new.magpie(getCells(x), NULL, c("intact", "crop", "past", "forestry", "peatExtract"), fill = 0)

  ## Merge deeply drained (DD) and shallow drained (SD) peatlands for composite agriculture variables
  ag <- setNames(dimSums(x[, , c("agricultureDD", "agricultureSD")], dim = 3), "agriculture")
  # use croplandDD and grasslandDD as weight
  w <- x[, , c("croplandDD", "grasslandDD")]
  # assume 0.5 as weight in case of no information
  w[dimSums(w, dim = 3) == 0] <- 0.5
  # names
  getNames(w) <- c("crop", "past")
  # create the mapping
  from <- c("agriculture",
            "agriculture")
  to <- c("crop",
          "past")
  map <- data.frame(from, to)
  # do the disaggregation into cropland and grassland
  ag2 <- toolAggregate(ag, map, from = "from", to = "to", dim = 3, weight = w)
  # add the disaggregated categories to the existing categories
  out[, , "crop"] <- setNames(x[, , "croplandDD", ], "crop") + ag2[, , "crop"]
  out[, , "past"] <- setNames(x[, , "grasslandDD", ], "past") + ag2[, , "past"]

  ## Merge deeply drained (DD) and shallow drained (SD) peatlands used for forestry
  out[, , "forestry"] <- dimSums(x[, , c("forestryDD", "forestrySD")], dim = 3)

  ## peatExtract
  out[, , "peatExtract"] <- x[, , "peatExtract"]

  ## calculate intact peatland area
  out[, , "intact"] <- collapseNames(x[, , c("peatlandAreaTotal")]) - dimSums(out, dim = 3)

  # dimnames
  names(dimnames(out)) <- c("iso", "t", "d3")

  # area cannot be smaller than 0
  out[out < 0] <- 0

  # fill missing countries
  out <- toolCountryFill(out, fill = 0)

  return(out)
}
