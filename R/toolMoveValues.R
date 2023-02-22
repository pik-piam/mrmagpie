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
#' (see \link[base]{as.logical}) mapping of the areas that have to be zeroed. If left empty, the inverse of `z` is
#' assumed.
#' @description Distances are calculated from the lat and lon coordinates. Therefore, all magpie objects must have
#' location information (see \link[magpiesets]{addLocation}). Values are only moved within a country. If no suitable
#' cell is available in the same country, the undesirable values are discarded.
#' This function takes only magpie objects with only one time and data dimensions to allow for more flexibility.
#' Whenever more than one dimension is available in the magpie objects,
#' I suggest using a loop (see \link[base]{for} and \link[base]{apply}).
#' @return Unidimensional magpie object with summed values of the moved values to the nearest suitale neighbor.
#' All the unmoved and discarded values are set to 0.
#' @author Marcos Alves
#' @importFrom magclass getCoords
#' @importFrom class knn1
#' @importFrom tidyr unite
#' @importFrom dplyr %>%
#' @export

toolMoveValues <- function(x, y, z, w = NULL) {

  coordsX <- NULL
  coordsY <- NULL
  coordsZ <- NULL
  try(coordsX <- getCoords(x), silent = TRUE)
  try(coordsY <- getCoords(y), silent = TRUE)
  try(coordsZ <- getCoords(z), silent = TRUE)

  if (is.null(coordsX) || is.null(coordsY) || is.null(coordsZ)) {
    stop("At least one of the objects has no coordinates. Add coordinates for Euclidian distance calculations")
  }

  if (!(all(is.null(coordsX) == is.null(coordsY)) || all(is.null(coordsZ) == is.null(coordsY)))) {
    stop("All inputs must have the same coordinates")
  }

  isoOut  <- missInfo <- x
  isoOut[, , ]  <- missInfo[, , ] <- 0

  for (i in getItems(x, dim = 1.1)) {

    iso      <- x[i, , ]
    isoWrong <- y[i, , ]
    isoAllo  <- z[i, , ]

    # check if there is any missiplaces data and if there is an available place in the region
    if (all(any(as.logical(isoWrong)), any(as.logical(isoAllo)))) {
      coordsY <- getCoords(iso[as.logical(isoWrong), , ])
      coordsZ <- getCoords(iso[as.logical(isoAllo), , ])
      cell     <- factor(getItems(iso[as.logical(isoAllo), , ], dim = 1))
      nc       <- class::knn1(coordsZ, coordsY, cell)

      isoNcDf     <- magclass::as.data.frame(iso[nc, , ], rev = 2)
      isoWrongDf  <- magclass::as.data.frame(iso[as.logical(isoWrong), , ], rev = 2)
      names(isoNcDf) <- paste0("nc", ".", names(isoNcDf))
      names(isoWrongDf) <- paste0("wr", ".", names(isoWrongDf))
      wr..value <- NULL # nolint
      aggCells  <- cbind(isoNcDf, isoWrongDf)
      aggCells  <- dplyr::group_by(aggCells, dplyr::across(1:2)) %>%
        dplyr::summarise(value = sum(wr..value)) %>%
        unite("cell", 1:2, sep = "_", remove = FALSE)

      tmp       <- as.magpie(aggCells[, c("cell", "value")], spatial = 1)
      getCells(tmp) <- gsub("_", ".", getCells(tmp))
      tmp <- addLocation(tmp)
      isoOut[getCells(tmp), , ]  <- tmp
    } else {
      missInfo <- append(missInfo, i)
    }
  }

  if (is.null(w)) {
    out <- x
    out[!z, , ]  <- 0 # setting all non - available areas to 0
    out  <-  isoOut + out
  } else {
    out <- x
    out[w, , ]  <- 0 # setting all non - available areas to 0
    out  <-  isoOut + out
  }

  return(list("redistributed" = out, "errased" = missInfo))
}
