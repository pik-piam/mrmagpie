#' @title calcCellArea
#' @description total cell area
#' @param cells "magpiecell" for 59199 cells or "lpjcell" for 67420 cells
#' @return magpie object in cellular resolution
#' @author Jan Philipp Dietrich
#'
#' @examples
#' \dontrun{ calcOutput("CellArea", aggregate = FALSE) }
#'

calcCellArea <-function(cells = "magpiecell"){

  x <- calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, input_magpie=TRUE,
                  cells = cells, years="y1995", round=6)
  x <- setYears(dimSums(x, dim = 3), NULL)
  return(list(x=x,
              weight=NULL,
              unit="mio. ha",
              description="total land area in a cell",
              isocountries=FALSE))
}
