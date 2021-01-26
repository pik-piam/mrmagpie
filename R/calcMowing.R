#' @title calcMowing
#' @description Calculates mowing pasture yields
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{calcOutput("Mowing")}
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'

calcMowing <-
  function() {

    x <- readSource("PastYields", subtype = "mowing",  convert = "onlycorrect")
    x <- add_dimension(x, dim = 3.1, add = "mngmt", nm="mowing")
    x <- add_dimension(x, dim = 3.4, add = "water", nm=c("rainfed","irrigated"))

        return(
          list(
            x = x,
            weight = NULL,
            unit = "tDM/ha/yr",
            description = paste("Mowing pasture yields"),
            isocountries = FALSE
          )
        )

    }

