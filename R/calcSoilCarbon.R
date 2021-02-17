#' @title calcSoilCarbon
#' @description Calculate soil carbon stocks for different LSU and climate conditions
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param sar Average range for smoothing annual variations
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon", lsu_levels = c(seq(0, 2, 0.2), 2.5))
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'

calcSoilCarbon <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJmL_cgrazing", climatetype = "HadGEM2_ES:rcp8p5:co2", sar = 20) {
    lsu_levels <- gsub("\\.", "p", lsu_levels)
    y <- list()
    for (lsu in lsu_levels) {
      .subtype <- paste(paste(lpjml, climatetype, lsu, sep = ":"), "soilc", sep = ".")
      x <- readSource("LPJmL", subtype = .subtype, convert = "onlycorrect")
      getNames(x) <- gsub("soilc", lsu, getNames(x))
      y[[lsu]] <- x
    }
    y <- mbind(y)
    y <- toolTimeAverage(y, averaging_range = sar)
    y <- toolHoldConstant(y, (max(getYears(y, as.integer = TRUE)) + 1):2150)

    return(
      list(
        x = y,
        weight = NULL,
        unit = "lsu/ha",
        description = "Optimal LSU density that corresponds to the maximum grass yields",
        isocountries = FALSE
      )
    )
  }
