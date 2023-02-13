#' @title calcRangelandsMax
#' @description Calculates rangelands maximum output
#' @param report Either 'harvest' or 'lsu/ha' controlling what values are output by the function.
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{calcOutput("ContGrazMax", lsu_levels = 0, lpjml, climatetype, report)}
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
calcRangelandsMax <- function(lsu_levels = c(seq(0, 2, 0.2), 2.5), # nolint: object_name_linter.
                              lpjml = "LPJmL_range",
                              climatetype = "HadGEM2_ES:rcp8p5:co2",
                              report = "harvest") {

    #tidyr variables initiation to avoid problems with buildlibrary()
    water <- NULL
    year <- NULL
    cell <- NULL
    lsu_ha <- NULL # nolint: object_name_linter.
    value <- NULL


    lsu_levels <- gsub("\\.", "p", lsu_levels) # nolint: object_name_linter.
    y <- list()
    for (lsu in lsu_levels) {
      .subtype <- paste(paste(lpjml, climatetype, lsu, sep = ":"), "harvest", sep = ".")
      x <- readSource("LPJmL", subtype = .subtype, convert = "onlycorrect")
      x <- x[, , "mgrass"]
      getNames(x) <- gsub("mgrass", lsu, getNames(x))
      y[[lsu]] <- x
    }
    y <- mbind(y)
    w <- as.array(y)
    w <- as.data.frame(w)
    w$cell <- rownames(w)
    y <-
      pivot_longer(
        w,
        cols = -dplyr::last_col(),
        names_to = c("year", ".value", "water"),
        names_sep = "\\."
      )
    y <- pivot_longer(y, cols = dplyr::matches("^[0-9]*p*[0-9]*$"), names_to = "lsu_ha")
    y <- mutate(y, year = substr(year, 2, 5), lsu_ha = as.numeric(gsub("p", ".", lsu_ha)))
    y <- dplyr::group_by(y, cell, year, water)
    y <- dplyr::filter(y, value == max(value))
    y <- dplyr::filter(y, lsu_ha == min(lsu_ha))

    if (report == "harvest") {
      maxHarvest <- as.magpie(y[, -4], tidy = TRUE, replacement = ".")
      maxHarvest <- add_dimension(maxHarvest, dim = 3.1, add = "CO2", nm = "range")
      return(
        list(
          x = maxHarvest,
          weight = NULL,
          unit = "t/DM/y",
          description = "Maximum pasture yields obtained with rangelands",
          isocountries = FALSE
        )
      )
    }
    if (report == "lsu/ha") {
      optmLsu <- as.magpie(y[, -5], tidy = TRUE, replacement = ".")
      optmLsu[optmLsu > 2.5] <- 2.5
      optmLsu <- add_dimension(optmLsu, dim = 3.1, add = "CO2", nm = "range")
      return(
        list(
          x = optmLsu,
          weight = NULL,
          unit = "lsu/ha",
          description = "Optimal LSU density that corresponds to the maximum grass yields",
          isocountries = FALSE
        )
      )
    }
  }
