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
#'
#'


calcRangelandsMax <-
  function(lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = "LPJmL_range", climatetype = "HadGEM2_ES:rcp8p5:co2", report = "harvest") {

    #tidyr variables initiation to avoid problems with buildlibrary()
    water <- NULL
    year <- NULL
    cell <- NULL
    lsu_ha <- NULL
    value <- NULL


    lsu_levels <- gsub("\\.", "p", lsu_levels)
    y <- list()
    for (lsu in lsu_levels) {
      .subtype <- paste(paste(lpjml, climatetype, lsu, sep = ":"), "harvest", sep = ".")
      x <- readSource("LPJmL", subtype = .subtype, convert = "onlycorrect")
      x <- x[, , "mgrass"]
      getNames(x) <- gsub("mgrass",lsu,getNames(x))
      y[[lsu]] <- x
    }
    y <- mbind(y)
    w <- as.array(y)
    w <- as.data.frame(w)
    w$cell <- rownames(w)
    y <-
      pivot_longer(
        w,
        cols = -last_col(),
        names_to = c("year", ".value", "water"),
        names_sep = "\\."
      )
    y <- pivot_longer(y, cols = matches("^[0-9]*p*[0-9]*$"), names_to = "lsu_ha")
    y <- mutate(y, year = substr(year, 2, 5), lsu_ha = as.numeric(gsub("p", ".", lsu_ha)))
    y <- group_by(y, cell, year, water)
    y <- filter(y, value == max(value))
    y <- filter(y, lsu_ha == min(lsu_ha))

    if (report == "harvest") {
      max_harvest <- as.magpie(y[, -4], tidy = TRUE, replacement = ".")
      max_harvest <- add_dimension(max_harvest, dim = 3.1, add = "CO2", nm = "range")
      return(
        list(
          x = max_harvest,
          weight = NULL,
          unit = "t/DM/y",
          description = "Maximum pasture yields obtained with rangelands",
          isocountries = FALSE
        )
      )
    }
    if (report == "lsu/ha") {
      optm_lsu <- as.magpie(y[, -5], tidy = TRUE, replacement = ".")
      optm_lsu[optm_lsu > 2.5] <- 2.5
      optm_lsu <- add_dimension(optm_lsu, dim = 3.1, add = "CO2", nm = "range")
      return(
        list(
          x = optm_lsu,
          weight = NULL,
          unit = "lsu/ha",
          description = "Optimal LSU density that corresponds to the maximum grass yields",
          isocountries = FALSE
        )
      )
    }
  }

