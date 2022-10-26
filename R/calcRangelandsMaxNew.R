#' @title calcRangelandsMaxNew
#' @description Calculates rangelands maximum output
#' @param report Either 'harvest' or 'lsu/ha' controlling what values are output by the function.
#' @param lsuLevels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param scenario specify ssp scenario
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{
#' calcOutput("ContGrazMax_new", lsuLevels = 0, lpjml, climatetype, report)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'


calcRangelandsMaxNew <- function(lsuLevels = c(seq(0, 2.2, 0.2), 2.5), lpjml = "lpjml5.2_pasture",
                                  climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN", report = "harvest") {

    # tidyr variables initiation to avoid problems with buildlibrary()
    water <- NULL
    year <- NULL
    cell <- NULL
    lsuHa <- NULL
    value <- NULL


    lsuLevels <- gsub("\\.", "p", lsuLevels)
    years <- seq(1965, 2100, 5)
    y <- list()
    for (lsu in lsuLevels) {
      .subtype <- paste0(lpjml, ":", climatetype, paste0(scenario, "/", lsu))
      message(.subtype)
      hist <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_hist", sep = ":"),
                                           convert = FALSE)[, , "mgrass"])
      scen <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_scen", sep = ":"),
                                           convert = FALSE)[, , "mgrass"])
      x <- mbind(hist, scen)
      x <- x[, years, ]
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
        cols = -last_col(),
        names_to = c("year", ".value", "water"),
        names_sep = "\\."
      )
    y <- pivot_longer(y, cols = matches("^[0-9]*p*[0-9]*$"), names_to = "lsuHa")
    y <- mutate(y, year = substr(year, 2, 5), lsuHa = as.numeric(gsub("p", ".", lsuHa)))
    y <- group_by(y, cell, year, water)
    y <- filter(y, value == max(value))
    y <- filter(y, lsuHa == min(lsuHa))

    if (report == "harvest") {
      maxHarvest <- as.magpie(y[, -4], tidy = TRUE, replacement = ".")
      maxHarvest <- add_dimension(maxHarvest, dim = 3.1, add = "CO2", nm = "range")
      return(
        list(
          x = maxHarvest,
          weight = NULL,
          unit = "gC/m2/y",
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
