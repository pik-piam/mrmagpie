#' @title calcContGrazMax_new
#' @description Calculates continuous grazing maximum output
#' @param report Either 'harvest' or 'lsu/ha' controlling what values are output by the function.
#' @param lsu_levels Livestock unit levels in the source folder
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param scenario specify ssp scenario
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{calcOutput("ContGrazMax_new", lsu_levels = 0, lpjml, climatetype, report)}
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'


calcContGrazMax_new <-
  function(lsu_levels = c(seq(0, 2.2, 0.2), 2.5),lpjml = "lpjml5.2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_limN", report = "harvest") {

    #tidyr variables initiation to avoid problems with buildlibrary()
    water <- NULL
    year <- NULL
    cell <- NULL
    lsu_ha <- NULL
    value <- NULL
    toolHoldConstant

    lsu_levels <- gsub("\\.", "p", lsu_levels)
    years <- 1965:2100
    y <- list()
    for (lsu in lsu_levels) {
      .subtype <- paste(lpjml, climatetype,paste0(scenario,"_", lsu),sep = ":")
      hist <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_hist", sep = ":"), convert = F)[,, "mgrass"])
      scen <- toolCoord2Isocell(readSource("LPJmL_new", subtype = paste(.subtype, "grass_pft_scen", sep = ":"), convert = F)[,, "mgrass"])
      x <- mbind(hist,scen)
      x <- x[,years,]
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
    y <- filter(y, lsu_ha == max(lsu_ha))

    if (report == "harvest") {
      max_harvest <- as.magpie(y[, -4], tidy = TRUE, replacement = ".")
      max_harvest <- add_dimension(max_harvest, dim = 3.1, add = "CO2", nm = "cont_grazing")
      return(
        list(
          x = max_harvest,
          weight = NULL,
          unit = "t/DM/y",
          description = "Maximum pasture yields obtained with continuous grazing",
          isocountries = FALSE
        )
      )
    }
    if (report == "lsu/ha") {
      optm_lsu <- as.magpie(y[, -5], tidy = TRUE, replacement = ".")
      optm_lsu[optm_lsu > 2.5] <- 2.5
      optm_lsu <- add_dimension(optm_lsu, dim = 3.1, add = "CO2", nm = "cont_grazing")
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

