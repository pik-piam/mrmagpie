#' @title calcContGrazMax
#' @description Calculates continuous grazing maximum output
#' @param report Either 'harvest' or 'lsu/ha' controlling what values are output by the function.
#' @return magpie object in cellular resolution
#' @author Marcos Alves
#'
#' @examples
#' \dontrun{calcOutput("ContGrazMax", report = "harvest")}
#'
#' @import madrat
#' @import magclass
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#'
#'

calcContGrazMax <-
  function(report = "harvest") {


    year <- NULL
    name <- NULL
    cell <- NULL
    lsu_ha <- NULL
    value <- NULL
    CO2 <- NULL
    RCP <- NULL
    x <- readSource("PastYields", subtype = "cont_grazing",  convert = "onlycorrect")

    w <- as.array(x)
    w <- as.data.frame(w)
    w$cell <- rownames(w)
    y <-
      pivot_longer(
        w,
        cols = -last_col(),
        names_to = c("year","CO2", "RCP", ".value"),
        names_sep = "\\."
      )
    y <- pivot_longer(y, cols = matches("^[0-9]*_*[0-9]*$"), names_to = "lsu_ha")
    y <- mutate(y, year = substr(year, 2, 5),
                lsu_ha = as.numeric(gsub("_",".",lsu_ha)))
    # y <- select(y, cell, year, lsu, value)
    y <- group_by(y, cell, year, CO2, RCP)
    y <- filter(y, value == max(value))
    y <- filter(y, lsu_ha == max(lsu_ha))


    if (report == "harvest") {
      max_harvest <- as.magpie(y[, -5], tidy=TRUE, replacement = ".")
      max_harvest <- add_dimension(max_harvest, dim = 3.1, add = "CO2", nm="cont_grazing")
      max_harvest <- add_dimension(max_harvest, dim = 3.3, add = "water", nm=c("rainfed","irrigated"))
      return(
        list(
          x = max_harvest,
          weight = NULL,
          unit = "t/DM/y",
          description = "Maximum pasture yields obtained with continuous grazing",
          isocountries = FALSE
        )
      )
    } else {
      if (report == "lsu/ha") {
        optm_lsu <- as.magpie(y[, -6], tidy=TRUE, replacement = ".")
        optm_lsu[optm_lsu > 2.5] <- 0
        optm_lsu <- add_dimension(optm_lsu, dim = 3.3, add = "water", nm=c("rainfed","irrigated"))
        optm_lsu <- add_dimension(optm_lsu, dim = 3.1, add = "CO2", nm="cont_grazing")
        return(
          list(
            x = optm_lsu,
            weight = NULL,
            unit = "lsu/ha",
            description = "Optimal LSU density that corresponds to the maximum grass yields",
            isocountries = FALSE
          )
        )
      } else {
        stop("Type of output not specified. Please select either 'harvest' or 'lsu/ha'")
      }
    }
  }
