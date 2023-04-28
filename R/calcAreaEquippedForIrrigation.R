#' @title calcAreaEquippedForIrrigation
#' @description Calculates the area equipped for irrigation based on LU2v2 or
#'              Mehta data sets.
#'              For LUH2v2, it assumes, that all cropland irrigated in the last
#'              20 years at least once is equipped for irrigation.
#'              Mehta et al. (2022) directly report Global Area Equipped for
#'              Irrigation for the years 1900-2015
#'
#' @param cellular    if TRUE: 0.5 degree resolution returned
#' @param cells       number of cells to be returned: magpiecell (59199), lpjcell (67420)
#' @param selectyears default on "past"
#'
#' @return List of magpie objects with results on country/cellular level,
#'         weight on country level, unit and description.
#'
#' @author Benjamin Leon Bodirsky, Kristine Karstens, Felicitas Beier
#'
#' @seealso
#' [calcLanduseInitialisation()]
#' @examples
#' \dontrun{
#' calcOutput("AreaEquippedForIrrigation", source = "LUH2v2", cellular = TRUE, aggregate = FALSE)
#' }
#' @importFrom luscale groupAggregate
#' @importFrom magpiesets findset
#'
#' @export

calcAreaEquippedForIrrigation <- function(cellular = FALSE,
                                          cells = "magpiecell",
                                          selectyears = "past") {

  selectyears <- sort(findset(selectyears, noset = "original"))

  ##########################################
  ### Read in LUH2v2 irrigated area data ###
  ##########################################
  yearsNeeded <- as.integer(substring(selectyears, 2))
  yearsNeeded <- (yearsNeeded[1] - 20):tail(yearsNeeded, 1)

  x <- collapseNames(calcOutput("LUH2v2",
                                landuse_types = "magpie",
                                irrigation = TRUE,
                                cellular = TRUE, cells = "lpjcell",
                                selectyears = yearsNeeded,
                                aggregate = FALSE)[, , "irrigated"])
  x     <- dimSums(x, dim = 3)
  years <- as.numeric(substring(selectyears, 2))
  luh   <- NULL

  # Cropland that it is irrigated at least once in the last 20 years
  # is defined as "equipped for irrigation"
  for (year in years) {
    span <- (year - 20):year
    tmp <- setYears(as.magpie(apply(X = x[, span, ], FUN = max, MARGIN = 1),
                              spatial = 1),
                    paste0("y", year))
    luh <- mbind(luh, tmp)
  }

  # Naming of first dimension:
  # Temporarily (until 67k preprocessing merged)
  mapping <- toolGetMappingCoord2Country(pretty = TRUE)
  getItems(luh, dim = 1, raw = TRUE) <- paste(mapping$coords, mapping$iso, sep = ".")
  # Temporarily (until 67k preprocessing merged)

  # rename sets
  getSets(luh) <- c("x", "y", "iso", "year", "data")

  # rename data dimension
  getItems(luh, dim = 3) <- "luh2v2"

  ########################################
  ### Read in Mehta et al. (2022) data ###
  ########################################
  mehta <- readSource("Mehta2022", convert = "onlycorrect")
  years <- intersect(getItems(mehta, dim = 2), selectyears)
  mehta <- mehta[, years, ]
  getItems(mehta, dim = 3) <- "mehta2022"

  #########################
  ### Combine data sets ###
  #########################
  out <- mbind(luh, mehta)

  ##############
  ### Output ###
  ##############
  # reduce to 59k cells
  if (cells == "magpiecell") {
    out <- toolCoord2Isocell(out)
  }

  # aggregate to iso level
  if (!cellular) {
    if (length(getItems(out, dim = 1)) == 67420) {
      out <- dimSums(out, dim = c("x", "y"))
    } else {
      mapping <- toolGetMapping(name = "CountryToCellMapping.rds",
                                where = "mrcommons")
      out <- toolAggregate(out, rel = mapping,
                           from = "celliso", to = "iso", dim = 1)
    }
    out <- toolCountryFill(out, fill = 0)
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "Million ha",
              description  = "Area equipped for irrigation",
              isocountries = !cellular))
}
