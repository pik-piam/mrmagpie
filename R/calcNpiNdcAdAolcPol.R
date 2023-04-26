#' @title calcNpiNdcAdAolcPol
#' @description Function creates dummy NPI/NDC policies
#' @param cells lpjcell for 67420 cells or magpiecell for 59199 cells
#'
#' @return magpie object in cellular resolution
#' @author Patrick v. Jeetze, Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("NpiNdcAdAolcPol", aggregate = FALSE) }
#' @importFrom magpiesets findset
#'

calcNpiNdcAdAolcPol <-function(cells = "magpiecell") {

  if (cells == "lpjcell") {

  # cell mappping
  mapping <- toolGetMappingCoord2Country()
  
  # create a dummy data set, which is later used to define NDC and NPI policies
  x <- new.magpie(cells_and_regions = paste(mapping$coords, mapping$iso, sep = "."),
                  years = seq(1995, 2150, 5),
                  names = c("none.forest", "npi.forest", "ndc.forest", "none.other", "npi.other", "ndc.other"),
                  fill = 0, 
                  sets = c("x.y.iso", "year", "data1.new"))

  } else if (cells == "magpiecell") {

    # create a dummy data set, which is later used to define NDC and NPI policies
    x <- new.magpie(cells_and_regions = toolGetMapping("CountryToCellMapping.csv", type="cell")$celliso,
                    years = seq(1995, 2150, 5),
                    names = c("none.forest", "npi.forest", "ndc.forest", "none.other", "npi.other", "ndc.other"),
                    fill = 0,
                    sets = c("region.cell", "year", "data1.new"))

  } else {
    stop("Please select cells argument: lpjcell for 67420 or magpiecell for 59199")
  }

  return(list(x = x,
              weight = NULL,
              unit = "dummy (none)",
              description = paste0("Dummy file for implementing forestry prescribed",
              " exogoneously by NDC/NPI policy as opposed to CDR GHG prices"),
              isocountries = FALSE))
}
