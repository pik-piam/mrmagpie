#' @title calcMAPSPAM
#' @description MAPSPAM data 
#' @return magpie object in cellular resolution
#' @author Edna J. Molina Bacca
#' @examples
#' \dontrun{
#' calcOutput("MAPSPAM", subtype = "physical", aggregate = FALSE)
#' }
#'
#' @importFrom magclass as.magpie

calcMAPSPAM <- function(subtype) {
   
    x <- readSource(type = "MAPSPAM", subtype = subtype) / 1e6 # to convert to mio. of hectares


    return(list(
        x = x,
        weight = NULL,
        unit = "mio. ha",
        description = "MAPSPAM cropland area",
        isocountries = FALSE
    ))
}
