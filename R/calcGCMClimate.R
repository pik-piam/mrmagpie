#' @title calcGCMClimate
#' @description Disaggregate CO2 global atmospheric concentration to cellular level
#' @param subtype type of climate data to collect
#' @return magpie object in cellular resolution
#' @author Marcos Alves, Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("GCMClimate", subtype = "HadGEM2_ES:rcp8p5.temperature") }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset
#'

calcGCMClimate <-function(subtype = "HadGEM2_ES:rcp8p5.temperature"){
  type <- unlist(strsplit(subtype, split = "\\."))[2]
  x <- readSource("GCMClimate", subtype=subtype, convert="onlycorrect")

  .unit = switch (type,
                  "temperature"          = "Degree Celcius",
                  "precipitation"        = "mm3 per year",
                  "longwave_radiation"   = "watt per m2",
                  "shortwave_radiation"  = "watt per m2",
                  "wetdays"              = "day")

  .description = switch (type,
                         "temperature"          = "Average annual air temperature (Mitchell & Jones 2005)",
                         "precipitation"        = "Average precipitation (Becker et al. 2013)",
                         "longwave_radiation"   = "?",
                         "shortwave_radiation"  = "?",
                         "wetdays"              = "?")

  return(list(
    x=x,
    weight=NULL,
    unit=.unit,
    description=.description,
    isocountries=FALSE))
}
