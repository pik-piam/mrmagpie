#' @title calcClimateClass
#' @description fraction of a cell belonging to a given climate classification based on Koeppen Geiger Classification. http://koeppen-geiger.vu-wien.ac.at/.
#' @param source select source from: Koeppen, IPCC, IPCC_reduced
#'
#' @return Clustered MAgPIE object on requested resolution
#' @author Abhijeet Mishra
#'
#' @examples
#' \dontrun{ calcOutput("ClimateClass", aggregate = FALSE) }
#'
#' @export

calcClimateClass <-function(source="Koeppen"){

  if(source=="Koeppen"){

    x      <- readSource("Koeppen", subtype="cellular",convert = "onlycorrect")

  } else if(grepl("IPCC",source)){

    x <- readSource("IPCCClimate", convert="onlycorrect")
    getNames(x) <- gsub(" ","_", tolower(getNames(x)))

    if(source=="IPCC_reduced"){
      reduceIPCC  <- toolGetMapping("IPCC2IPCCreduced.csv", type = "sectoral")
      x           <- toolAggregate(x,reduceIPCC,from="ipcc",to="ipcc_reduced", dim=3, partrel=TRUE)
    }

  } else { stop("Source unkown.")}

  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven", fao_corr=TRUE, input_magpie=TRUE, years="y1995", round=6), dim=3)

  return(list(
    x=x,
    weight=weight,
    unit="share",
    description="share of koeppen geiger area",
    isocountries=FALSE))
}
