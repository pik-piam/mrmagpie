#' @title calcBphEffect
#' @description Biogeophysical temperature change of afforestation (degree C). File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018
#'
#' @return magpie object in cellular resolution
#' @author Michael Windisch
#'
#' @examples
#' \dontrun{ calcOutput("BphEffect", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom madrat readSource

calcBphEffect <-function(){

  x <- readSource("BphEffect", convert="onlycorrect")
  k <- readSource("Koeppen", subtype="cellular", convert="onlycorrect")[,1995,]
  weight <- dimSums(calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven", fao_corr=TRUE, input_magpie=TRUE, years="y1995", round=6), dim=3)


  #mapping to connect cell names with latitudes
  map <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")

  #assuming 0 was NA before.
  x[,,"ann_bph"][x[,,"ann_bph"]==0] <- NA

  #extract climate class names
  cclass <- getNames(k)

  #select dominating climate class
  ccl <- apply(k,1,function(k) {
    cclass[which(k==max(k))]
  })
  #add to mapping
  map <- cbind(map,ccl)

  #global mean, used in case of NA for all cells for a give lat
  m_glo <- mean(x[,,"ann_bph"],na.rm=TRUE)
  #m_glo <- mean(b,na.rm=FALSE)

  #Loop over climate classes
  for (sel in cclass) {
    #get the magpie cells corresponding to cl
    cells <- map[ccl %in% sel,"celliso"]
    #find cells with NA
    cells_NA <- cells[is.na(x[cells,,"ann_bph"])]
    #If all cells are NA, use m_glo, otherwise calc mean based on the non NA cells.
    if(identical(cells,cells_NA)) {
      x[cells_NA,,"ann_bph"] <- m_glo
    } else {
      m <- mean(x[cells,,"ann_bph"],na.rm=TRUE)
      x[cells_NA,,"ann_bph"] <- m
    }
  }

  return(list(
    x=x,
    weight=weight,
    unit="degC",
    description="Biogeophysical temp change of afforestation in degC",
    isocountries=FALSE))
}
