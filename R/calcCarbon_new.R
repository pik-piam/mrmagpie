#' @title calcCarbon_new
#' @description This function extracts carbon densities from LPJ to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("Carbon_new", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass add_dimension

calcCarbon_new <- function(lpjml=c(natveg="LPJmL4", crop="LPJmL5"), climatetype){

  soilc_natveg <-  calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype, subtype="soilc", stage="harmonized", aggregate=FALSE)
  vegc_natveg  <-  calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype, subtype="vegc", stage="harmonized", aggregate=FALSE)
  litc_natveg  <-  calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype, subtype="litc", stage="harmonized", aggregate=FALSE)

  natveg       <- mbind(vegc_natveg, soilc_natveg, litc_natveg)
  rm(vegc_natveg, litc_natveg)

  soilc_grass  <-  calcOutput("LPJmL_new", version=lpjml["crop"], climatetype=climatetype, subtype="soilc_grass", stage="harmonized", aggregate=FALSE)
  vegc_grass   <-  calcOutput("LPJmL_new", version=lpjml["crop"], climatetype=climatetype, subtype="vegc_grass", stage="harmonized", aggregate=FALSE)
  litc_grass   <-  calcOutput("LPJmL_new", version=lpjml["crop"], climatetype=climatetype, subtype="litc_grass", stage="harmonized", aggregate=FALSE)

  grass           <- mbind(vegc_grass, soilc_grass, litc_grass)
  rm(vegc_grass, litc_grass)

  getNames(grass) <- getNames(natveg)

  topsoilc       <- calcOutput("TopsoilCarbon_new", lpjml=lpjml, climatetype=climatetype, aggregate=FALSE)
  #find cshare
  cshare         <- calcOutput("SOCLossShare", aggregate=FALSE, years="y1995")

  ####################################################
  #Create the output file
  ####################################################


  carbon_stocks <- new.magpie(cells_and_regions = getCells(natveg),
                              years = getYears(natveg),
                              names = getNames(natveg))

  carbon_stocks <- add_dimension(carbon_stocks, dim = 3.1, add = "landtype",
                                 nm = c("crop","past","forestry","primforest","secdforest", "urban", "other"))

  landuse <- calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, nclasses="seven", fao_corr=TRUE, input_magpie=TRUE, years="y1995", round=6)

  ####################################################
  #Calculate the appropriate values for all land types and carbon types.
  ####################################################

  #Factor 0.012 is based on the script subversion/svn/tools/carbon_cropland, executed at 30.07.2013
  carbon_stocks[,,"crop.vegc"]       <- 0.012*natveg[,,"vegc"]
  carbon_stocks[,,"crop.litc"]       <- 0 # does not make sense
  carbon_stocks[,,"crop.soilc"]      <- cshare * topsoilc + (soilc_natveg - topsoilc)

  carbon_stocks[,,"past"]            <- grass
  grasssoil <- TRUE
  if(dimSums(soilc_grass*landuse[,,"past"], dim=c(1,2)) > dimSums(soilc_natveg*landuse[,,"past"], dim=c(1,2))){
    # use natveg stock for soilc grassland, for too big soilc stocks in ALLCROP grass runs
    carbon_stocks[,,"past.soilc"]    <- natveg[,,"soilc"]
    grasssoil <- FALSE
  }

  carbon_stocks[,,"forestry"]        <- natveg
  carbon_stocks[,,"primforest"]      <- natveg
  carbon_stocks[,,"secdforest"]      <- natveg
  carbon_stocks[,,"urban"]           <- 0
  carbon_stocks[,,"other"]           <- natveg #or grass?


  # Check for NAs
  if(any(is.na(carbon_stocks))){
    stop("produced NA Carbon")
  }

  landuse <- dimSums(landuse, dim=3)

  return(list(
    x=carbon_stocks,
    weight=landuse,
    unit="t per ha",
    description="Carbon in tons per hectar for different land use types.",
    note = ifelse(grasssoil,"Pasture soil carbon stocks are based on allcrop run.",
                            "Pasture soil carbon stocks are based on natveg run."),
    isocountries=FALSE))
}
