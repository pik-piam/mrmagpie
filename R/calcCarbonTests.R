#' @title calcCarbonTests
#' @description This function extracts carbon densities from LPJ to MAgPIE
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different GCM climate scenarios
#' @param stage Switch for raw data or harmonization
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens, Florian Humpenoeder
#'
#' @examples
#' \dontrun{ calcOutput("CarbonTests", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#' @importFrom magclass add_dimension

calcCarbonTests <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_44ac93de", crop="ggcmi_phase3_nchecks_9ca735cb"),
                           climatetype="GSWP3-W5E5:historical", stage="raw"){

  .getLPJmLCPools <- function(pool, cfg){
    out <- calcOutput("LPJmL_new", version=cfg$lpjml,
                      climatetype=cfg$climatetype,
                      subtype=pool, stage=cfg$stage,
                      aggregate=FALSE)
    out <- toolCoord2Isocell(out)
    out <- setNames(out, pool)
    return(out)
  }

  cfg    <- list(lpjml=lpjml["natveg"], climatetype=climatetype, stage=stage)
  natveg <- mbind(.getLPJmLCPools("vegc", cfg),
                  .getLPJmLCPools("soilc", cfg),
                  .getLPJmLCPools("litc", cfg))

  cfg    <- list(lpjml=lpjml["crop"], climatetype=climatetype, stage=stage)
  grass  <- mbind(.getLPJmLCPools("vegc_grass", cfg),
                  .getLPJmLCPools("soilc_grass", cfg),
                  .getLPJmLCPools("litc_grass", cfg))

  getNames(grass) <- getNames(natveg)

  soilc_layer_natveg <- toolCoord2Isocell(calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype,
                                                     subtype="soilc_layer", stage=stage, aggregate=FALSE))
  topsoilc           <- soilc_layer_natveg[,,1] + 1/3 * soilc_layer_natveg[,,2]

  #topsoilc       <- calcOutput("TopsoilCarbon", lpjml=lpjml, climatetype=climatetype, aggregate=FALSE)
  cshare         <- calcOutput("SOCLossShare", aggregate=FALSE, years="y1995")

  ####################################################
  #Create the output object
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
  carbon_stocks[,,"crop.soilc"]      <- cshare * topsoilc + (natveg[,,"soilc"] - topsoilc)

  carbon_stocks[,,"past"]            <- grass
  grasssoil <- TRUE

    if(dimSums(grass[,,"soilc"]*landuse[,,"past"], dim=c(1,2)) > dimSums(natveg[,,"soilc"]*landuse[,,"past"], dim=c(1,2))){
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
