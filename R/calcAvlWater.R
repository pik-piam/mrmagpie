#' @title calcAvlWater
#' @description This function calculates water availability for MAgPIE retrieved from LPJmL
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios (default: "CRU_4")
#' @param stage Degree of processing: raw, smoothed, harmonized, harmonized2020
#' @param seasonality grper (default): water available in growing period per year; total: total water available throughout the year; monthly: monthly water availability (for further processing, e.g. in calcEnvmtlFlow)
#'
#' @import magclass
#' @import madrat
#' @importFrom mrcommons toolHarmonize2Baseline
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Kristine Karstens, Abhijeet Mishra
#'
#' @examples
#' \dontrun{ calcOutput("AvlWater", aggregate = FALSE) }
#'

calcAvlWater <- function(lpjml=c(natveg="LPJmL4_for_MAgPIE_84a69edd", crop="ggcmi_phase3_nchecks_72c185fa"),
                         climatetype="GSWP3-W5E5:historical", stage="harmonized2020", seasonality="grper"){

  ##### CONFIG #####
  baseline_hist <- "GSWP3-W5E5:historical"
  ref_year_hist <- "y2010"
  baseline_gcm  <- "GFDL-ESM4:ssp370"
  ref_year_gcm  <- "y2020"
  ##### CONFIG #####


  ######################################################
  ############ Water availability per cell #############
  # Runoff is distributed across the river basin cells #
  # based on discharge-weighted algorithm              #
  ######################################################
  if(stage%in%c("raw","smoothed")){

    ### Monthly Discharge (unit (after calcLPJmL): mio. m^3/month)
    monthly_discharge_magpie <- toolCoord2Isocell(calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype,
                                                             subtype="mdischarge", aggregate=FALSE, stage="raw"))
    # Transform to array (faster calculation)
    monthly_discharge_magpie <- as.array(collapseNames(monthly_discharge_magpie))

    ### Monthly Runoff (unit (after calcLPJmL): mio. m^3/month)
    monthly_runoff_magpie    <- toolCoord2Isocell(calcOutput("LPJmL_new", version=lpjml["natveg"], climatetype=climatetype,
                                                             subtype="mrunoff", aggregate=FALSE, stage="raw"))
    # Transform to array (faster calculation)
    monthly_runoff_magpie    <- as.array(collapseNames(monthly_runoff_magpie))

    ### Calculate available water per month (avl_water_month)
    # Empty array
    avl_water_month     <- monthly_runoff_magpie
    avl_water_month[,,] <- NA

    ## River basin water allocation algorithm:
    # River basin information
    basin_code <- toolGetMapping("rivermapping.csv",type="cell")
    basin_code <- basin_code$basincode

    # Sum the runoff in all basins and allocate it to the basin cells with discharge as weight
    for(basin in unique(basin_code)){
      basin_cells     <- which(basin_code==basin)
      basin_runoff    <- colSums(monthly_runoff_magpie[basin_cells,,,drop=FALSE])
      basin_discharge <- colSums(monthly_discharge_magpie[basin_cells,,,drop=FALSE])
      for(month in dimnames(avl_water_month)[[3]]){
        avl_water_month[basin_cells,,month] <- t(basin_runoff[,month]*t(monthly_discharge_magpie[basin_cells,,month])/basin_discharge[,month])
      }
    }
    # Remove no longer needed objects
    rm(basin_discharge,basin_runoff)

    # avl_water_month contain NA's wherever basin_discharge was 0 -> Replace NA's by 0
    avl_water_month[is.nan(avl_water_month)] <- 0
    avl_water_month <- as.magpie(avl_water_month)

    if(stage=="smoothed") avl_water_month <- toolSmooth(avl_water_month)

    #######################
    ##### Aggregation #####
    #######################
    ### Available water per cell per month
    if(seasonality=="monthly") {
      # Check for NAs
      if(any(is.na(avl_water_month))) {
        stop("produced NA water availability")
      }
      out=avl_water_month

      ### Total water available per cell per year
    } else if(seasonality=="total") {
      # Sum up over all month:
      avl_water_total <- dimSums(avl_water_month, dim=3)
      # Check for NAs
      if(any(is.na(avl_water_total))){
        stop("produced NA water availability")
      }
      out=avl_water_total

      ### Water available in growing period per cell per year
    } else if (seasonality=="grper") {
      # magpie object with days per month with same dimension as avl_water_month
      tmp <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      month_days     <- new.magpie(names=dimnames(avl_water_month)[[3]])
      month_days[,,] <- tmp
      month_day_magpie     <- as.magpie(avl_water_month)
      month_day_magpie[,,] <- 1
      month_day_magpie     <- month_day_magpie * month_days

      # Daily water availability
      avl_water_day <- avl_water_month/month_day_magpie

      # Growing days per month
      grow_days <- calcOutput("GrowingPeriod", lpjml=lpjml, climatetype=climatetype,
                              stage=stage, yield_ratio=0.1, aggregate=FALSE)

      # Adjust years
      years_wat <- getYears(avl_water_day)
      years_grper  <- getYears(grow_days)
      if(length(years_wat)>=length(years_grper)){
        years <- years_grper
      } else {
        years <- years_wat
      }
      rm(years_grper, years_wat)

      # Available water in growing period per month
      avl_water_grper <- avl_water_day[,years,]*grow_days[,years,]
      # Available water in growing period per year
      avl_water_grper <- dimSums(avl_water_grper, dim=3)

      # Check for NAs
      if(any(is.na(avl_water_grper))){
        stop("produced NA water availability")
      }
      out=avl_water_grper
    } else {
      stop("Please specify seasonality: monthly, total or grper")
    }

  }  else if(stage=="harmonized"){

    if(climatetype == baseline_hist) stop("You can not harmonize the historical baseline.")

    # load smoothed data
    baseline <- calcOutput("AvlWater", lpjml=lpjml, climatetype=baseline_hist, seasonality=seasonality,
                           aggregate=FALSE, stage="smoothed")
    x        <- calcOutput("AvlWater", lpjml=lpjml, climatetype=climatetype, seasonality=seasonality,
                           aggregate=FALSE, stage="smoothed")
    # Harmonize to baseline
    out <- toolHarmonize2Baseline(x=x, base=baseline, ref_year=ref_year_hist)

  } else if(stage == "harmonized2020"){

    #read in historical data for subtype
    baseline2020 <- calcOutput("AvlWater", lpjml=lpjml, climatetype=baseline_gcm, seasonality=seasonality,
                               aggregate=FALSE, stage="harmonized")

    if(climatetype == baseline_gcm){
      out <- baseline2020

    } else {

      x        <- calcOutput("AvlWater", lpjml=lpjml, climatetype=climatetype, seasonality=seasonality,
                             aggregate=FALSE, stage="smoothed")
      out      <- toolHarmonize2Baseline(x, baseline2020, ref_year=ref_year_gcm)
    }

  } else { stop("Stage argument not supported!") }

  description=paste0("Available water in ", seasonality)

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description=description,
    isocountries=FALSE))
}
