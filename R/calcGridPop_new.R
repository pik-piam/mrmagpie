#' calcGridPop_new
#'
#' @description Past and future (SSP1-5) population based on HYDE3.2 and Jones & O'Neill (2016)
#' Data is scaled to match WDI data from calcPopulation
#' NOTE that some scaling factors for the future (for small countries Gambia and Djibouti) are off,
#' data read in is 50% of WDI data, most likely due to large resolution
#' @param subtype past (1965-2005), future (2005-2010) or all (divergence starts at year in harmonize_until)
#' @param source  default source or Gao data (readGridPopGao) which is split into urban and rural.
#' @param cellular only cellular
#' @param FiveYear TRUE for 5 year time steps, otherwise yearly from source
#' @param harmonize_until 2015 default divergence of SSPs
#' @param urban TRUE to return only urban gridded population based on iso share
#' @return Population in millions.
#' @author David Chen
#' @importFrom magclass add_columns collapseNames
#' @importFrom magpiesets findset
#' @importFrom madrat calcOutput toolGetMapping toolAggregate


calcGridPop_new <- function(source = "ISIMIP", subtype="all", cellular=TRUE, FiveYear=TRUE, harmonize_until=2015, urban = FALSE) {
  if (!cellular)(stop("Run calcPopulation instead"))

  if (source=="ISIMIP") {
  ##past
  if (subtype=="past") {

    gridpop <- readSource("GridPop_new", subtype="past", convert=F)
    CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
    agg   <- toolAggregate(gridpop, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)

    ## scale to match WDI country-level pop

    pop <- calcOutput("Population", aggregate=F)
    pop <- time_interpolate(pop, interpolated_year=getYears(agg))

    #scaling factor sc_f applied to every cell
    sc_f <- (agg/1e6)/pop[getRegions(agg),,"pop_SSP2"]

    gridpop1 <- gridpop/sc_f[getRegions(gridpop),,]

    #mauritius and ATF have 0, but the cells exist, so get filled.
    #the single MUS cell gets the pop, the ATF cells get even division of pop
    gridpop1["MUS",,] <- 1e6*pop["MUS",,"pop_SSP2"]
    gridpop1["ATF",,] <- 1e6*pop["ATF",,"pop_SSP2"]/length(getCells(gridpop["ATF",,]))
    gridpop1["SGS",,] <- 1e6*pop["SGS",,"pop_SSP2"]/length(getCells(gridpop["SGS",,]))
    x <- collapseNames(gridpop1,collapsedim=2)
    x <- add_columns(x, dim=3.1,addnm=c(getNames(pop)[1:5]))
    x[,,2:6] <- x[,,1]
    x <- (x[,,-1])/1e6

    # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1 - doesn't really matter for past
    if("pop_SSP1" %in% getNames(x) && !("pop_SDP" %in% getNames(x))){
      combined_SDP <- x[,, "pop_SSP1"]
      for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
        getNames(combined_SDP) <- gsub("SSP1", i, getNames(x[,, "pop_SSP1"]))
        x <- mbind(x, combined_SDP)
      }
    }
    # Add SSP2EU as copy of SSP2 - doesn't really matter for past
    if("pop_SSP2" %in% getNames(x) && !("pop_SSP2EU" %in% getNames(x))){
      combined_EU <- x[,, "pop_SSP2"]
     getNames(combined_EU) <- gsub("SSP2", "SSP2EU", getNames(x[,, "pop_SSP2"]))
        x <- mbind(x, combined_EU)
      }
  }

  ##future
  if (subtype=="future"){

    gridpop <- readSource("GridPop_new",subtype="future",convert=F)

    # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1 - as in calcPopulation
    if("pop_SSP1" %in% getNames(gridpop) && !("pop_SDP" %in% getNames(gridpop))){
      combined_SDP <- gridpop[,, "pop_SSP1"]
      for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
        getNames(combined_SDP) <- gsub("SSP1", i, getNames(gridpop[,, "pop_SSP1"]))
        gridpop <- mbind(gridpop, combined_SDP)
      }
    }
    # Add SSP2EU as copy of SSP2 - this will be scaled to SSP2EU - given the lack of grid pop information for SSP2EU
    if("pop_SSP2" %in% getNames(gridpop) && !("pop_SSP2EU" %in% getNames(gridpop))){
      combined_EU <- gridpop[,, "pop_SSP2"]
      getNames(combined_EU) <- gsub("SSP2", "SSP2EU", getNames(gridpop[,, "pop_SSP2"]))
      gridpop <- mbind(gridpop, combined_EU)
    }

    CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
    agg   <- toolAggregate(gridpop, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)

    ## scale to match madrat country-level pop
    pop <- calcOutput("Population",aggregate=F)
    pop <- time_interpolate(pop, interpolated_year=getYears(agg))

    #scaling factor sc_f applied to every cell
    sc_f <- (agg/1e6)/pop[getRegions(agg),,]
    gridpop1 <- gridpop/sc_f[getRegions(gridpop),,]

    #ATF, SJM, SGS have 0, but the cells exist, so get filled.
    #the few cells for each island get even division of total pop
    for (i in c("SGS","ATF","SJM")){
      gridpop1[i,,] <- 1e6*pop[i,,]/length(getCells(gridpop[i,,]))
    }

    x <- gridpop1/1e6
  }

  if (subtype=="all"){
    past <- calcOutput("GridPop_new",subtype="past", aggregate=F, FiveYear=F)
    future <- calcOutput("GridPop_new", subtype="future", aggregate=F, FiveYear=F)

    #harmonize future SSPs to divergence year by making them SSP2
    harm_y <- getYears(future, as.integer = T)[1:(harmonize_until-min(getYears(future,as.integer=T))+1)]
    future[,harm_y,] <- future[,harm_y,"SSP2"]
    x <- mbind(past,future)
  }

  if (FiveYear==TRUE){
    years <- findset("time")
    x <- x[,intersect(years,getYears(x)),]
    x <- toolHoldConstantBeyondEnd(x)
  }

getNames(x) <- gsub("pop_", "", getNames(x))

  if (urban){

    vcat(verbosity=1, "note for this data source urban/rural is diaggregated using uniform country level value. Use
         Gao source instead for cellular urban/rural population")

    urban <- calcOutput("Urban", aggregate=F)[,getYears(x),]
    getNames(urban) <- gsub("pop_","",getNames(urban))

    mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")

    urban <- toolAggregate(urban, rel=mapping, from="iso", to="celliso", partrel=T)

    x <- x * urban
   }
}

else if (source == "Gao") {

    if (subtype == "past") {stop("Data only available from 2000 onwards, use ISIMIP source instead")}

    if (subtype == "future") {

      x <- readSource("GridPopGao", convert=F)

      int_years <- seq(2005,2095,10)

      x <- time_interpolate(x, interpolated_year = int_years, integrate_interpolated_years = TRUE)

      if (!urban) {x <- collapseNames(dimSums(x, dim=3.2))}

      # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1 - as in calcPopulation
      if("SSP1" %in% getNames(x, dim=1) && !("SDP" %in% getNames(x, dim=1))){
        combined_SDP <- x[,, "SSP1"]
        for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
          getNames(combined_SDP, dim=1) <- gsub("SSP1", i, getNames(x[,, "SSP1"], dim=1))
          x <- mbind(x, combined_SDP)
        }
      }
      # Add SSP2EU as copy of SSP2 - this will be scaled to SSP2EU - given the lack of grid pop information for SSP2EU
      if("SSP2" %in% getNames(x, dim=1) && !("SSP2EU" %in% getNames(x, dim=1))){
        combined_EU <- x[,,"SSP2"]
        getNames(combined_EU, dim=1) <- gsub("SSP2", "SSP2EU", getNames(x[,, "SSP2"], dim=1))
        x <- mbind(x, combined_EU)
      }

      CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
      agg   <- toolAggregate(x, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)

      ## scale to match madrat country-level pop
      pop <- calcOutput("Population",aggregate=F)
      getNames(pop) <- gsub("pop_", "", getNames(pop))
      pop <- pop[,getYears(agg), ]

      #scaling factor sc_f applied to every cell
      sc_f <- (dimSums(agg,dim=3.2)/1e6)/pop[getRegions(agg),,]
      x1 <- x/sc_f[getRegions(x),,]

      #ATF, SJM, SGS have 0, but the cells exist, so get filled.
      #the few cells for each island get even division of total pop
      for (i in c("SGS","ATF","SJM")){
        x1[i,,] <- 1e6*pop[i,,]/length(getCells(x[i,,]))
      }

      x <- x1/1e6
    }

  if (subtype == "all") {


    past <- calcOutput("GridPop_new",source = "ISIMIP", subtype="past", aggregate=F, FiveYear=F)
    past <- past[,seq(1965,2005,5),]
    future <- calcOutput("GridPop_new", source="Gao", urban = TRUE, subtype="future", aggregate=F, FiveYear=F)

    if(urban){
    ratio <- future[,2000,]/dimSums(future[,2000,], dim=3.2)
    ratio[is.na(ratio)] <- 0
    #hold past rural urban share constant in each grid for now, based on year 2000
    past <- setYears(ratio,NULL) * past
    } else if(!urban) {
    future <- dimSums(future, dim=3.2)
    }

    past <- past[,seq(1965,1995,5),]

    #harmonize future SSPs to divergence year by making them SSP2
    harm_y <- getYears(future, as.integer = T)[1:4]
    future[,harm_y,] <- future[,harm_y,"SSP2"]
    x <- mbind(past,future)

   x <- toolHoldConstantBeyondEnd(x)

  }

}

  else { (stop("No other source available"))}

    return(list(x=x,
              weight=NULL,
              unit="million",
              description="Population in millions",
              isocountries=FALSE))
}
