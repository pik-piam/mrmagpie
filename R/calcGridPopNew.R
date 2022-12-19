#' @title calcGridPopNew
#'
#' @description Past and future (SSP1-5) population based on HYDE3.2 and Jones & O'Neill (2016)
#' Data is scaled to match WDI data from calcPopulation
#' NOTE that some scaling factors for the future (for small countries Gambia and Djibouti) are off,
#' data read in is 50% of WDI data, most likely due to large resolution
#'
#' @param subtype  time horizon to be returned.
#'                 Options: past (1965-2005),
#'                 future (2005-2010) or
#'                 all (divergence starts at year in harmonize_until)
#' @param source   default source (ISIMIP) or
#'                 Gao data (readGridPopGao) which is split into urban and rural.
#' @param cellular if true: half degree grid cell data returned
#' @param cells    number of halfdegree grid cells to be returned.
#'                 Options: "magpiecell" (59199), "lpjcell" (67420)
#' @param FiveYear TRUE for 5 year time steps, otherwise yearly from source
#' @param harmonize_until harmonization year until which SSPs diverge (default: 2015)
#' @param urban    TRUE to return only urban gridded population based on iso share
#'
#' @return Population in millions.
#'
#' @author David Chen, Felicitas Beier
#'
#' @importFrom magclass add_columns collapseNames where
#' @importFrom magpiesets findset
#' @importFrom madrat calcOutput toolGetMapping toolAggregate
#'
#' @export
#'

calcGridPopNew <- function(source = "ISIMIP", subtype = "all", # nolint
                           cellular = TRUE, cells = "magpiecell",
                           FiveYear = TRUE, # nolint
                           harmonize_until = 2015, urban = FALSE) { # nolint

  if (!cellular) (stop("Run calcPopulation instead"))

  # Country-level population data (in million)
  pop <- calcOutput("Population", aggregate = FALSE)

  # Gridded population data
  if (source == "ISIMIP") { # nolint

    ## past data
    if (subtype == "past") {

      # gridded population data
      gridpop <- readSource("GridPopNew", subtype = "past", convert = FALSE)

      # aggregate to country-level and scale to match WDI country-level pop
      agg <- collapseNames(dimSums(gridpop, dim = c("x", "y")))

      # fill missing years
      pop <- time_interpolate(pop, interpolated_year = getYears(agg))

      commonCountries <- intersect(getItems(agg, dim = 1), getItems(pop, dim = 1))
      # Note: 15 countries are dropped because they are not in the spatial
      #       data ("ABW" "AND" "ATA" "BES" "BLM" "BVT" "GIB" "LIE"
      #             "MAC" "MAF" "MCO" "SMR" "SXM" "VAT" "VGB")

      # scaling factor scF applied to every cell
      scF <- (agg[commonCountries, , ] / 1e6) / pop[commonCountries, , "pop_SSP2"]

      gridpop1 <- gridpop[commonCountries, , ] / scF[commonCountries, , ]

      # Some countries have 0 population in grid, but the cells exist, so get filled.
      # even division of population across cells
      missing <- where(scF[commonCountries, , ] == 0)$true$region
      for (i in missing) {
        gridpop1[i, , ] <- 1e6 * pop[i, , "pop_SSP2"] / length(getCells(gridpop[i, , ]))
      }

      # Add scenario dimension and fill with same value
      x <- collapseNames(gridpop1, collapsedim = 2)
      x <- add_columns(x, dim = 3.1, addnm = c(getNames(pop)[1:5]))
      x[, , 2:6] <- x[, , 1]
      x <- (x[, , -1]) / 1e6

      # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1 - doesn't really matter for past
      if ("pop_SSP1" %in% getNames(x) && !("pop_SDP" %in% getNames(x))) {
        combinedSDP <- x[, , "pop_SSP1"]
        for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
          getNames(combinedSDP) <- gsub("SSP1", i, getNames(x[, , "pop_SSP1"]))
          x <- mbind(x, combinedSDP)
        }
      }
      # Add SSP2EU as copy of SSP2 - doesn't really matter for past
      if ("pop_SSP2" %in% getNames(x) && !("pop_SSP2EU" %in% getNames(x))) {
        combinedEU <- x[, , "pop_SSP2"]
        getNames(combinedEU) <- gsub("SSP2", "SSP2EU", getNames(x[, , "pop_SSP2"]))
        x <- mbind(x, combinedEU)
      }
    }

    ## future scenarios
    if (subtype == "future") {

      # Gridded population data for future
      gridpop <- readSource("GridPopNew", subtype = "future", convert = FALSE)

      # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1 - as in calcPopulation
      if ("pop_SSP1" %in% getNames(gridpop) && !("pop_SDP" %in% getNames(gridpop))) {
        combinedSDP <- gridpop[, , "pop_SSP1"]
        for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
          getNames(combinedSDP) <- gsub("SSP1", i, getNames(gridpop[, , "pop_SSP1"]))
          gridpop <- mbind(gridpop, combinedSDP)
        }
      }
      # Add SSP2EU as copy of SSP2 - this will be scaled to SSP2EU - given the lack of grid pop information for SSP2EU
      if ("pop_SSP2" %in% getNames(gridpop) && !("pop_SSP2EU" %in% getNames(gridpop))) {
        combinedEU <- gridpop[, , "pop_SSP2"]
        getNames(combinedEU) <- gsub("SSP2", "SSP2EU", getNames(gridpop[, , "pop_SSP2"]))
        gridpop <- mbind(gridpop, combinedEU)
      }

      # aggregate to country-level and scale to match madrat country-level pop
      agg <- dimSums(gridpop, dim = c("x", "y"))

      pop <- time_interpolate(pop, interpolated_year = getYears(agg))

      commonCountries <- intersect(getItems(agg, dim = 1), getItems(pop, dim = 1))

      # scaling factor scF applied to every cell
      scF <- (agg[commonCountries, , ] / 1e6) / pop[commonCountries, , ]
      gridpop1 <- gridpop[commonCountries, , ] / scF[commonCountries, , ]

      # Some island states have 0 population at grid cell level, but the cells exist, so get filled.
      # the few cells for each island get even division of total pop
      missing <- where(scF[getItems(gridpop, dim = "iso"), , ] == 0)$true$region
      for (i in missing) {
        gridpop1[i, , ] <- 1e6 * pop[i, , ] / length(getCells(gridpop[i, , ]))
      }

      # unit transformation to mio.
      x <- gridpop1 / 1e6
    }

    if (subtype == "all") {
      past   <- calcOutput("GridPopNew", subtype = "past", cells = "lpjcell",
                           aggregate = FALSE, FiveYear = FALSE)
      future <- calcOutput("GridPopNew", subtype = "future", cells = "lpjcell",
                           aggregate = FALSE, FiveYear = FALSE)

      # harmonize future SSPs to divergence year by making them SSP2
      selectY           <- 1:(harmonize_until - min(getYears(future, as.integer = TRUE)) + 1)
      harmY             <- getYears(future, as.integer = TRUE)[selectY]
      future[, harmY, ] <- future[, harmY, "SSP2"]

      x <- mbind(past, future)
    }

    if (FiveYear == TRUE) {
      years <- findset("time")
      x     <- x[, intersect(years, getYears(x)), ]
      x     <- toolHoldConstantBeyondEnd(x)
    }

    getNames(x) <- gsub("pop_", "", getNames(x))

    if (urban) {

      message("This data source urban/rural is diaggregated using uniform country level value. ",
              "Use Gao source instead for cellular urban/rural population")

      # urban population at country-level
      urban           <- calcOutput("Urban", aggregate = FALSE)
      commonYears     <- intersect(getItems(x, dim = 2), getItems(urban, dim = 2))
      urban           <- urban[, commonYears, ]
      x               <- x[, commonYears, ]
      getNames(urban) <- gsub("urb_", "", getNames(urban))

      # disaggregate to cell level
      coordMapping    <- toolGetMappingCoord2Country()
      urban           <- toolAggregate(urban, rel = coordMapping,
                                       from = "iso", to = "coords", partrel = TRUE)
      getCells(urban) <- paste(getItems(urban, dim = 1),
                           coordMapping$iso[coordMapping$coords == getItems(urban, dim = 1)],
                           sep = ".")
      getSets(urban)  <- c("x", "y", "iso", "year", "data")

      x <- x * urban
    }
  } else if (source == "Gao") { # nolint

    if (subtype == "past") {
      stop("Data only available from 2000 onwards, use ISIMIP source instead")
    }

    if (subtype == "future") {

      x <- readSource("GridPopGao", convert = FALSE)

      intYears <- seq(2005, 2095, 10)
      x        <- time_interpolate(x, interpolated_year = intYears,
                                   integrate_interpolated_years = TRUE)

      if (!urban) {
        x <- collapseNames(dimSums(x, dim = 3.2))
      }

      # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1 - as in calcPopulation
      if ("SSP1" %in% getNames(x, dim = 1) && !("SDP" %in% getNames(x, dim = 1))) {
        combinedSDP <- x[, , "SSP1"]
        for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
          getNames(combinedSDP, dim = 1) <- gsub("SSP1", i, getNames(x[, , "SSP1"], dim = 1))
          x <- mbind(x, combinedSDP)
        }
      }
      # Add SSP2EU as copy of SSP2 - this will be scaled to SSP2EU - given the lack of grid pop information for SSP2EU
      if ("SSP2" %in% getNames(x, dim = 1) && !("SSP2EU" %in% getNames(x, dim = 1))) {
        combinedEU <- x[, , "SSP2"]
        getNames(combinedEU, dim = 1) <- gsub("SSP2", "SSP2EU", getNames(x[, , "SSP2"], dim = 1))
        x <- mbind(x, combinedEU)
      }

      # aggregate to country-level and scale to match madrat country-level pop
      agg <- dimSums(x, dim = c("x", "y"))
      commonCountries <- intersect(getItems(agg, dim = 1), getItems(pop, dim = 1))

      getNames(pop) <- gsub("pop_", "", getNames(pop))
      getNames(agg) <- gsub("pop_", "", getNames(agg))
      commonYears   <- intersect(getYears(agg), getYears(pop))
      pop           <- pop[, commonYears, ]
      agg           <- agg[, commonYears, ]
      x             <- x[, commonYears, ]

      # scaling factor scF applied to every cell
      if (urban) {
        scF <- (dimSums(agg[commonCountries, , ], dim = 3.2) / 1e6) / pop[commonCountries, , ]
      } else {
        scF <- (agg[commonCountries, , ] / 1e6) / pop[commonCountries, , ]
      }
      x1  <- x / scF[commonCountries, , ]

      # Some small states have have 0 population in gridded data, but the cells exist, so get filled.
      # the few cells for each island get even division of total pop
      missing <- where(scF[commonCountries, , ] == 0)$true$region
      for (i in missing) {
        x1[i, , ] <- 1e6 * pop[i, , ] / length(getCells(x[i, , ]))
      }

      x <- x1 / 1e6
    }

    if (subtype == "all") {

      past   <- calcOutput("GridPopNew", source = "ISIMIP",
                           subtype = "past", cells = "lpjcell",
                           aggregate = FALSE, FiveYear = FALSE)
      past   <- past[, seq(1965, 2005, 5), ]
      future <- calcOutput("GridPopNew", source = "Gao", urban = TRUE,
                           subtype = "future", cells = "lpjcell",
                           aggregate = FALSE, FiveYear = FALSE)

      if (urban) {

        ratio <- future[, 2000, ] / dimSums(future[, 2000, ], dim = 3.2)
        ratio[is.na(ratio)] <- 0
        # hold past rural urban share constant in each grid for now, based on year 2000
        past <- setYears(ratio, NULL) * past

      } else if (!urban) {
        future <- dimSums(future, dim = 3.2)
      }

      past <- past[, seq(1965, 1995, 5), ]

      # harmonize future SSPs to divergence year by making them SSP2
      harmY             <- getYears(future, as.integer = TRUE)[1:4]
      future[, harmY, ] <- future[, harmY, "SSP2"]
      x <- mbind(past, future)
      x <- toolHoldConstantBeyondEnd(x)
    }

  } else {
    stop("No other source available. Please select ")
  }

  # Reduce number of grid cells to 59199
  if (cells == "magpiecell") {
    x <- toolCoord2Isocell(x, cells = cells)
  }

  # Scale to match country-level data


  # Checks
  if (any(is.na(x))) {
    stop("Function calcGridPopNew returned NAs.")
  }
  if (any(round(x, digits = 4) < 0)) {
    stop("Function calcGridPopNew returned negative values.")
  }

  return(list(x = x,
              weight = NULL,
              unit = "million",
              description = "Population in millions",
              isocountries = FALSE))
}
