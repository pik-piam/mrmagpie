#' @title calcPastrTauHist
#' @description Calculates managed pastures Tau based on FAO yield trends for 1995.
#' @param past_mngmt Pasture management reference yield
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' calcOutput("PastrTauHist", past_mngmt)
#' }
#'
calcPastrTauHist <- function(past_mngmt = "2me") {
  past <- findset("past")
  # Production
  prod <- calcOutput("GrasslandBiomass", aggregate = F)[, past, "pastr"]
  prod <- toolCountryFill(prod, fill = 0)

  # areas
  pastr_weight <- calcOutput("PastureSuit",
    subtype = paste("ISIMIP3bv2", "MRI-ESM2-0", "1850_2100", sep = ":"), aggregate = F
  )[, past, 1]
  # regional mapping
  cell2reg <- toolGetMapping("CountryToCellMapping.csv", type = "cell")

  # pasture areas
  area <- calcOutput("LUH2v2", landuse_types = "LUH2v2", cellular = F, aggregate = F)[, past, "pastr"]
  area <- toolCountryFill(area, fill = 0)

  # Adding 'otherland' as an extra source of grass biomass comparable to managed pastures in India, Pakistan and Bangladesh.
  otherland <- calcOutput("LUH2v2", landuse_types = "LUH2v2", cellular = F, aggregate = F)[, past, c("secdn", "primn")]
  area["IND", , "pastr"] <- area["IND", , "pastr"] + setNames(dimSums(otherland["IND", , ], dim = 3), "pastr")
  area["BGD", , "pastr"] <- area["BGD", , "pastr"] + setNames(dimSums(otherland["BGD", , ], dim = 3), "pastr")
  area["PAK", , "pastr"] <- area["PAK", , "pastr"] + setNames(dimSums(otherland["PAK", , ], dim = 3), "pastr")

  # Actual yields
  yact <- prod[, past, ] / area[, past, ]
  yact[is.nan(yact) | is.infinite(yact)] <- 0

  # reference yields
  yref <- calcOutput("GrasslandsYields",
    lpjml = "lpjml5p2_pasture", climatetype = "MRI-ESM2-0:ssp245", subtype = "/co2/Nreturn0p5",
    lsu_levels = c(seq(0, 2.2, 0.2), 2.5), past_mngmt = past_mngmt,
    aggregate = F
  )[, past, "pastr.rainfed"]

  yref_weights <- calcOutput("LUH2v2", landuse_types = "LUH2v2", cellular = T, aggregate = F)[, past, "pastr"]
  yref <- toolAggregate(yref, rel = cell2reg, from = "celliso", to = "iso", weight = yref_weights)
  yref <- toolCountryFill(yref, fill = 0)

  # tau calculation
  t <- yact[, past, ] / yref[, past, ]
  t[is.nan(t) | is.infinite(t)] <- 0
  t <- collapseNames(t)

  # replacing unrealistic high tau values by regional averages
  reg_map <- toolGetMapping("regionmappingH12.csv", type = "cell")
  t_reg <- toolAggregate(t, rel = reg_map, weight = area, from = "CountryCode", to = "RegionCode")
  regions <- reg_map$RegionCode
  names(regions) <- reg_map[, "CountryCode"]

  largeTC <- where(t >= 10)$true$individual # tau threshold
  colnames(largeTC)[1] <- "country"
  largeTC <- as.data.frame(largeTC)

  for (i in as.vector(largeTC[, "country"])) {
    for (j in as.vector(largeTC[largeTC$country == i, "year"])) {
      t[i, j, ] <- t_reg[regions[i], j, ]
    }
  }

  return(list(
    x = t,
    weight = yref * area, # Xref
    unit = "1",
    min = 0,
    max = 10,
    description = "Historical trends in managed pastures land use intensity (Tau) based on FAO yield trends"
  ))
}
