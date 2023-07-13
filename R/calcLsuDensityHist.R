#' @title calcLsuDensityHist
#' @description Calculate livestock historical livestock densities
#' @param disagg_type select the disaggregaton weights for biomass production (can be either grassland or livestock)
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' calcOutput("LsuDensityHist")
#' }
calcLsuDensityHist <- function(disagg_type = "grassland") {
  mag_years_past <- findset("past")
  biomass <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "production.dm"][, mag_years_past, "pasture"]
  biomass <- collapseNames(biomass)

  biomass <- toolIso2CellCountries(biomass)
  land <- calcOutput("LanduseInitialisation", cellular = TRUE, nclasses = "nine", aggregate = FALSE)[, mag_years_past, ]
  grassl_land <- land[, , c("past", "range")]
  grassl_land <- setNames(grassl_land, c("pastr", "range"))
  grassl_shares <- setNames(grassl_land[, , "pastr"] / dimSums(grassl_land, dim = 3), "pastr")
  grassl_shares <- add_columns(grassl_shares, addnm = "range", dim = 3.1)
  grassl_shares[, , "range"] <- 1 - grassl_shares[, , "pastr"]
  grassl_shares[is.nan(grassl_shares) | is.infinite(grassl_shares)] <- 0

  mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell", where = "mappingfolder")

  livestock <- setNames(toolCell2isoCell(readSource("GLW3")), "liv_numb")
  livst_split <- livestock * grassl_shares
  livst_split <- collapseNames(livst_split)
  livst_split_ctry <- toolAggregate(livst_split, rel = mapping, to = "iso", from = "celliso")
  livst_share_ctry <- livst_split_ctry[, , "pastr"] / dimSums(livst_split_ctry, dim = 3)
  livst_share_ctry[is.nan(livst_share_ctry) | is.infinite(livst_share_ctry)] <- 0
  livst_share_ctry <- add_columns(livst_share_ctry, addnm = "range", dim = 3.1)
  livst_share_ctry[, , "range"] <- 1 - livst_share_ctry[, , "pastr"]

  if (disagg_type == "livestock") {
    weight <- livst_split
  } else {
    if (disagg_type == "grassland") {
      weight <- grassl_land
    } else {
      stop(paste0("disagg_type ", disagg_type, " is not supported"))
    }
  }


  biomass_split <- biomass * livst_share_ctry
  biomass_split_cell <- toolAggregate(biomass_split, rel = mapping, weight = weight, from = "iso", to = "celliso")

  # removing values above simulation
  lsu_eq <- (8.9 * 365) / 1000 # tDM y-1
  lsus <- biomass_split_cell / lsu_eq
  lsu_ha <- lsus / grassl_land
  lsu_ha[is.nan(lsu_ha) | is.infinite(lsu_ha)] <- 0
  lsu_ha[lsu_ha[, , "range"] > 2.5] <-  2.5
  lsu_ha[lsu_ha[, , "pastr"] > 10] <-  10

  return(list(
    x = lsu_ha,
    weight = grassl_land,
    unit = "LSU/ha",
    description = "Cattle livestock density",
    isocountries = FALSE
  ))
}
