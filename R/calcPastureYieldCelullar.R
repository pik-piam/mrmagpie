#' Calculate Pasture Yields
#'
#' Provides Pasture yields defined as ratio of grazed biomass to grazed area
#' @param max_yields Maximum yields in tDM/ha allowed in a cell.
#' @param max_iter Maximum number of iterations of the disaggregation algorithm
#' @description `max_yields` and `max_iter` are only affecting the calculations if `cellular` is TRUE.
#' @return Pasture yields and corresponding weights as a list of
#' two MAgPIE objects
#' @author Marcos Alves
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}},
#' \code{\link{readSource}}
#' @examples
#' \dontrun{
#' calcOutput("PastureYield")
#' }
#' @importFrom stats quantile

calcPastureYieldCelullar <- function(max_yields = 20, max_iter = 30) {
  mag_years_past <- findset("past")[c(7, 8, 9, 10)]
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

  mapping <- toolGetMapping(name = "CountryToCellMapping.csv", type = "cell")

  livestock <- setNames(toolCell2isoCell(readSource("GLW3")), "liv_numb")
  livst_split <- livestock * grassl_shares
  livst_split <- collapseNames(livst_split)
  livst_split_ctry <- toolAggregate(livst_split, rel = mapping, to = "iso", from = "celliso")
  livst_share_ctry <- livst_split_ctry[, , "pastr"] / dimSums(livst_split_ctry, dim = 3)
  livst_share_ctry[is.nan(livst_share_ctry) | is.infinite(livst_share_ctry)] <- 0
  livst_share_ctry <- add_columns(livst_share_ctry, addnm = "range", dim = 3.1)
  livst_share_ctry[, , "range"] <- 1 - livst_share_ctry[, , "pastr"]

  # I am splitting biomass consumption assuming the share
  # between animals reared on rangelands and pastures correlates linearly
  # with the production of grass in pastures and rangelands in a country. That can be
  # derived by the fact that the feedbaskets assume the same productivity (feed ingreedients shares)
  # within a country.

  biomass_split <- biomass * livst_share_ctry
  biomass_split_cell <- toolAggregate(biomass_split, rel = mapping, weight = livst_split, from = "iso", to = "celliso")
  grassl_land_ctry <- toolAggregate(grassl_land, rel = mapping, to = "iso", from = "celliso")

  pstr_yield <- biomass_split / grassl_land_ctry
  pstr_yield[is.nan(pstr_yield) | is.infinite(pstr_yield)] <- 0
  pstr_yield_cell <- toolAggregate(pstr_yield, weight = dimSums(land, dim = 3), rel = mapping, from = "iso", to = "celliso")


  # Cluster glassland types and biomass areas into country

  cellular_yield_calc <- function(biomass_split_cell, grassl_land, livst_split, mapping, grassl_thres, max_yields, max_iter) {
    m <- cbind(getNames(biomass_split_cell), getNames(grassl_land), getNames(livst_split))
    check_names <- sum(rowSums(m == m[, 1]) - ncol(m))
    if (check_names != 0) {
      stop("Biomass, grassland and livestock datanames differ. Check magpie object dimention names for details")
    }
    print(paste0("#--------- Report ---------#"))

    print(paste0("Grassland removed: ", sum(grassl_land[grassl_land < grassl_thres])))
    grassl_land[grassl_land < grassl_thres] <- 0
    bin_grassl_land <- grassl_land
    bin_grassl_land[bin_grassl_land >= grassl_thres] <- 1
    biomass_split_cell <- toolAggregate(biomass_split, rel = mapping, weight = livst_split, from = "iso", to = "celliso")
    pstr_yield <- biomass_split_cell / grassl_land
    pstr_yield[is.nan(pstr_yield)] <- 0
    pstr_yield[is.infinite(pstr_yield)] <- 0
    iter <- 1
    iso_id <- unique(mapping$iso)
    iso_count <- setNames(rep(0, length(iso_id)), iso_id)
    croped_livst_split <- livst_split * bin_grassl_land

    while (max(pstr_yield) > max_yields && iter < max_iter) {
      print(paste0("Iteration: ", iter + 1))
      print(paste0("       ", "           ", "old max", "       --> ", "new max"))
      for (iso in iso_id) {
        for (dim in getNames(pstr_yield)) {
          tmp_pstr_yield <- pstr_yield[iso, , dim]
          if (max(tmp_pstr_yield) > max_yields) {
            tmp_pstr_yield[tmp_pstr_yield < max_yields] <- 0
            tmp_exc_biomass <- grassl_land[iso, , dim] * tmp_pstr_yield
            tmp_exc_biomass_reg <- dimSums(tmp_exc_biomass, dim = 1)
            redstr_exc_biomass <- toolAggregate(tmp_exc_biomass_reg, rel = mapping[grepl(iso, mapping$celliso), ], weight = croped_livst_split[iso, , dim], from = "iso", to = "celliso")
            tmp_redstr_biomass <- biomass_split_cell[iso, , dim] - tmp_exc_biomass + redstr_exc_biomass
            tmp_caped_yields <- tmp_redstr_biomass / grassl_land[iso, , dim]
            tmp_caped_yields[is.nan(tmp_caped_yields)] <- 0
            tmp_caped_yields[is.infinite(tmp_caped_yields)] <- 0
            if (iso_count[iso] > 10 && abs((max(tmp_caped_yields) - max(tmp_pstr_yield)) / max(tmp_pstr_yield)) <= 0.05 && max(tmp_caped_yields) > max_yields) {
              tmp_caped_yields[tmp_caped_yields > max_yields] <- max_yields
            }
            pstr_yield[iso, , dim] <- tmp_caped_yields
            biomass_split_cell[iso, , dim] <- tmp_redstr_biomass
            iso_count[iso] <- iso_count[iso] + 1
            print(paste0(iso, ": ", dim, "      ", max(tmp_pstr_yield), " --> ", max(tmp_caped_yields)))
          }
        }
      }
      cat(paste0("\n Quantiles:\n"))
      print(quantile(pstr_yield, c(0.95, 0.98, 0.99)))
      cat(paste0("\n"))
      cat(paste0("Total biomass: ", sum(biomass_split_cell), " | Total biomass loss: ", sum(pstr_yield * grassl_land) - sum(biomass_split_cell)), "\n")
      print(paste0("--------------------------------------------"))
      cat(paste0("\n"))
      iter <- iter + 1
    }
    if (max(pstr_yield) > max_yields) {
      pstr_yield[pstr_yield > max_yields] <- max_yields
      cat(paste0("Optmization finished before all cells had yields samaller than ", max_yields, " tDM/ha/y. Non-compliante cells were croped down to the maximum yield \n \n"))
      cat(paste0("Total biomass: ", sum(biomass_split_cell), " Total biomass loss: ", sum(pstr_yield * grassl_land) - sum(biomass_split_cell)), "\n")
    }
    print("#--------- END ---------#")

    return(list(pstr_yield, abs(pstr_yield * grassl_land - biomass_split_cell)))
  }
  pstr_yield <- cellular_yield_calc(biomass_split_cell, grassl_land, livst_split, mapping, grassl_thres = 0.00001, max_yields = max_yields, max_iter = max_iter)[[1]]
  pstr_yield <- toolCell2isoCell(pstr_yield)

  return(list(
    x = pstr_yield,
    weight = grassl_land,
    isocountries = FALSE,
    unit = "ton DM per ha",
    description = "Pasture yields"
  ))
}
