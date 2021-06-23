#' @title fullCELLULARMAGPIE
#' @description Function that produces the complete cellular data set required for running the
#' MAgPIE model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param ctype aggregation clustering type, which is a combination of a single letter, indicating the cluster methodology, and a number,
#' indicating the number of resulting clusters. Available methodologies are hierarchical clustering (h), normalized k-means clustering
#' (n) and combined hierarchical/normalized k-means clustering (c). In the latter hierarchical clustering is used to determine the
#' cluster distribution among regions whereas normalized k-means is used for the clustering within a region.
#' @param dev development suffix to distinguish development versions for the same data revision. This can be useful to distinguish
#' parallel lines of development.
#' @param climatetype climate change scenario to be used
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param isimip Defines isimip crop model input which replace maiz, tece, rice_pro and soybean
#' @param clusterweight Should specific regions be resolved with more or less detail? Values > 1 mean higher share, < 1 lower share
#' e.g. cfg$clusterweight <- c(LAM=2) means that a higher level of detail for region LAM if set to NULL all weights will be assumed to be 1.
#' examples:
#' c(LAM=1.5,SSA=1.5,OAS=1.5)
#' c(LAM=2,SSA=2,OAS=2)
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Kristine Karstens, Jan Philipp Dietrich
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#' @examples
#' \dontrun{
#' retrieveData("CELLULARMAGPIE", revision = 12, mainfolder = "pathtowhereallfilesarestored")
#' }
#' @importFrom madrat setConfig getConfig
#' @importFrom magpiesets findset
#' @importFrom digest digest

fullCELLULARMAGPIE <- function(rev = 0.1, dev = "", ctype = "c200", climatetype = "MRI-ESM2-0:ssp370",
                               lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                         crop = "ggcmi_phase3_nchecks_9ca735cb"),
                               isimip = NULL, clusterweight = NULL) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  cat(paste0("Start preprocessing for \n climatescenario: ", climatetype,
                 "\n LPJmL-Versions: ", paste(names(lpjml), lpjml, sep = "->", collapse = ", "),
                 "\n clusterweight: ", paste(names(clusterweight), clusterweight, sep = ":", collapse = ", "),
                 "\n isimip yield subtype: ", paste(names(isimip), isimip, sep = ":", collapse = ", ")))

  # Create version tag (will be returned at the very end of this function)
  version_tag <- paste(ctype,
                       gsub(":", "-", climatetype),
                       paste0("lpjml-", digest::digest(lpjml, algo = getConfig("hash"))),
                       sep = "_")
  version_tag <- ifelse(is.null(isimip),
                        version_tag,
                        paste0(version_tag, "_isimip-", digest::digest(isimip, algo = getConfig("hash"))))

  mag_years_past_long  <- c("y1995", "y2000", "y2005", "y2010", "y2015")
  mag_years <- findset("time")
  short_years <- findset("t_all")
  lpj_years <- seq(1995, 2100, by = 5)

  map      <- calcOutput("Cluster", ctype = ctype, weight = clusterweight, lpjml = lpjml,
                         clusterdata = "yield_airrig", aggregate = FALSE)
  weightID <- ifelse(is.null(clusterweight), "", paste0("_", names(clusterweight), clusterweight, collapse = ""))
  clustermapname <- sub("\\.[^.]*$", ".rds", paste0("clustermap_rev", rev, dev, "_", ctype,
                                                    weightID, "_", getConfig("regionmapping")))
  toolStoreMapping(map, clustermapname, type = "regional", where = c("mappingfolder", "outputfolder"),
                   error.existing = FALSE)
  setConfig(extramappings = clustermapname)

  # 14 yields

  calcOutput("Yields", aggregate = FALSE, source = c(lpjml = lpjml[["crop"]], isimip = isimip),
             climatetype = climatetype, round = 2, years = "y1995", file = paste0("lpj_yields_0.5.mz"))

  calcOutput("Yields", aggregate = "cluster", source = c(lpjml = lpjml[["crop"]], isimip = isimip),
             climatetype = climatetype, round = 2, years = lpj_years, file = paste0("lpj_yields_", ctype, ".mz"))

  calcOutput("PastYields_new", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), mowing_events = "me2",
             lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_Nreturn0p5_limN",
             file = paste0("lpj_past_yields_new_", ctype, ".mz"), years = mag_years, aggregate = "cluster")
  calcOutput("LUH2v2",  aggregate = "cluster", landuse_types = "LUH2v2", cellular = TRUE, file =
               paste0("f14_LUH2v2_", ctype, ".mz"))
  calcOutput("PastureSuit",  aggregate = "cluster", subtype = "ISIMIP3b:IPSL-CM6A-LR:1850-2100",
             file = paste0("f14_past_suitability_", ctype, ".mz"), years = mag_years)
  calcOutput("GrassPastureShare", aggregate = "cluster", file = paste0("f14_past_share_", ctype, ".mz"))

  # if(grepl("pasturetest",dev)){
  #
  #   if(grepl("nN",dev)){
  #     calcOutput("PastYields_new", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), mowing_events = "me2", lpjml= "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_noN", file=paste0("lpj_past_yields_new_", ctype, ".mz"), years = mag_years,  aggregate = "cluster")
  #     calcOutput("ScaleEnvironmentData_new", subtype="ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 1, sel_feat = c("tas","pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg"), aggregate="cluster", file=paste0("soilc_ml_environment_scaled_new_", ctype, ".mz"))
  #     calcOutput("ScaleEnvironmentData_new", subtype="ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 1, sel_feat = c("tas","pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg"), aggregate="cluster", file=paste0("environment_scaled_new_", ctype, ".mz"), years = mag_years)
  #     calcOutput("ScaledPastSoilCarbon", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_noN", sar = 1, aggregate="cluster", file=paste0("soilc_ml_stocks_new_", ctype, ".mz"))
  #     calcOutput("SCScalingFactors", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_noN", sar = 1, aggregate=F, file = paste0("SCScalingFactors_", ctype, ".mz"))
  #     calcOutput("CollectSoilCarbonMow", mowing_events = "me2", lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", aggregate ="cluster", scenario = "ssp126_co2_limN", sar = 1,  years = mag_years, file = paste0("soilc_stocks_mow_", ctype, ".mz"))
  #
  #   } else {
  #     calcOutput("PastYields_new", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), mowing_events = "me2", lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_Nreturn0p5_limN", file = paste0("lpj_past_yields_new_", ctype, ".mz"), years = mag_years, aggregate = "cluster")
  #     calcOutput("ScaleEnvironmentData_new", subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 1, sel_feat = c("tas", "pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg"), aggregate = F, file = paste0("soilc_ml_environment_scaled_new_", ctype, ".mz"), years = mag_years)
  #     calcOutput("ScaleEnvironmentData_new", subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:1965-2100", sar = 1, sel_feat = c("tas", "pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg"), aggregate = F, aggr ="cluster", file = paste0("environment_scaled_new_", ctype, ".mz"), years = mag_years)
  #     calcOutput("ScaledPastSoilCarbon", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_Nreturn0p5_limN", sar = 1, aggregate = F, file = paste0("soilc_ml_stocks_new_", ctype, ".mz"), years = mag_years)
  #     calcOutput("SCScalingFactors", lsu_levels = c(seq(0, 2.2, 0.2), 2.5), lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", scenario = "ssp126_co2_Nreturn0p5_limN", sar = 1, aggregate = F, file = paste0("SCScalingFactors_", ctype, ".mz"))
  #     calcOutput("CollectSoilCarbonMow", mowing_events = "me2", lpjml = "lpjml5p2_pasture", climatetype = "IPSL_CM6A_LR", aggregate ="cluster", scenario = "ssp126_co2_Nreturn0p5_limN", sar = 1,  years = mag_years, file = paste0("soilc_stocks_mow_", ctype, ".mz"))
  #   }
  # }

  calcOutput("ClimateClass", aggregate = "cluster", years = "y2015", file = paste0("koeppen_geiger_", ctype, ".mz"))

   # 10 land
  calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, cells = "magpiecell", nclasses = "seven",
             fao_corr = TRUE, input_magpie = TRUE, selectyears = mag_years_past_long, round = 6,
             file = "avl_land_t_0.5.mz")
  calcOutput("LanduseInitialisation", aggregate = "cluster", cellular = TRUE, nclasses = "seven", fao_corr = TRUE,
             input_magpie = TRUE, selectyears = mag_years_past_long, round = 6,
             file = paste0("avl_land_t_", ctype, ".mz"))
  calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, fao_corr = TRUE, input_magpie = TRUE,
             selectyears = mag_years_past_long, round = 6, country_level = TRUE, file = paste0("avl_land_t_iso.cs3"))

  calcOutput("SeaLevelRise", aggregate = FALSE, cellular = TRUE, years = mag_years, round = 6,
             file = "f10_SeaLevelRise_0.5.mz")
  calcOutput("AvlLandSi", aggregate = FALSE, round = 6, file = "avl_land_si_0.5.mz")
  calcOutput("AvlLandSi", aggregate = "cluster", round = 6, file = paste0("avl_land_si_", ctype, ".mz"))

  # 30 crop
  # calcOutput("Croparea", sectoral="kcr", physical=TRUE, cellular=TRUE, irrigation=FALSE,
  #            aggregate = FALSE,file="f30_croparea_initialisation_0.5.mz")
  # calcOutput("Croparea", sectoral="kcr", physical=TRUE, cellular=TRUE, irrigation=TRUE,
  #            aggregate = FALSE,file="f30_croparea_w_initialisation_0.5.mz")
  calcOutput("Croparea", sectoral = "kcr", physical = TRUE, cellular = TRUE, irrigation = FALSE,
             aggregate = "cluster", file = paste0("f30_croparea_initialisation_", ctype, ".mz"))
  calcOutput("Croparea", sectoral = "kcr", physical = TRUE, cellular = TRUE, irrigation = TRUE,
             aggregate = "cluster", file = paste0("f30_croparea_w_initialisation_", ctype, ".mz"))

  calcOutput("AvlCropland", marginal_land = "magpie", cell_upper_bound = 0.9,  aggregate = FALSE, round = 6,
             file = "avl_cropland_0.5.mz")
  calcOutput("AvlCropland", marginal_land = "magpie", cell_upper_bound = 0.9, aggregate = "cluster", round = 6,
             file = paste0("avl_cropland_", ctype, ".mz"))
  calcOutput("AvlCropland", marginal_land = "magpie", cell_upper_bound = 0.9, aggregate = FALSE,
             country_level = TRUE, round = 6, file = paste0("avl_cropland_iso.cs3"))

  # 32 forestry
  calcOutput("AfforestationMask", subtype = "noboreal",     aggregate = "cluster", round = 6,
             file = paste0("aff_noboreal_", ctype, ".mz"))
  calcOutput("AfforestationMask", subtype = "onlytropical", aggregate = "cluster", round = 6,
             file = paste0("aff_onlytropical_", ctype, ".mz"))
  calcOutput("AfforestationMask", subtype = "unrestricted", aggregate = "cluster", round = 6,
             file = paste0("aff_unrestricted_", ctype, ".mz"))

  calcOutput("NpiNdcAdAolcPol", aggregate = "cluster", round = 6, file = paste0("npi_ndc_ad_aolc_pol_", ctype, ".mz"))
  calcOutput("NpiNdcAffPol",    aggregate = "cluster", round = 6, file = paste0("npi_ndc_aff_pol_", ctype, ".mz"))

  calcOutput("BphEffect", aggregate = "cluster", file = paste0("f32_bph_effect_noTCRE_", ctype, ".mz"))
  calcOutput("BphTCRE",   aggregate = "cluster", file = paste0("f32_localTCRE_", ctype, ".mz"))
  calcOutput("BphMask",   aggregate = "cluster", file = paste0("f32_bph_mask_", ctype, ".mz"))

  # 34
  calcOutput("UrbanLandFuture", aggregate = FALSE, round = 6, years = short_years, file = "f34_UrbanLand_0.5.mz")
  calcOutput("UrbanLandFuture", aggregate = "cluster", round = 6, years = short_years,
             file = paste0("f34_UrbanLand_", ctype, ".mz"))

  # 35 natveg
  calcOutput("AgeClassDistribution", aggregate = "cluster", round = 6, file = paste0("forestageclasses_", ctype, ".mz"))
  calcOutput("ProtectArea",          aggregate = "cluster", round = 6, file = paste0("protect_area_", ctype, ".mz"))

  # 37 labour prod
   if (grepl("labourprodtest", dev)) {
     calcOutput("LabourProdImpactEmu", aggregate = "cluster", round = 6,
                file = paste0("f37_labourprodimpact_", ctype, ".mz"))
   }

  # 38 factor costs
  calcOutput("LabourProdImpact", aggregate = FALSE, round = 6, years = short_years, file = "labour_impact.mz")

  # 40
  calcOutput("TransportDistance", aggregate = "cluster", round = 6, file = paste0("transport_distance_", ctype, ".mz"))

  # 41 area equipped for irrigation
  calcOutput("AreaEquippedForIrrigation", aggregate = "cluster", cellular = TRUE, source = "Siebert",
             round = 6, file = paste0("avl_irrig_", ctype, ".mz"))
  calcOutput("AreaEquippedForIrrigation", aggregate = "cluster", cellular = TRUE, source = "LUH2v2",
             selectyears = mag_years_past_long, round = 6, file = paste0("avl_irrig_luh_t_", ctype, ".mz"))


  # 42 water demand
  calcOutput("Irrigation", lpjml = lpjml, years = lpj_years, climatetype = climatetype, aggregate = "cluster",
             round = 6, file = paste0("lpj_airrig_", ctype, ".mz"))

  # dummy Growing Period calc
  calcOutput("GrowingPeriod", lpjml = lpjml, years = lpj_years, climatetype = climatetype, yield_ratio = 0.1,
             aggregate = FALSE, round = 2, file = "lpj_grper_0.5.mz")

  # 43 water availability
  calcOutput("AvlWater", lpjml = lpjml, years = lpj_years, climatetype = climatetype, seasonality = "grper",
             aggregate = "cluster", round = 6, file = paste0("lpj_watavail_grper_", ctype, ".mz"))
  calcOutput("AvlWater", lpjml = lpjml, years = lpj_years, climatetype = climatetype, seasonality = "total",
             aggregate = "cluster", round = 6, file = paste0("lpj_watavail_total_", ctype, ".mz"))


  calcOutput("EnvmtlFlow", lpjml = lpjml, years = lpj_years, climatetype = climatetype, aggregate = "cluster",
             round = 6, seasonality = "grper", file = paste0("lpj_envflow_grper_", ctype, ".mz"))
  calcOutput("WaterUseNonAg", source = "WATCH_ISIMIP_WATERGAP", years = lpj_years, seasonality = "grper", lpjml = lpjml,
             climatetype = climatetype, aggregate = "cluster", file = paste0("watdem_nonagr_grper_", ctype, ".mz"))

  # 44 biodiversity
  calcOutput("Luh2SideLayers", aggregate = "cluster", round = 6, file = paste0("luh2_side_layers_", ctype, ".mz"))
  calcOutput("RRLayer",        aggregate = "cluster", round = 6, file = paste0("rr_layer_", ctype, ".mz"))

  # 50 nitrogen
  calcOutput("AtmosphericDepositionRates", cellular = TRUE, aggregate = FALSE, round = 6,
             file = "f50_AtmosphericDepositionRates_0.5.mz")
  calcOutput("NitrogenFixationRateNatural",               aggregate = FALSE, round = 6,
             file = "f50_NitrogenFixationRateNatural_0.5.mz")

  calcOutput("AtmosphericDepositionRates", cellular = TRUE, aggregate = "cluster", round = 6,
             file = paste0("f50_AtmosphericDepositionRates_", ctype, ".mz"))
  calcOutput("NitrogenFixationRateNatural",               aggregate = "cluster", round = 6,
             file = paste0("f50_NitrogenFixationRateNatural_", ctype, ".mz"))

  calcOutput("Carbon", aggregate = FALSE, lpjml = lpjml, climatetype = climatetype,
             round = 6, years = "y1995", file = "lpj_carbon_stocks_0.5.mz")
  calcOutput("TopsoilCarbon", aggregate = FALSE, lpjml = lpjml, climatetype = climatetype,
             round = 6, years = "y1995", file = "lpj_carbon_topsoil_0.5.mz")

  calcOutput("Carbon", aggregate = "cluster", lpjml = lpjml, climatetype = climatetype,
             round = 6, years = lpj_years, file = paste0("lpj_carbon_stocks_", ctype, ".mz"))
  calcOutput("TopsoilCarbon", aggregate = "cluster", lpjml = lpjml, climatetype = climatetype,
             round = 6, years = lpj_years, file = paste0("lpj_carbon_topsoil_", ctype, ".mz"))

  if (dev == "+cFromSink") {

    calcOutput("Carbon_new", aggregate = "cluster", lpjml = lpjml, climatetype = climatetype, fromFlow = TRUE,
                round = 6, years = lpj_years, file = paste0("lpj_carbon_stocks_new_", ctype, ".mz"))
    calcOutput("TopsoilCarbon_new", aggregate = "cluster", lpjml = lpjml, climatetype = climatetype, fromFlow = TRUE,
                round = 6, years = lpj_years, file = paste0("lpj_carbon_topsoil_new_", ctype, ".mz"))
  }

  # 58 peatland
  calcOutput("Peatland", subtype = "degraded", aggregate = FALSE, round = 6, file = "f58_peatland_degrad_0.5.mz")
  calcOutput("Peatland", subtype = "intact",   aggregate = FALSE, round = 6, file = "f58_peatland_intact_0.5.mz")
  calcOutput("Peatland", subtype = "degraded", aggregate = "cluster", round = 6,
             file = paste0("f58_peatland_degrad_", ctype, ".mz"))
  calcOutput("Peatland", subtype = "intact",   aggregate = "cluster", round = 6,
             file = paste0("f58_peatland_intact_", ctype, ".mz"))


  # 59 som
  calcOutput("SOMinitialsiationPools", aggregate = "cluster", round = 6,
             file = paste0("f59_som_initialisation_pools_", ctype, ".mz"))
  calcOutput("SOCLossShare",           aggregate = "cluster", rate = "loss", round = 6, years = "y1995",
             file = paste0("cshare_released_", ctype, ".mz"))

  ##### AGGREGATION ######

  # create info file
  writeInfo <- function(file, lpjml_data, res_high, res_out, rev) {
    functioncall <- paste(deparse(sys.call(-2)), collapse = "")

    map <- toolGetMapping(type = "regional", name = getConfig("regionmapping"))
    regionscode <- regionscode(map)

    info <- c("lpj2magpie settings:",
              paste("* LPJmL data:", lpjml_data),
              paste("* Revision:", rev),
              "", "aggregation settings:",
              paste("* Input resolution:", res_high),
              paste("* Output resolution:", res_out),
              paste("* Regionscode:", regionscode),
              paste("* Call:", functioncall))
    base::cat(info, file = file, sep = "\n")
  }
  writeInfo(file = paste0(getConfig("outputfolder"), "/info.txt"), lpjml_data = climatetype,
            res_high = "0.5", res_out = ctype, rev = rev)

  return(list(tag = version_tag))

}
