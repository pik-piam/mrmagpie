#' @title fullCELLULARMAGPIE
#' @description Function that produces the complete cellular data set required for running the
#' MAgPIE model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param ctype aggregation clustering type, which is a combination of a single letter, indicating the cluster
#' methodology, and a number, indicating the number of resulting clusters. Available methodologies are hierarchical
#' clustering (h), normalized k-means clustering (n) and combined hierarchical/normalized k-means clustering (c).
#' In the latter hierarchical clustering is used to determine the cluster distribution among regions whereas
#' normalized k-means is used for the clustering within a region.
#' @param dev development suffix to distinguish development versions for the same data revision.
#' This can be useful to distinguish parallel lines of development.
#' @param climatetype Global Circulation Model to be used
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param isimip Defines isimip crop model input which replace maiz, tece, rice_pro and soybean
#' @param emu_id Pasture Soil carbon emulator ID
#' @param clusterweight Should specific regions be resolved with more or less detail? Values > 1 mean higher share,
#' < 1 lower share e.g. cfg$clusterweight <- c(LAM=2) means that a higher level of detail for region LAM if set to NULL
#' all weights will be assumed to be 1.
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
#' @importFrom stringr str_split

fullCELLULARMAGPIE <- function(rev = 0.1, dev = "",
                               ctype = "c200",
                               climatetype = "MRI-ESM2-0:ssp370",
                               lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                                         crop = "ggcmi_phase3_nchecks_9ca735cb",
                                         grass = "lpjml5p2_pasture"),
                               isimip = NULL,
                               clusterweight = NULL,
                               emu_id = NULL) {

  sizelimit <- getOption("magclass_sizeLimit")
  options(magclass_sizeLimit = 1e+12)
  on.exit(options(magclass_sizeLimit = sizelimit))

  ### Version settings ###
  if (rev < 4.63) stop("mrmagpie(>= 1.14.0) does not support revision below 4.63 anymore.
                       Please use a older snapshot/version of the library, if you need older revisions.")



  climatemodel <- str_split(climatetype, ":")[[1]][1]
  scenario <- str_split(climatetype, ":")[[1]][2]

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
                        paste0(version_tag, "_isimip-",
                               digest::digest(isimip, algo = getConfig("hash"))))
  version_tag <- ifelse(is.null(clusterweight),
                        version_tag,
                        paste0(version_tag, "_clusterweight-",
                               digest::digest(clusterweight, algo = getConfig("hash"))))
  version_tag <- ifelse(is.null(emu_id),
                        version_tag,
                        paste0(version_tag, "_gsoilc-", emu_id))


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
    climatetype = climatetype, round = 2, years = lpj_years, file = paste0("lpj_yields_0.5.mz"),
    weighting = ifelse(grepl("YieldWeights_", dev), gsub("YieldWeights_", "", dev), "totalCrop"))

  calcOutput("Yields", aggregate = "cluster", source = c(lpjml = lpjml[["crop"]], isimip = isimip),
    climatetype = climatetype, round = 2, years = lpj_years, file = paste0("lpj_yields_", ctype, ".mz"),
    weighting = ifelse(grepl("YieldWeights_", dev), gsub("YieldWeights_", "", dev), "totalCrop"))

  if (dev == "+calibYield") {

    calcOutput("YieldsCalibrated", aggregate = "cluster", source = c(lpjml = lpjml[["crop"]], isimip = isimip),
               climatetype = climatetype, round = 2, years = lpj_years, file = paste0("lpj_yields_", ctype, ".mz"))
  }

  if (grepl("MPPA", dev)) {
    #--- Root folder name: currently expresses the ismip version ---#
    version_isimip <- "ISIMIP3b"

    #--- Module 14_yields: Grasslands / LHU2 for yield calibration  ---#
    calcOutput("GrasslandsYields",
               subtype = paste(lpjml[["grass"]],
                               paste0(paste(climatemodel, scenario, sep = ":"),
                                      "/co2/Nreturn0p5/limN"), sep = ":"),
               lsu_levels = c(seq(0, 2.2, 0.2), 2.5), past_mngmt = "me2",
               file = paste0("f14_grassl_yld_", ctype, ".mz"), years = mag_years, aggregate = "cluster")

    calcOutput("LUH2v2", aggregate = "cluster", landuse_types = "LUH2v2", cellular = TRUE,
               file = paste0("fm_LUH2v2_", ctype, ".mz"))

    #--- Module 31_past: suitable managed pasture areas ---#
    calcOutput("PastureSuit",  subtype = paste(version_isimip, climatemodel, "1850_2100", sep = ":"),
               file = paste0("f31_pastr_suitability_", ctype, ".mz"), years = mag_years, aggregate = "cluster")

    #--- Post-processing: LPJmL emulator files ---#
    calcOutput("GrassSoilEmu", subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, mfile = "weights", aggregate = F, file = "f31_weights.mz")
    calcOutput("GrassSoilEmu", subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, mfile = "mean_col", aggregate = F, file = "f31_mean_col.mz")
    calcOutput("GrassSoilEmu", subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, mfile = "stddevs_col", aggregate = F,  file = "f31_stddevs_col.mz")
    calcOutput("GrassSoilEmu", subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, mfile = "mean_lab", aggregate = F,  file = "f31_mean_lab.mz")
    calcOutput("GrassSoilEmu", subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, mfile = "stddevs_lab", aggregate = F, file = "f31_stddevs_lab.mz")
    calcOutput("GrassSoilEmu", subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, mfile = "inputs", aggregate = F,  file = "f31_inputs.mz")

    #--- Post-processing: LPJmL emulator inputs ---#
    calcOutput("CollectEnvironmentData_new",
               subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"), sar = 1, aggregate = F,
               sel_feat = c("tas", "pr", "lwnet", "rsds", "CO2", "Ks", "Sf", "w_pwp", "w_fc", "w_sat", "hsg", "wet"),
               file = paste0("environment_gramnt.mz"), years = seq(1965, 2100, by = 5))

    #--- Post-processing: files for disaggregation of outputs---#
    calcOutput("RangeSoilCarbonHist",
               subtype = paste(version_isimip, climatemodel, scenario, "1965_2100", sep = ":"),
               model = emu_id, lpjml = lpjml[["grass"]], file = "f31_range_soilc_hist.mz", aggregate = F)
    calcOutput("GrasslandsYields",
               subtype = paste(lpjml[["grass"]], paste0(paste(climatemodel, scenario, sep = ":"),
                                                        "/co2/Nreturn0p5/limN"), sep = ":"),
               lsu_levels = c(seq(0, 2.2, 0.2), 2.5), past_mngmt = "me2", file = paste0("f14_grassl_yld.mz"),
               years = mag_years, aggregate = F)
    calcOutput("LsuDensityHist", disagg_type = "grassland", aggregate = F,  file = "f31_lsu_ha_grassl.mz")
    calcOutput("LsuDensityHist", disagg_type = "livestock", aggregate = F,  file = "f31_lsu_ha_livestock.mz")
    calcOutput("LUH2v2", aggregate = F, landuse_types = "LUH2v2", cellular = TRUE, file = paste0("fm_LUH2v2.mz"))

    #--- Post-processing: Soil carbon ---#
    calcOutput("CollectSoilCarbonLSU", lsu_levels = c(seq(0, 2, 0.2), 2.5), lpjml = lpjml[["grass"]],
               climatemodel = climatemodel, scenario = paste0(scenario, "/co2/Nreturn0p5/limN"), sar = 1,
               aggregate = F, file = paste0("soilc_stocks_gramnt.mz"), years = seq(1965, 2100, by = 5))
    calcOutput("CollectSoilCarbonPastr", past_mngmt = "me2", lpjml = lpjml[["grass"]], climatemodel = climatemodel,
               aggregate = F, scenario = paste0(scenario, "/co2/Nreturn0p5/limN"), sar = 1,
               file = paste0("soilc_stocks_pastr.mz"), years = seq(1965, 2100, by = 5))

    #--- Experimental functions ----#
    # calcOutput("GrassPastureShare", aggregate = "cluster", file = paste0("f31_pastr_share_", ctype, ".mz"))
    # calcOutput("GrassLndYldHist", aggregate = "cluster", file = paste0("f14_grassl_yld_hist_cell", ctype, ".mz"))
  }

  calcOutput("ClimateClass", aggregate = "cluster", years = "y2015", file = paste0("koeppen_geiger_", ctype, ".mz"))

  # 09 drivers
  calcOutput("GridPop_new", subtype = "all", cellular = TRUE, harmonize_until = 2015, urban = FALSE,
             aggregate = "cluster", years = mag_years, round = 6, file = "f09_pop_grid.cs3")

  calcOutput("GridPop_new", subtype="all", cellular=TRUE, harmonize_until=2015, urban = TRUE,
             aggregate="cluster", years = mag_years, round=6, file="f09_urbanpop_grid.cs3")

  # 10 land
  # seven land classes
  calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, cells = "magpiecell", nclasses = "seven",
    fao_corr = TRUE, input_magpie = TRUE, selectyears = mag_years_past_long, round = 6,
    file = "avl_land_t_0.5.mz")
  calcOutput("LanduseInitialisation", aggregate = "cluster", cellular = TRUE, nclasses = "seven", fao_corr = TRUE,
    input_magpie = TRUE, selectyears = mag_years_past_long, round = 6,
    file = paste0("avl_land_t_", ctype, ".mz"))
  calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, nclasses = "seven", fao_corr = TRUE,
             input_magpie = TRUE, selectyears = mag_years_past_long, round = 6, country_level = TRUE,
             file = paste0("avl_land_t_iso.cs3"))

  # nine land classes
  calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, cells = "magpiecell", nclasses = "nine",
             fao_corr = TRUE, input_magpie = TRUE, selectyears = mag_years_past_long, round = 6,
             file = "avl_land_full_t_0.5.mz")
  calcOutput("LanduseInitialisation", aggregate = "cluster", cellular = TRUE, nclasses = "nine", fao_corr = TRUE,
             input_magpie = TRUE, selectyears = mag_years_past_long, round = 6,
             file = paste0("avl_land_full_t_", ctype, ".mz"))
  calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, nclasses = "nine", fao_corr = TRUE,
             input_magpie = TRUE, selectyears = mag_years_past_long, round = 6, country_level = TRUE,
             file = paste0("avl_land_full_t_iso.cs3"))

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
  calcOutput("LabourProdImpactEmu", aggregate = "cluster", round = 6, subtype = "impact",
             file = paste0("f37_labourprodimpact_", ctype, ".mz"))
  calcOutput("LabourProdImpactEmu", aggregate = "cluster", round = 6, subtype = "relief",
             file = paste0("f37_labourprodrelief_", ctype, ".mz"))

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
