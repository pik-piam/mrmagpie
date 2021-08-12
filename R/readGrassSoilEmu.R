#' @title readGrassSoilEmu
#' @description Read files related to the training and optimization of the LPJml emulators.
#' @param subtype Subtype of file to be opened. Subtypes available:
#' 'weights', 'inputs',  'stddevs' and 'means'.
#' @return Magpie objects with a diverse inforamtion
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' readSource("GrassSoilEmu",
#'   subtype =
#'     "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100:5f5fa2:weights", convert = F
#' )
#' }
#'
#' @import madrat
#'

readGrassSoilEmu <-
  function(subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100:5f5fa2:weights") {
    subtype_split <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL, scenario = NULL, years = NULL, model = NULL, variable = NULL))
    file <- subtype_split$variable
    dirs <- list.dirs()
    folder <- grep(subtype_split$model, dirs, value = T)
    if (length(dir.exists(file.path(folder))) != 0) {
      files_list <- list.files(folder)
      files <- files_list[grep(file, files_list)]
      if (file != "baselines") {
        files <- files[grep(".rds", files)]
        x <- readRDS(file.path(folder, files))
        if (length(x) == 1) {
          spatial <- NULL
        } else {
          spatial <- 1
        }
        x <- as.magpie(as.matrix(x), spatial = spatial)
        getNames(x) <- file
        return(x)
      } else {
        files <- files[grep(".mz", files)]
        x <- read.magpie(file.path(folder, files))
        getNames(x) <- file
        return(x)
      }
    } else {
      print(paste(
        "Path", folder, "does not exist. Check the defition of your",
        "subtype and if an emulator with this inputs have already been trained."
      ))
      x <- magclass::population_magpie * 0
      return(x)
    }
  }
