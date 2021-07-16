#' @title readGrassSoilEmu
#' @description Read files related to the training and optimization of the LPJml emulators.
#' @param subtype Subtype of file to be opened. Subtypes available:
#' 'weights', 'inputs',  'stddevs' and 'means'.
#' @return Magpie objects with a diverse inforamtion
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' readSource("GrassSoilEmu", subtype =
#' "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100:5f5fa2:weights", convert = F)
#' }
#'
#' @import madrat
#'

readGrassSoilEmu <-
  function(subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100:5f5fa2:weights") {
    . <- NULL
    subtype_split <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL, scenario = NULL, years = NULL, model = NULL, variable = NULL))
    file <- subtype_split$variable
    dirs <- list.dirs()
    subtype <- paste(subtype_split[1], subtype_split[2], subtype_split[3], subtype_split[4], sep = "_")
    subtype <- subtype %>% gsub("[:-]", "_", .)
    folder <- grep(subtype, dirs, value = T)

    if (dir.exists(file.path(folder))) {
      files_list <- list.files(folder)
      files <- files_list[grep(file, files_list)]
    } else {
      stop(paste(
        "Path", folder, "does not exist. Check the defition of your",
        "subtype or the folder structure you are trying to access."
      ))
    }
    x <- readRDS(file.path(folder, files))
    x <- as.magpie(as.matrix(x), spatial = 1)
    getNames(x) <- file
    return(x)
  }
