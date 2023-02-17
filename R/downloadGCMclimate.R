#' @title downloadGCMClimate
#' @description Download GCM climate input used for Lpjml runs
#'              NOTE: This function will be depreciate soon, please use mrland::downloadLPJmLClimate
#' @param subtype Switch between different inputs (e.g. "ISIMIP3b:IPSL-CM6A-LR:historical:1850-2014:tas")
#'                Argument consists of GCM version, climate model, scenario and variable,
#'                separated by ":"
#' @return metadata entry
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' readSource("GCMClimate", convert = "onlycorrect")
#' }
#'
downloadGCMClimate <- function(subtype = "ISIMIP3b:IPSL-CM6A-LR:ssp126:2015-2100:tas") { # nolint

  x <- toolSplitSubtype(subtype, list(version = NULL,  climatemodel = NULL,
                                      scenario = NULL, period = NULL,
                                      variable = NULL))

  if (x$climatemodel == "GSWP3-W5E5") {
    storage <- "/p/projects/lpjml/input/historical/" # nolint: absolute_path_linter.
  } else {
    storage <- "/p/projects/lpjml/input/scenarios"   # nolint: absolute_path_linter.
  }

  path        <- file.path(storage,                     # historical or scenarios
                           x$version,                   # version: ISIMIP3a or b(v2)
                           gsub("_", "/", x$scenario),  # obsclim e.g. ssp119
                           x$climatemodel)              # GCMs or GSWP3-W5E5

  if (!dir.exists(path)) {
    path <- file.path(storage,
                      x$version,
                      gsub("_", "/", x$scenario),
                      gsub("_", "-", x$climatemodel))
  }

  fileList <- list.files(path)
  file     <- grep(paste0(x$variable, "_"),
                   fileList, value = TRUE)
  file     <- grep(x$period, file, value = TRUE)
  filePath <- file.path(path, file)

  if (file.exists(filePath)) {
    file.copy(filePath, file)
  } else {
    stop("Data is not available so far!")
  }

  # Compose meta data
  return(list(url           = paste0(storage, filePath),
              doi           = NULL,
              title         = x$version,
              author        = NULL,
              version       = x$version,
              release_date  = NULL,
              description   = NULL,
              license       = NULL,
              reference     = NULL)
  )
}
