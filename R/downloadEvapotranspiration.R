#' @title downloadEvapotranspiration
#' @description Download water models evapotranspiration data
#' @param subtype Switch between different inputs
#' @author  Marcos Alves
#' @examples
#'
#' \dontrun{readSource("Evapotranspiration",  convert="onlycorrect")}

downloadEvapotranspiration <- function(subtype="H08:mri-esm2-0:historical") {

  x           <- toolSplitSubtype(subtype, list(water_model=NULL, climate_model=NULL, scenario = NULL))
  path     <- "/p/projects/rd3mod/inputdata/sources/Evapotranspiration_raw"

  list_files  <- list.files(path)
  file        <- grep("_evap", list_files, value=TRUE)
  if (x$scenario == "historical") {
    file  <- grep("historical_histsoc", file, value=TRUE)
  } else {
    file  <- grep(paste0(x$scenario,"_2015soc"), file, value=TRUE)
  }

  file_path   <- file.path(path, file)

  if (file.exists(file_path)) {
    file.copy(file_path, file)
  } else {
    stop(paste("Data is not available so far:",file_path))
  }

  # Compose meta data
  return(list(url           = paste0(file_path),
              doi           = NULL,
              title         = x$scenario,
              author        = NULL,
              version       = x$water_model,
              release_date  = NULL,
              description   = NULL,
              license       = NULL,
              reference     = NULL,
              unit          = "kg m-2 s-1")
  )
}
