#' @title readGCMClimate_new
#' @description Read Climate data used as LPJmL inputs into MAgPIE objects
#' @param subtype Switch between different inputs eg. "ISIMIP3b:IPSL-CM6A-LR:historical:1850-2014:tas"
#' @return MAgPIE objects with results on cellular level.
#' @author Marcos Alves, Kristine Karstens
#' @seealso
#' \code{\link{readGCMClimate_new}}
#' @examples
#' \dontrun{
#' readSource("GCMClimate_new", subtype, convert = "onlycorrect")
#' }
#'
#' @importFrom lpjclass read.LPJ_input
#' @importFrom madrat toolSubtypeSelect
#' @importFrom magpiesets findset
#' @export

readGCMClimate_new <-
  function(subtype = "ISIMIP3b:IPSL-CM6A-LR:historical:1850-2014:tas") {
    x <- toolSplitSubtype(subtype, list(version = NULL, climatemodel = NULL, scenario = NULL, period = NULL, variable = NULL))
    file_name <- Sys.glob("*.clm")
    file_type <- tail(unlist(strsplit(file_name, "\\.")), 1)

    if (file_type == "clm") {
      filedata <- file(description = file_name, open = "rb", blocking = TRUE, encoding = getOption("encoding"))
      seek(filedata, where = 15, origin = "start")
      in_header <- as.numeric(readBin(filedata, what = integer(), size = 4, n = 5, endian = .Platform$endian))
      start_year <- in_header[1]
      nyear <- in_header[2]
      number_anual_predictions <- in_header[5]
      years <- seq(start_year, start_year + nyear - 1, 1)
      close(filedata)
    } else {
      stop("File format of LPJmL input data unknown. Please provide .clm file format.")
    }
    if (x$variable == "wet") {
      x <- read.LPJ_input(
        file_name = file_name,
        out_years = paste0("y", years),
        namesum = T,
        rule4binary = ">0"
      )
      x <- collapseNames(as.magpie(x))
      return(x)
    }
    x <- lpjclass::read.LPJ_input(
      file_name = file_name,
      out_years = paste0("y", years),
      namesum = T
    )
    x <- collapseNames(as.magpie(x))
    x <- x / number_anual_predictions
    return(x)
  }
