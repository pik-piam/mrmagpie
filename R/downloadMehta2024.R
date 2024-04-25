#' @title downloadMehta2024
#' @description download Global Area Equipped for Irrigation Dataset 1900-2015 from Mehta et al. (2024).
#'              Gridded dataset is created based on (sub-)national statistics from FAOSTAT, AQUASTAT, EUROSTAT
#'              and country's census data downscaled using two alternative gridded irrigation maps
#'              (GMIA from Siebert et al. 2013 and Meier et al. 2018)
#'
#' @param subtype data subtype to be downloaded.
#'                Subtypes available:
#'                'GMIA': gridded base map for downscaling from Stefan et al. (2013).
#'                        Global Map of Irrigation Areas version 5.
#'                'Meier2018': gridded base map for downscaling from Meier, et al. (2018).
#'                             Global Irrigated Areas.
#'
#' @author  Felicitas Beier
#' @seealso [downloadSource()] [readMehta2024()]
#' @examples
#' \dontrun{
#' a <- downloadSource()
#' }
#'
#' @importFrom utils download.file
#' @importFrom withr with_options

downloadMehta2024 <- function(subtype = "GMIA") {

  aeiURL <- "https://zenodo.org/records/7809342"

  if (subtype == "GMIA") {
    dataname <- "G_AEI_"
  } else if (subtype == "Meier2018") {
    dataname <- "MEIER_G_AEI_"
  } else {
    stop("The selected subtype is not available for downloadMehta2024. Please select 'GMIA' or 'Meier2018'.")
  }

  years <- c(seq(1900, 1970, by = 10),
             seq(1980, 2015, by = 5))

  files <- c(paste0(dataname, years, ".ASC"))

  # repeat with count and maxcount is necessary for case of 'Couldn't connect to server' error
  maxcount <- 10
  count    <- 0
  for (file in files) {
    repeat {

      withr::with_options(list(timeout = NULL),
                          code = try(download.file(paste0(aeiURL, "/files/", file),
                                                   destfile = file, mode = "wb")))

      count <- count + 1
      if (file.exists(file) || count >= maxcount) {
        break
      }
    }
  }

  return(list(url          = aeiURL,
              doi          = "10.5281/zenodo.6740334",
              title        = "Global Area Equipped for Irrigation Dataset 1900-2015",
              revision     = "2024",
              release_date = "2023-04-07",
              author       = "Mehta, Piyush;  Siebert, Stefan;
                              Kummu, Matti; Deng, Qinyu; Ali, Tariq;
                              Marston, Landon; Xie, Wei;  Davis, Kyle",
              description  = paste0("Gridded dataset of Areas Equipped for Irrigation. ",
                                    "Created based on (sub-)national statistics from FAOSTAT, ",
                                    "AQUASTAT, EUROSTAT and country's census data ",
                                    "downscaled using two alternative gridded irrigation maps ",
                                    "(GMIA from Siebert et al. 2013 and Meier et al. 2018)"),
              unit         = "ha",
              license      = "Creative Commons Attribution 4.0 International"))
}
