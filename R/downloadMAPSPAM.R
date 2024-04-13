#' @title downloadMAPSPAM
#' @description Downloads the MAP-SPAM (SPAM) data set for harvested and physical croparea
#' @return raw files for MAPSPAM
#' @author Edna J. Molina Bacca
#' @importFrom utils download.file unzip
#' @seealso [downloadSource()]
#' @examples
#' \dontrun{
#' a <- download("downloadMAPSPAM")
#' }
#'
downloadMAPSPAM <- function() {
  # MAP-SPAM data sets for harvested and physical area
  # more information at: https://www.mapspam.info/data/
  #                      https://www.mapspam.info/publications/

  links <- c(Harv2000  = "https://dataverse.harvard.edu/api/access/datafile/3666794?gbrecs=true", # nolint
             Phy2000   = "https://dataverse.harvard.edu/api/access/datafile/3666793?gbrecs=true", # nolint
             Harv2005  = "https://dataverse.harvard.edu/api/access/datafile/3086558?gbrecs=true", # nolint
             Phy2005   = "https://dataverse.harvard.edu/api/access/datafile/3086559?gbrecs=true", # nolint
             Harv2010  = "https://dataverse.harvard.edu/api/access/datafile/3985008?gbrecs=true", # nolint
             Phy2010   = "https://dataverse.harvard.edu/api/access/datafile/3985010?gbrecs=true")  # nolint

  names <- c(Harv2000  = "spam2000v3r7_global_harv_area.geotiff", Phy2000   = "spam2000v3r7_global_phys_area.geotiff",
             Harv2005  = "spam2005v3r2_global_harv_area.geotiff", Phy2005   = "spam2005v3r2_global_phys_area.geotiff",
             Harv2010  = "spam2010v2r0_global_harv_area.geotiff", Phy2010   = "spam2010v2r0_global_phys_area.geotiff")

  ###  unzip files

  lapply(seq_len(length(links)), FUN = function(x) {
    download.file(links[x], destfile = paste0(names[x], ".zip"), mode = "wb")
  })

  zipfiles <- list.files(pattern = ".zip$")

  lapply(seq_len(length(links)), FUN = function(x) {
    unzip(zipfiles[x], exdir = names[x])
  })

  lapply(zipfiles, unlink)

  return(list(url           = "https://www.mapspam.info/data/",
              doi           =  NULL,
              title         = "MapSPAM Global Data",
              author        = "IFPRI",
              version       = "2000=v3.0.7, 2005=v3.2, 2010=v2.0",
              release_date  = "2000=Jan 22, 2020, 2005=Dec 5, 2017, 2010=Jul 15, 2020",
              description   = "Grid data of physical and harvested croparea",
              license       = "CC BY 4.0",
              reference     = NULL,
              unit          = "ha"))

}
