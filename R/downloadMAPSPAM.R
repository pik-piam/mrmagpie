#' @title downloadMAPSPAM
#' @description Downloads the MAP-SPAM (SPAM) data set for harvested and physical croparea
#' @return raw files for MAPSPAM
#' @author Edna J. Molina Bacca
#' @importFrom utils download.file
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

 links <- c(
          Harv2000  = "https://s3.amazonaws.com/mapspam/2000/v3.0.7/geotiff/spam2000v3.0.7_global_harvested-area.geotiff.zip",
          Phy2000   = "https://s3.amazonaws.com/mapspam/2000/v3.0.7/geotiff/spam2000v3.0.7_global_physical-area.geotiff.zip",
          Harv2005  = "https://s3.amazonaws.com/mapspam/2005/v3.2/geotiff/spam2005v3r2_global_harv_area.geotiff.zip",
          Phy2005   = "https://s3.amazonaws.com/mapspam/2005/v3.2/geotiff/spam2005v3r2_global_phys_area.geotiff.zip",
          Harv2010  = "https://s3.amazonaws.com/mapspam/2010/v2.0/geotiff/spam2010v2r0_global_harv_area.geotiff.zip",
          Phy2010   = "https://s3.amazonaws.com/mapspam/2010/v2.0/geotiff/spam2010v2r0_global_phys_area.geotiff.zip"
        )

 names <- c(Harv2000  = "spam2000v3r7_global_HA_geotiff",
          Phy2000   = "spam2000v3r7_global_PA_geotiff",
          Harv2005  = "spam2005v3r2_global_HA_geotiff",
          Phy2005   = "spam2005v3r2_global_PA_geotiff",
          Harv2010  = "spam2010v2r0_global_HA_geotiff",
          Phy2010   = "spam2010v2r0_global_PA_geotiff"
        )

 ###  unzip files

 lapply(seq_len(length(links)), FUN = function(x) {
 download.file(links[x], destfile = paste0(names[x], ".zip"), mode = "wb")
 })

 zipfiles <- list.files(pattern = ".zip$")

 lapply(seq_len(length(links)), FUN = function(x) {
 unzip(zipfiles[x], exdir = names[x])
 })

 lapply(zipfiles, unlink)

}
