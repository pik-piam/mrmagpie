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

  links <- c(
    Harv2000  = "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/A50I2T/16fcdb67eea-f921ce3db735?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27spam2000v3.0.7_global_harvested-area.geotiff.zip&response-content-type=application%2Fzip&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240301T100405Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240301%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=ba5c37dc3979e3b735c0439c60f821e7c74b406647e3d348c763fe112d06a138", # nolint
    Phy2000   = "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/A50I2T/16fcdb6a393-48a3ec461f3e?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27spam2000v3.0.7_global_physical-area.geotiff.zip&response-content-type=application%2Fzip&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240301T095814Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240301%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=2b2c0176d16691e9637161a10fcc16d1915c07199cc57128e0f3e2e25be6478e",  # nolint
    Harv2005  = "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/DHXBJX/16028059857-53c4854c49cc?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27spam2005v3r2_global_harv_area.geotiff.zip&response-content-type=application%2Fzip&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240301T100508Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240301%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=de525e5e07e42b184ca5d62cf70b420357367787c674e2c51e011ef0d7ae432c", # nolint
    Phy2005   = "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/DHXBJX/160280628d5-764f80d27a41?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27spam2005v3r2_global_phys_area.geotiff.zip&response-content-type=application%2Fzip&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240301T100453Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240301%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=5fa97d903ed5fe6e7543924e4f8ada761813ecfaae7a2333eb4affacd61b1a76", # nolint
    Harv2010  = "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/PRFF8V/1735567cda7-3ad213a31087?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27spam2010v2r0_global_harv_area.geotiff.zip&response-content-type=application%2Fzip&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240301T100549Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240301%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=84402b34cbf080487477ffb61fb58b0086fc26c9d823c7dd6ed8003e50d71541", # nolint
    Phy2010   = "https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/PRFF8V/17355681721-503806f0e24a?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27spam2010v2r0_global_phys_area.geotiff.zip&response-content-type=application%2Fzip&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20240301T100601Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20240301%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=ff08690dfc132351037be9153c87469f0035b33c9da8f839ab89f6bdc6510d1b"  # nolint
  )

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
              reference     = NULL))

}
