#' @title readRamankutty
#'
#' @description Read in data of Ramankutty dataset (Source: Ramankutty N, Foley JA, Norman J and McSweeney K (2002) The global distribution of cultivable lands: current patterns and sensitivity to possible climate change. Global Ecology and Biogeography, 11, 377-392.). Note: data processing analogous to /p/projects/magpie/data/input/other/rev23/avl_land_si.R
#'
#' @return magpie object
#'
#' @examples
#' \dontrun{
#'   readSource("Ramankutty", convert="onlycorrect")
#' }
#' @import magclass
#' @export
#' @author Felicitas Beier
#'
readRamankutty <- function(){

  #x <- read.magpie("avl_land_si_0.5.mz")

  # suit1 <- as.magpie(brick("C:/Users/beier/Documents/Tasks/MAgPIE tasks/raw_data_land_suit_land_suit_0.50x0.50_cluster.nc"))
  # suit3 <- as.magpie(brick("C:/Users/beier/Documents/Tasks/MAgPIE tasks/raw_data_land_suit_land_suit_0.50x0.50.nc"))
  #
  # suit2 <- as.magpie(brick("C:/Users/beier/Documents/Modelle/inputdata/sources/suit/suit/w001001.adf"))
  #

  # x <- suit3

  # options(error=function()traceback(2))
  #
  # library(magclass)
  # #https://www.nelson.wisc.edu/sage/data-and-models/global-land-use/grid.php
  # #Source: Ramankutty N, Foley JA, Norman J and McSweeney K (2002) The global distribution of cultivable lands: current patterns and sensitivity to possible climate change. Global Ecology and Biogeography, 11, 377-392.
  # in_file    <- "raw_data_land_suit_land_suit_0.50x0.50.nc"
  # out_file   <- "avl_land_si_0.5.mz"
  #
  # x <- read.magpie(in_file)
  # x[is.na(x)] <- 0
  #
 # si0_binary <- x
#  si0_binary[si0_binary > 0.1]  <-  1
#  si0_binary[si0_binary <= 0.1] <-  0
 # getNames(si0_binary) <- NULL
  #
  # cellArea <- dimSums(read.magpie("avl_land_0.5.mz"),dim=3)
  #
  # si0 <- si0_binary*cellArea
  # nsi0 <- cellArea-si0
  #
  # out <- mbind(setNames(si0,"si0"),setNames(nsi0,"nsi0"))
  #
  # getComment(out) <- c(" description: si0 and nsi0 areas based on Ramankutty dataset",
  #                      " unit: Mha",
  #                      " origin: /p/projects/landuse/data/input/other/rev23/avl_land_si.R",
  #                      " creation date: Tue May 29 11:10:50 2017")
  # write.magpie(out,file_name = out_file)
  #
  # # shr <- out/cellArea
  # # getYears(shr) <- "y2000"
  # # write.magpie(shr,"avl_land_si_shr_0.5.nc")

  return(x)
}
