#' @title calcAvlLandSi
#' @description Extracts si0 and nsi0 areas based on Ramankutty dataset
#'
#' @param dataprocessing just a temporary argument until read Ramankutty is fully functional (original or mrmagpie)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("AvlLandSi", aggregate = FALSE) }
#'
#' @importFrom magpiesets findset
#'

calcAvlLandSi <-function(x, dataprocessing="original"){

  if (dataprocessing=="original") {
    x <- readSource("AvlLandSi", convert="onlycorrect")

  } else if (dataprocessing=="mrmagpie") {
    x <- readSource("Ramankutty", convert="onlycorrect")

    #
    # #x <- read.magpie("avl_land_si_0.5.mz") #### TO COMPARE ENDRESULT TO
    #
    # # path: C:\Users\beier\Documents\Modelle\inputdata\sources\Ramankutty
    # # read in raster brick and transform to magpie object
    # x <- as.magpie(brick("raw_data_land_suit_land_suit_0.50x0.50.nc"))
    #
    # #x <- as.magpie(brick(paste0(readClipboard(),"/raw_data_land_suit_land_suit_0.50x0.50.nc")))
    #
    # x1 <- read.magpie(paste0(readClipboard(),"/raw_data_land_suit_land_suit_0.50x0.50.nc"))
    # x1 <- addLocation(x1)
    # x1 <- collapseDim(x1, dim=c("fake","region"))
    # avl_land_original <- read.magpie(paste0(readClipboard(),"/avl_land_0.5.mz"))
    # avl_land_original_total <- dimSums(avl_land_original,dim=3)
    # avl_land_original <- addLocation(avl_land_original)
    #
    #
    # #### Calculations
    #
    # si0_binary                    <- x
    # si0_binary[si0_binary > 0.1]  <- 1
    # si0_binary[si0_binary <= 0.1] <- 0
    # getNames(si0_binary) <- NULL
    #
    # cellArea <-   calcOutput("LanduseInitialisation", aggregate=FALSE, cellular=TRUE, cell="lpjcell", land="fao", input_magpie=TRUE, selectyears=mag_years_past_long, round=6, file="avl_land_t_0.5.mz")
    #
    # rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
    #
    # # path: C:\Users\beier\Documents\Tasks
    # avl_land_magpie <- read.magpie(paste0(readClipboard(), "/avl_land_t_0.5.mz"))
    # avl_land_magpie <- addLocation(avl_land_magpie)
    # avl_land_magpie <- collapseDim(avl_land_magpie, dim=c("fake","lpjcell"))
    # avl_land_lpjml  <- read.magpie(paste0(readClipboard(), "/avl_land_t_0.5_lpjcell.mz"))
    # getCells(avl_land_lpjml) <- rs$coordinates
    #
    # avl_land_magpie_total <- dimSums(avl_land_magpie, dim=3)
    # avl_land_lpjml_total <- dimSums(avl_land_lpjml, dim=3)
    # getCells(avl_land_lpjml_total) <- rs$coordinates
    #
    # range(avl_land_magpie_total[,,]-avl_land_lpjml_total[getCells(avl_land_magpie),,])
    #
    # range(avl_land_magpie[,,]-avl_land_lpjml[getCells(avl_land_magpie),,])
    # summary(avl_land_magpie[,,]-avl_land_lpjml[getCells(avl_land_magpie),,])
    #
    # getCells(avl_land_lpjml) <- rs$coordinates
    #
    #
    # #cellArea <- avl_land_lpjml
    # cellArea <- avl_land_magpie
    # #cellArea <- avl_land_original
    # cellArea <- dimSums(cellArea, dim=3)
    #
    # si0  <- si0_binary[getCells(cellArea),,] * cellArea
    # nsi0 <- cellArea-si0
    #
    #
    #
    # # x <- suit3
    #
    # ##### --> only read in Ramankutty!!!!!
    #
    # # options(error=function()traceback(2))
    # #
    # # library(magclass)
    # # #https://www.nelson.wisc.edu/sage/data-and-models/global-land-use/grid.php
    # # #Source: Ramankutty N, Foley JA, Norman J and McSweeney K (2002) The global distribution of cultivable lands: current patterns and sensitivity to possible climate change. Global Ecology and Biogeography, 11, 377-392.
    # # in_file    <- "raw_data_land_suit_land_suit_0.50x0.50.nc"
    # # out_file   <- "avl_land_si_0.5.mz"
    # #
    # # x <- read.magpie(in_file)
    # # x[is.na(x)] <- 0
    # #
    # # si0_binary <- x
    # #  si0_binary[si0_binary > 0.1]  <-  1
    # #  si0_binary[si0_binary <= 0.1] <-  0
    # # getNames(si0_binary) <- NULL
    # #
    # # cellArea <- dimSums(read.magpie("avl_land_0.5.mz"),dim=3)
    # #
    # # si0 <- si0_binary*cellArea
    # # nsi0 <- cellArea-si0
    # #
    # # out <- mbind(setNames(si0,"si0"),setNames(nsi0,"nsi0"))
    # #
    # # getComment(out) <- c(" description: si0 and nsi0 areas based on Ramankutty dataset",
    # #                      " unit: Mha",
    # #                      " origin: /p/projects/landuse/data/input/other/rev23/avl_land_si.R",
    # #                      " creation date: Tue May 29 11:10:50 2017")
    # # write.magpie(out,file_name = out_file)
    # #
    # # # shr <- out/cellArea
    # # # getYears(shr) <- "y2000"
    # # # write.magpie(shr,"avl_land_si_shr_0.5.nc")
    #
    #

  } else {
    stop("Please specify the datapreprocessing argument: original or mrmagpie")
  }

  return(list(
    x=x,
    weight=NULL,
    unit="Mha",
    description="si and nsi0 areas",
    isocountries=FALSE))
}
