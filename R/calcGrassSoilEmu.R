#' @title calcGrassSoilEmu
#' @description Read files related to the training and optimization of the LPJml emulators.
#' @param subtype Subtype of file to be opened. Subtypes available:
#' 'weights', 'inputs',  'stddevs' and 'means'.
#' @return Magpie objects with a diverse inforamtion
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' readSource("GrassSoilEmu", subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100:5f5fa2:weights")
#' }
#'
#' @import madrat
#'

calcGrassSoilEmu <-
  function(subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100:5f5fa2:weights") {

    x <- readSource("GrassSoilEmu", subtype = subtype, convert = F)

    return(
      list(
        x = x,
        weight = NULL,
        unit = NULL,
        description = "emulator parameters",
        isocountries = FALSE
      )
    )
  }
