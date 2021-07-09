#' @title calcGrassSoilEmu
#' @description Read files related to the training and optimization of the LPJml emulators.
#' @param subtype Subtype of file to be opened. Subtypes available:
#' 'weights', 'inputs',  'stddevs' and 'means'.
#' @param model trained model ID
#' @param mfile model file name
#' @return Magpie objects with a diverse inforamtion
#' @author Marcos Alves
#' @examples
#' \dontrun{
#' readSource("GrassSoilEmu", subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100",
#'  model = "5f5fa2", mfile = "weights")
#' }
#'
#' @import madrat
#'

calcGrassSoilEmu <-
  function(subtype = "ISIMIP3b:IPSL_CM6A_LR:ssp126:1965_2100", model = "5f5fa2", mfile = "weights") {

    subtype <- paste(subtype, model, mfile, sep = ":")
    subtype_split <-
      toolSplitSubtype(
        subtype,
        list(
          version = NULL,
          climatemodel = NULL,
          scenario = NULL,
          years = NULL,
          model = NULL,
          variable = NULL
        )
      )
    x <- readSource("GrassSoilEmu", subtype = subtype, convert = F)
    mf <- getConfig("outputfolder")
    fname <-
      paste0(mf, "/", "f31_", subtype_split$variable, ".rds")
    attr(x, "model") <- model
    write.magpie(x, fname)

    magclass::population_magpie
    return(
      list(
        x = magclass::population_magpie*0, # not the real output of the function
        weight = NULL,
        unit = NULL,
        description = "Ghost calc function to write .rds into the outputfolder",
        isocountries = FALSE
      )
    )
  }
