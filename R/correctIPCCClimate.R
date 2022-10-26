#' @title correctIPCCClimate
#' @description Correct IPCC climate classification
#'
#' @return Magpie object with results on cellular level for 12 IPCC climate zone types
#' @param x magpie object provided by the read function
#' @author  Kristine Karstens
#' @examples
#' \dontrun{
#' readSource("IPCCClimate", convert = "onlycorrect")
#' }
#'
correctIPCCClimate <-  function(x) {

  ### determine gaps on iso level
  iso <- unique(substring(where(setYears(x[, , "NA"], "y2010") == 1)$true$reg, 1, 3))

  ## move unknown cells to most common country values
  nmax        <- rep(0, length(iso))
  names(nmax) <- iso
  for (i in iso) {
    nmax[i] <- which.max(dimSums((x[i, , ]), dim = 1))
    x[i, , nmax[i]] <- x[i, , nmax[i]] + x[i, , "NA"]
  }

  if (any(x[, , "NA"] == 1)) message("Still grid cells without climate zone")
  out <- x[, , "NA", invert = TRUE]

  return(out)
}
