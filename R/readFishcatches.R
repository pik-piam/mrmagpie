#' @title readFishCatches
#' @description Read soil classification data used as input for lpjml
#' @return Magpie object with results on cellular level for soil types
#' @author Marcos Alves, Kristine Karstens
#' @examples
#'
#' \dontrun{
#' readSource("SoilClassification")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom dplyr left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_xlsx

readFishCatches <-
  function() {

    files_list <- list.files()
    files <- c(fishes = files_list[grep("FOCUS", files_list)])
    x <- read_xlsx(files)
    colnames(x) <- gsub("q","",colnames(x))
    y <- pivot_longer(x,cols = 3:14, names_to = "Region", values_to = "Data1")
    y <- as.magpie(y)
    return(y)
  }
