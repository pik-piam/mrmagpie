#' @title readPastYields
#' @description Function `readPastYields` read binary data from output of LPJmL runs and processes them into a `magpie object`.
#' @param subtype type of Pasture Management
#' @author Marcos Alves
#' @import lpjclass
#' @import magclass
#' @importFrom stats na.omit
#' @export


readPastYields <- function(subtype) {
  yield_transform <- 0.01 / 0.45
  dirs <- list.dirs(path = subtype)[-1]

  co2type <- NULL
  RCPtype <- NULL
  mngmttype <- NULL

  for (dir in dirs) {
    y <- unlist(strsplit(dir, split = "/"))
    co2type <- append(co2type, y[2])
    RCPtype <- append(RCPtype, y[3])
    mngmttype <- append(mngmttype, y[4])
    y <- NULL
  }

  co2type <- unique(na.omit(co2type))
  RCPtype <- unique(na.omit(RCPtype))
  mngmttype <- unique(na.omit(mngmttype))

  y <- list()

  for (co2 in co2type) {
    for (RCP in RCPtype) {
      for (mngmt in mngmttype) {
        folder <- paste(subtype, co2, RCP, mngmt, sep = "/")
        files <- list.files(folder)
        file_name <- "pft_harvest.pft.bin"
        if (length(grep(file_name, files)) > 0) {
          print(paste("Processing:", folder))
          if (subtype == "cont_grazing") {
            x <- lpjclass::readLPJ(
              file_name = paste0("/", file_name),
              file_folder = folder,
              wyears = c(seq(1995, 2099, 5), 2099),
              syear = 1951,
              monthly = F,
              years = 149,
              bands = 32,
              ncells = 67420,
              soilcells = T
            )
          }

          if (subtype == "mowing") {
            x <- lpjclass::readLPJ(
              file_name = paste0("/", file_name),
              file_folder = folder,
              wyears = c(2000), # I have to fix this to allow all years in the simulation
              syear = 1998,
              monthly = F,
              years = 5,
              bands = 32,
              ncells = 67420,
              soilcells = T
            )
          }

          x <- as.magpie(x[, , "mgrass", "rainfed"])
          dimnames(x)[3] <- paste(co2, RCP, gsub("\\.", "_", mngmt), sep = ".")
          y[[paste(co2, RCP, mngmt, sep = ".")]] <- x
        }
      }
    }
  }
  u <- mbind(y)
  u <- yield_transform * u
  return(u)
}


#
# readPastYields <- function(subtype = "ContinousGrazing:elevatedCO2:rcp8p5") {
#   #subtype = "ContinousGrazing:elevatedCO2:rcp8p5"
#   if (grepl("\\:", subtype)) {
#     type <- strsplit(gsub(":", "/", subtype), split = "\\.")
#     folder <- unlist(type)[1]
#     subtype <- unlist(strsplit(folder, "/"))[1]
#   } else {
#     stop("Subtype not defined properly")
#   }
#
#   yield_transform <- 0.01/0.45
#   dirs <- list.dirs(path = folder)[-1]
#   lsu_ha <- unlist(strsplit(dirs, split = "/"))
#
#   co2 <- NULL
#   RCP <- NULL
#   mngmt <- NULL
#   print(dirs)
#   for (dir in dirs) {
#     print(dir)
#     y <- unlist(strsplit(dir, split = "/"))
#     co2 <- append(co2,y[2])
#     RCP <- append(RCP,y[3])
#     mngmt <- append(mngmt,y[4])
#   }
#   print(unique(co2))
#   print(unique(RCP))
#   print(unique(mngmt))
#
#   y <- list()
#
#   if (subtype == "ContinousGrazing") {
#     for (lsu in 1:length(dirs)) {
#       print(paste("Processing:", dirs[lsu]))
#       lsu_ha <- unlist(strsplit(dirs[lsu], split = "/"))
#
#       if (is.na(as.numeric(lsu_ha[length(lsu_ha)]))) {
#         stop(paste("The folder containing the output of LPLmL runs for continous grazing have to be
#         named with numbers representing the livestock densities of that run. Problem folder name:", lsu_ha[length(lsu_ha)]))
#       }
#
#       x <- lpjclass::readLPJ(
#         file_name = "/pft_harvest.pft.bin",
#         file_folder = dirs[lsu],
#         wyears = c(seq(1995, 2099, 5), 2099),
#         syear = 1951,
#         monthly = F,
#         years = 149,
#         bands = 32,
#         ncells = 67420,
#         soilcells = T
#       )
#       x <- collapseNames(x)
#       x <- as.magpie(x[, , "mgrass", "rainfed"])
#       dimnames(x)[3] <- lsu_ha[length(lsu_ha)]
#       y[[lsu]] <- x
#     }
#     u <- mbind(y)
#     u <- yield_transform * u
#   }
#
#   if (subtype == "Mowing") {
#     for (events in 1:length(dirs)) {
#     print(paste("Processing:", dirs[events]))
#     m_events <- unlist(strsplit(dirs[events], split = "/"))
#     #I still need to adjust this read function to the actual files provided by LPJML folks 01.20.2020
#     x <- lpjclass::readLPJ(
#       file_name = "/pft_harvest.pft.bin",
#       file_folder = dirs[events],
#       wyears = c(2000), #I have to fix this to allow all years in the simulation
#       syear = 1998,
#       monthly = F,
#       years = 5,
#       bands = 32,
#       ncells = 67420,
#       soilcells = T
#     )
#     x <- as.magpie(x[, , "mgrass", "rainfed"])
#     dimnames(x)[3] <- m_events[length(m_events)]
#     y[[events]] <- x
#     }
#     u <- yield_transform * x
#     u <- mbind(y)
#   }
#   return(u)
# }
