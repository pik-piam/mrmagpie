#' @title calcPastureSuit
#' @description Calculate glassland suitable for pasture management based
#' on population and aridity criteria.
#'
#' @param lpjml Defines LPJmL version for crop/grass and natveg specific inputs
#' @param climatetype Switch between different climate scenarios
#' @param cells number of halfdegree grid cells to be returned.
#'              Options: "magpiecell" (59199), "lpjcell" (67420)
#'
#' @return List of magpie object with results on cluster level
#' @author Marcos Alves, Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("PastureSuit")
#' }
#' @importFrom raster area rasterFromXYZ

calcPastureSuit <- function(climatetype = "MRI-ESM2-0:ssp126",
                            lpjml       =  "LPJmL4_for_MAgPIE_44ac93de",
                            cells       = "lpjcell") {

  x <- toolSplitSubtype(climatetype, list(climatemodel = NULL, scenario = NULL))

  # Extract stage argument information
  if (grepl("GSWP3-W5E5", climatetype)) {
    stage <- "smoothed"
  } else {
    stage <- "harmonized2020"
  }

  # Drivers of managed pastures
  population <- calcOutput("GridPop", subtype = "all", cellular = TRUE, FiveYear = TRUE, harmonize_until = 2015,
                           aggregate = FALSE)[, , toupper(substring(x$scenario, first = 0, last = 4))]

  cellPrep   <- calcOutput("LPJmLClimateInput", climatetype  = climatetype,
                           variable     = "precipitation:monthlySum",
                           stage        = stage,
                           lpjmlVersion = lpjml,
                           aggregate    = FALSE)


  cellPet    <- calcOutput(type = "LPJmL_new", climatetype = climatetype,
                           subtype   = "mpet",
                           stage     = stage,
                           version   = lpjml,
                           aggregate = FALSE)

  yearsCellPet      <- intersect(getYears(cellPet), findset("time"))
  yearsCellPrep     <- intersect(findset("time"), getYears(cellPrep))
  years             <- intersect(yearsCellPet, yearsCellPrep)
  cellPrep          <- dimSums(cellPrep[, years, ], dim = 3)
  cellPet           <- dimSums(cellPet[, years, ], dim = 3)

  # Cell area calculation
  landArea <- calcOutput("LandArea", aggregate = FALSE) / 100 # transformed to km^2

  # population density
  popDensity <- (population * 1e6) / landArea # population density in number of people per km2
  popDensity[is.infinite(popDensity)] <- 0
  popDensity[is.nan(popDensity)] <- 0

  years <- intersect(getYears(popDensity), getYears(cellPrep))

  aridity <- cellPrep[, years, ] / cellPet[, years, ]
  aridity[is.infinite(aridity) | is.nan(aridity)] <- 0
  # 0.5 aridity threshold for managed pastures. Same from HYDE 3.2.
  aridity[aridity < 0.5] <- 0
  aridity[aridity >= 0.5] <- 1

  # pasture suitability check
  pastureSuit <- aridity
  popDensity <- popDensity[, getYears(pastureSuit), ]
  pastureSuit[popDensity < 5] <- 0 # 5 hab km2 population threshold for managed pastures. Same from HYDE 3.2.

  pastureSuitArea <- (pastureSuit * landArea * 100) / 1e6 # (from km2 (x100) to mha (/1e6))
  pastureSuitArea <- collapseDim(pastureSuitArea)
  pastureSuitArea <- toolHoldConstantBeyondEnd(pastureSuitArea)

  # calibration to historical values
  histPastr <- setNames(calcOutput("LanduseInitialisation", cellular = TRUE,
                                   nclasses = "nine", aggregate = FALSE)[, , c("past")], "pastr")
  pastAll <- intersect(getYears(histPastr), getYears(pastureSuitArea))

  pastLy <- findset("past")
  pastLy <- pastLy[length(pastLy)] # past last year
  future <- setdiff(getYears(pastureSuitArea), pastAll)

  pastureSuitAreaReg <- dimSums(pastureSuitArea, dim = c("x", "y"))
  histPastrReg       <- dimSums(histPastr, dim = c("x", "y"))
  calibReg <- histPastrReg[, pastLy, ] / pastureSuitAreaReg[, pastLy, ]
  calibReg[is.infinite(calibReg)] <- 1
  calibReg[is.nan(calibReg)]      <- 0
  pastureSuitArea[, future, ] <- calibReg * pastureSuitArea[, future, ]

  pastureSuitArea[is.infinite(pastureSuitArea) | is.nan(pastureSuitArea) | is.na(pastureSuitArea)] <- 0
  pastureSuitArea[pastureSuitArea < 0] <- 0
  pastureSuitArea[, pastAll, ] <- histPastr[, pastAll, ]

  y <- intersect(pastAll, getYears(pastureSuitArea))
  pastureSuitArea[, y, ] <- histPastr[, intersect(pastAll, getYears(pastureSuitArea)), ]
  pastureSuitArea <- toolHoldConstant(pastureSuitArea, findset("time"))
  pastureSuitArea <- collapseNames(pastureSuitArea)
  pastureSuitArea[, pastAll, ] <- histPastr[, pastAll, ]
  #maybe change to: pastureSuitArea[, pastAll, ] <- max(histPastr[, pastAll, ], pastureSuitArea[, pastAll, ]) #nolint
  pastureSuitArea <- setNames(pastureSuitArea, "yields")

  # Reduce number of grid cells to 59199
  if (cells == "magpiecell") {
    pastureSuitArea <- toolCoord2Isocell(pastureSuitArea, cells = cells)
  }

  return(list(x            = pastureSuitArea,
              weight       = NULL,
              unit         = "Mha",
              description  = "Area suitable for pasture management in mha",
              isocountries = FALSE))
}
