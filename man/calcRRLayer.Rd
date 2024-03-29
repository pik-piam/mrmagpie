% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRRLayer.R
\name{calcRRLayer}
\alias{calcRRLayer}
\title{calcRRLayer}
\usage{
calcRRLayer(cells = "lpjcell")
}
\arguments{
\item{cells}{number of cells to be returned: magpiecell (59199), lpjcell (67420)}
}
\value{
magpie object in cellular resolution
}
\description{
Function extracts range-rarity as used for biodiversity loss
}
\examples{
\dontrun{
calcOutput("RRLayer", aggregate = FALSE)
}

}
\author{
Patrick v. Jeetze
}
