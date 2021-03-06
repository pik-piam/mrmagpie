% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPastYields.R
\name{calcPastYields}
\alias{calcPastYields}
\title{calcPastYields}
\usage{
calcPastYields(
  lsu_levels = c(seq(0, 2, 0.2), 2.5),
  past_mngmt = "2me",
  lpjml = c(range = "LPJmL_range", pastr = "LPJmL_pastr"),
  climatetype = "HadGEM2_ES:rcp8p5:co2"
)
}
\arguments{
\item{lsu_levels}{Livestock unit levels in the source folder}

\item{past_mngmt}{pasture areas management option}

\item{lpjml}{Defines LPJmL version for crop/grass and natveg specific inputs}

\item{climatetype}{Switch between different climate scenarios}
}
\value{
magpie object in cellular resolution
}
\description{
Calculates rangelands maximum output and managed pastures yields
}
\examples{
\dontrun{
calcOutput("PastYields", lsu_levels, past_mngmt = "2me", lpjml, climatetype)
}

}
\author{
Marcos Alves
}
