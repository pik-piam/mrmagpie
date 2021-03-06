% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toolPatternScaling.R
\name{toolPatternScaling}
\alias{toolPatternScaling}
\title{toolPatternScaling}
\usage{
toolPatternScaling(
  scen,
  scen_mean,
  ref_mean,
  ref_year = "y2010",
  variation = "yieldCalibMAG"
)
}
\arguments{
\item{scen}{time series of the scenario}

\item{scen_mean}{mean of scenario time series}

\item{ref_mean}{mean of reference time series}

\item{ref_year}{Reference year}

\item{variation}{'yieldCalibMAG' (default); to be implemented: 'jensPaper'}
}
\value{
scaled data in magclass format
}
\description{
This tool scales time series based on the approach used in the magpiemodel yield module.
}
\author{
Kristine Karstens
}
