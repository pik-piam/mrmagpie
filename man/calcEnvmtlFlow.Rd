% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcEnvmtlFlow.R
\name{calcEnvmtlFlow}
\alias{calcEnvmtlFlow}
\title{calcEnvmtlFlow}
\usage{
calcEnvmtlFlow(
  lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop =
    "ggcmi_phase3_nchecks_9ca735cb"),
  climatetype = "GSWP3-W5E5:historical",
  stage = "harmonized2020",
  seasonality = "grper"
)
}
\arguments{
\item{lpjml}{Defines LPJmL version for crop/grass and natveg specific inputs}

\item{climatetype}{Switch between different climate scenarios}

\item{stage}{Degree of processing: raw, smoothed, harmonized, harmonized2020}

\item{seasonality}{grper (default): EFR in growing period per year; total:
EFR throughout the year; monthly: monthly EFRs}
}
\value{
magpie object in cellular resolution
}
\description{
This function calculates environmental flow requirements (EFR) for MAgPIE
             retrieved from LPJmL monthly discharge and water availability
}
\examples{
\dontrun{
calcOutput("EnvmtlFlow", aggregate = FALSE)
}

}
\author{
Felicitas Beier
}
