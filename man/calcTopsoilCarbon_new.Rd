% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcTopsoilCarbon_new.R
\name{calcTopsoilCarbon_new}
\alias{calcTopsoilCarbon_new}
\title{calcTopsoilCarbon_new}
\usage{
calcTopsoilCarbon_new(
  lpjml = c(natveg = "LPJmL4_for_MAgPIE_44ac93de", crop =
    "ggcmi_phase3_nchecks_9ca735cb"),
  climatetype = "GSWP3-W5E5:historical",
  fromFlows = FALSE
)
}
\arguments{
\item{lpjml}{Defines LPJmL version for crop/grass and natveg specific inputs}

\item{climatetype}{Switch between different GCM climate scenarios}

\item{fromFlows}{TRUE, if calculated from harmonized flows}
}
\value{
magpie object in cellular resolution
}
\description{
This function extracts topsoil carbon densities from LPJ to MAgPIE
}
\examples{
\dontrun{ calcOutput("TopsoilCarbon", aggregate = FALSE) }

}
\author{
Kristine Karstens
}
