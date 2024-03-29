% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readWindisch2021.R
\name{readWindisch2021}
\alias{readWindisch2021}
\title{readWindisch2021}
\usage{
readWindisch2021(subtype)
}
\arguments{
\item{subtype}{refordefor_BPHonly_05_new, annmean_pertCha_05_EW1, annstd_diff_pertCha_05_EW1}
}
\value{
List of magpie objects with results on cellular level, weight, unit and description.
}
\description{
Reads in data to calculate BphEffect, BphTCRE or BphMask depending on the chosen subtype.
             BphEffect: Biogeophysical temperature change of afforestation (degree C).
             (File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018).
             BphMask: Mask of Datapoints of biogeophysical temperature change of afforestation (degree C)
             to be used as weight.
             (File is based on observation datasets of Bright et al. 2017 and Duveiller et al. 2018).
             BphTCRE: Transient Climate Response to accumulated doubling of CO2.
             (File is based on CMIP5 +1perc CO2 per year experiment.
             To be used in the translation to carbon equivalents of BphEffect)
}
\examples{

\dontrun{
  readSource("Windisch2021", convert="onlycorrect")
}

}
\author{
Felicitas Beier, Michael Windisch, Patrick v. Jeetze
}
