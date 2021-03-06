% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcLabourProdImpact.R
\name{calcLabourProdImpact}
\alias{calcLabourProdImpact}
\title{calcLabourProdImpact}
\usage{
calcLabourProdImpact(timestep = "5year", subtype = "Orlov", cellular = TRUE)
}
\arguments{
\item{timestep}{5year or yearly}

\item{subtype}{data source comes from}

\item{cellular}{cellular is true}
}
\value{
List of magpie objects with results on 0.5deg grid level, weights based on production value, unit (ratio) and description.
}
\description{
Labour productivity impacts
}
\author{
David Chen
}
