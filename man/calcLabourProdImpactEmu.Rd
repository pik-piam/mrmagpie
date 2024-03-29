% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcLabourProdImpactEmu.R
\name{calcLabourProdImpactEmu}
\alias{calcLabourProdImpactEmu}
\title{calcLabourProdImpactEmu}
\usage{
calcLabourProdImpactEmu(
  timestep = "5year",
  cellular = TRUE,
  subtype = "impact",
  cells = "lpjcell"
)
}
\arguments{
\item{timestep}{5-year or yearly}

\item{cellular}{cellular is true}

\item{subtype}{impact for rcp based laborprod decrease, relief for LCLM based relief of impact}

\item{cells}{"magpiecell" or "lpjcell"}
}
\value{
List of magpie object of gridded (0.5) labour productivity as percentage of full labour prod 1
}
\description{
Spatial and temporal aggr. of labour productivity impacts from climate change emulated by LAMACLIMA

based on method of Orlov et al. 2019. Economics of Disasters and Climate Change, 3(3), 191-211.
}
\author{
Michael Windisch, Florian Humpenöder
}
