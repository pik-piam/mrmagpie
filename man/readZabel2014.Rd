% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readZabel2014.R
\name{readZabel2014}
\alias{readZabel2014}
\title{readZabel2014}
\usage{
readZabel2014(subtype = "all_marginal")
}
\arguments{
\item{subtype}{The different options are:
\itemize{
\item \code{"all_marginal"}: Of the total marginal land (suitability index = 0.0 - 0.33), areas with an index of 0.1 and lower are excluded.
\item \code{"q33_marginal"}: The bottom tertile (suitability index below 0.13) of the marginal land area is excluded.
\item \code{"q50_marginal"}: The bottom  half (suitability index below 0.18) of the marginal land area is excluded.
\item \code{"q66_marginal"}: The first and second tertile (suitability index below 0.23) of the marginal land area are excluded.
\item \code{"q75_marginal"}: The first, second and third quartiles (suitability index below 0.25) of the marginal land are are excluded
\item \code{"no_marginal"}: Areas with a suitability index of 0.33 and lower are excluded.
}}
}
\value{
Returns magpie objects with the share of suitable cropland per grid cell
}
\description{
Reads crop suitability data published in Zabel, F., Putzenlechner, B., & Mauser, W. (2014). Global Agricultural Land Resources – A High Resolution Suitability Evaluation and Its Perspectives until 2100 under Climate Change Conditions. PLOS ONE, 9(9), e107522. https://doi.org/10.1371/journal.pone.0107522 and extracts the share of suitable cropland per grid cell, depending on different suitability thresholds.
}
\examples{
\dontrun{
readSource("Zabel2014", subtype = "all_marginal", convert = "onlycorrect")
}


}
\author{
Patrick v. Jeetze
}
