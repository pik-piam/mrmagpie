% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readGCMClimate.R
\name{readGCMClimate}
\alias{readGCMClimate}
\title{readGCMClimate}
\usage{
readGCMClimate(subtype = "HadGEM2_ES:rcp8p5.temperature")
}
\arguments{
\item{subtype}{Switch between different inputs}
}
\value{
MAgPIE objects with results on cellular level.
}
\description{
Read Climate data used as LPJmL inputs into MAgPIE objects
}
\examples{

\dontrun{
readSource("GCMClimate", subtype="HadGEM2_ES:rcp8p5.temperature", convert="onlycorrect")
}

}
\seealso{
\code{\link{readGCMClimate}}
}
\author{
Marcos Alves, Kristine Karstens
}
