% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_derived_quantities.R
\name{get_dqs}
\alias{get_dqs}
\title{get_dqs}
\usage{
get_dqs(model)
}
\arguments{
\item{model}{<casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions}

\item{reformat_labels}{<bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)}
}
\value{
A data frame with all derived quantity reports from Casal2 model output
}
\description{
An accessor function that returns a data frame from a Casal2 model output of derived_quantities
}
\author{
Craig Marsh
}
