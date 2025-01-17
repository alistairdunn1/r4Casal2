% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_estimated_parameters.R
\name{summarise_estimated_parameters}
\alias{summarise_estimated_parameters}
\alias{summarise_estimated_parameters.casal2MPD}
\title{summarise_estimated_parameters}
\usage{
summarise_estimated_parameters(model, plot_it = F, ignore_ycs = F)

\method{summarise_estimated_parameters}{casal2MPD}(model, plot_it = F, ignore_ycs = F)
}
\arguments{
\item{model}{an MPD model}

\item{plot_it}{if true plot MPD value, initial value, and prior, otherwise report MPD, init, lower_bound, upper_bound, prior_type}

\item{ignore_ycs}{ignore YCS parameters}
}
\value{
either a ggplot or a list with two elements. The first is for plotting, the second is a useful data frame
outlining priors, bounds mpd values initial values etc.
}
\description{
this function will find estimate_summary report and summarise the priors, MPD and initial value
}
\author{
C Marsh
}
