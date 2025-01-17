% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tag_recapture_observations.R
\name{get_tag_recapture_observations}
\alias{get_tag_recapture_observations}
\alias{get_tag_recapture_observations.casal2MPD}
\alias{get_tag_recapture_observations.list}
\alias{get_tag_recapture_observations.casal2TAB}
\title{get_tag_recapture_observations}
\usage{
get_tag_recapture_observations(model)

\method{get_tag_recapture_observations}{casal2MPD}(model, reformat_labels = TRUE)

\method{get_tag_recapture_observations}{list}(model, reformat_labels = TRUE)

\method{get_tag_recapture_observations}{casal2TAB}(model, reformat_labels = TRUE)
}
\arguments{
\item{model}{<casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple MPD runs (should be a named list)}
}
\value{
data frame with all observations of type == 'observation' and observation_type %in% c('biomass', 'abundance')
}
\description{
An accessor function that returns a data frame of all tag-recapture observations
}
\author{
Craig Marsh
}
