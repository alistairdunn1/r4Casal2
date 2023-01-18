#'
#' This is an example which includes some R-code that will apply Francis Method TA 1.8 method
#' and automatically update a Casal2 estimation file

library(Casal2)
library(r4Casal2)
library(ggplot2)
library(dplyr)
csl_dir <- normalizePath(file.path("misc", "AutomateDataWeighting", "csls"))

mpd <- extract.mpd(file = "estimate.log", path = csl_dir)
obs_csl2 <- extract.csl2.file(file = "Observation.csl2", path = csl_dir)

names(mpd)

## comp labels
comp_labels <- c("chatTANage", "chatOBSwst", "chatOBSest")
## check weights
max_tolerance <- abs(1 - Method.TA1.8(model = mpd, observation_labels = comp_labels, plot.it = F))

# Before running this it always pays to save a copy of the original Observation.csl2
# in case an error occurs and deletes the file or something silly
## run this line of code
if (FALSE) {
  file.copy(from = file.path(csl_dir, "Observation.csl2"), to = file.path(csl_dir, "Observation_original.csl2"))
}

# first run an estimation
current_dir <- getwd()
setwd(csl_dir)
system2(command = "casal2", args = "-e", stdout = paste0("estimate_", 0, ".log"), stderr = "estimate.err")
setwd(current_dir)

# to be even more sure, it pays to change the config.csl2 to work off !include with a different file.
weighting_iterator <- 1
weighted_mpds <- list()
while (max_tolerance > 0.01) {
  cat("weighting loop index = ", weighting_iterator, " max tolerance = ", max_tolerance, "\n")
  ## change observation.csl2
  obs_csl2 <- extract.csl2.file(file = "Observation.csl2", path = csl_dir, quiet = T)
  # read in mpd
  mpd <- extract.mpd(file = paste0("estimate_", weighting_iterator - 1, ".log"), path = csl_dir)
  weighted_mpds[[as.character(round(max_tolerance, 3))]] <- mpd
  vals <- vector()
  ## loop over each comp data set and weight individually
  for (comp_ndx in 1:length(comp_labels)) {
    # calculate weight
    weight <- Method.TA1.8(model = mpd, observation_labels = comp_labels[comp_ndx], plot.it = F)
    # get the obs input file
    vals[comp_ndx] <- weight
    this_obs <- obs_csl2[[paste0("observation[", comp_labels[comp_ndx], "]")]]
    # change the subcommand 'error_value_multiplier'
    if (is.null(this_obs$error_value_multiplier)) {
      this_obs$error_value_multiplier <- list()
      this_obs$error_value_multiplier$value <- weight
    } else {
      this_obs$error_value_multiplier$value <- weight
    }
    # the francis method wants weight = 1.0

    # check if the tolerance approx = 1
    if (abs(1 - weight) > max_tolerance) {
      max_tolerance <- abs(1 - weight)
    }
    # save the observation back into the overall config
    obs_csl2[[paste0("observation[", comp_labels[comp_ndx], "]")]] <- this_obs
  }
  ## print the weights
  cat(comp_labels, "\n")
  cat(round(vals, 5), "\n")

  ## write the file
  ## Note!! this will overwrite 'Observation.csl2' it will strip out all comments as well
  write.csl2.file(obs_csl2, file = "Observation.csl2", path = csl_dir)

  ## estimate the model with Casal2
  current_dir <- getwd()
  setwd(csl_dir)
  system2(command = "casal2", args = "-e", stdout = paste0("estimate_", weighting_iterator, ".log"), stderr = "estimate.err")
  setwd(current_dir)
  weighting_iterator <- weighting_iterator + 1
}



## visualise the effects of the data weighting
## can tell us about how things change due to
## the weighting
ssbs <- get_dqs(weighted_mpds)

ggplot(ssbs, aes(x = as.numeric(years), y = values, col = model_label, linetype = model_label)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "SSB (t)") +
  ylim(0, NA)
