#'
#' calculate_composition_stage_two_weights
#' @description this function will iterate over all (or a subset of) compositional observations to calculate the method TA 1.8 weights from \insertCite{francis2011data}{r4Casal2}
#' @param model casal2 mpd model
#' @param observation_labels vector<string> of observation labels to calculate the stage two weights for. This function assumes your observation label (@observation) is the same as the report label (@report)
#' @param approximate_single_year_obs <bool> see details
#' @return data.frame of second stage weights
#' @importFrom Rdpack reprompt
#' @importFrom Casal2 Method.TA1.8
#' @rdname calculate_composition_stage_two_weights
#' @export calculate_composition_stage_two_weights
#' @details if approximate_single_year_obs then we will search across other observations and years to find a an error value similar in magnitude for the year with a single observation
#' and assign a weight based on the year that is similar in magnitude.
#' @references
#' \insertAllCited{}
calculate_composition_stage_two_weights <- function(model, observation_labels = NULL, approximate_single_year_obs = FALSE) {
  # model = Casal2::extract.mpd("C:\\Users\\marshc\\OneDrive - NIWA\\22_23\\SNA1\\csl\\P30 Base HGBP\\Casal2\\estimate.log")
  comp_obs <- get_composition_observations(model)
  comp_labels <- unique(comp_obs$observation_label)
  if (!is.null(observation_labels)) {
    if (any(!(observation_labels %in% comp_labels))) {
      stop("Could not find some of the observation_labels supplied in model. Please check this or this function.")
    }
    comp_labels <- subset(comp_labels, subset = comp_labels %in% observation_labels)
  }
  weights <- vector()
  Ns <- NULL
  NA_ns <- vector()
  for (i in 1:length(comp_labels)) {
    this_obs <- subset(comp_obs, subset = comp_obs$observation_label == comp_labels[i])
    mean_N_by_year <- tapply(this_obs$adjusted_error, this_obs$year, mean)
    weights[i] <- Method.TA1.8(model = model, observation_labels = comp_labels[i], plot.it = F)
    if (!is.na(weights[i])) {
      temp_df <- data.frame(year = names(mean_N_by_year), N = mean_N_by_year, label = comp_labels[i], weight = weights[i])
      Ns <- rbind(Ns, temp_df)
      NA_ns[i] <- NA
    } else {
      ## usually NA due to only a single year
      NA_ns[i] <- (mean_N_by_year)
    }
  }
  if (approximate_single_year_obs & any(is.na(weights))) {
    for (i in 1:length(weights)) {
      if (is.na(weights[i])) {
        ndx <- which.min(abs(Ns$N - NA_ns[i]))
        weights[i] <- Ns$weight[ndx]
      }
    }
  }

  return(data.frame(observation = comp_labels, weight = weights))
}
