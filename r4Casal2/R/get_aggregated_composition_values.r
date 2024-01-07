#' @title get_aggregated_composition_values
#' @description An accessor function that returns a dataframe of aggregated composition
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple MPD runs (should be a named list)
#' @return data frame with aggregated observed and expected over all years for each observation
#' @rdname get_aggregated_composition_values
#' @export
#'
"get_aggregated_composition_values" <- function(model) {
  UseMethod("get_aggregated_composition_values", model)
}

#' @rdname get_aggregated_composition_values
#' @method get_aggregated_composition_values casal2MPD
#' @export
"get_aggregated_composition_values.casal2MPD" <- function(model) {
  comp_df <- get_composition_observations(model)
  comp_df$number_observed <- comp_df$observed * comp_df$adjusted_error
  comp_df$number_expected <- comp_df$expected * comp_df$adjusted_error

  is_age <- grepl(comp_df$observation_type, pattern = "_age")
  comp_df$is_age <- is_age
  obs_label <- unique(comp_df$observation_label)
  full_summarised_df <- NULL
  for (obs_ndx in 1:length(obs_label)) {
    this_obs <- comp_df %>% filter(observation_label == obs_label[obs_ndx])

    if (all(this_obs$is_age)) {
      # age observation
      summarise_obs <- this_obs %>%
        group_by(age, category) %>%
        summarise(aggregated_observed = sum(number_observed), aggregated_expected = sum(number_expected))
    } else {
      # length observation
      summarise_obs <- this_obs %>%
        group_by(length, category) %>%
        summarise(aggregated_observed = sum(number_observed), aggregated_expected = sum(number_expected))
    }
    summarise_obs$observation_label <- obs_label[obs_ndx]
    full_summarised_df <- rbind(full_summarised_df, summarise_obs)
  }
  return(full_summarised_df)
}

#' @rdname get_aggregated_composition_values
#' @method get_aggregated_composition_values list
#' @export
"get_aggregated_composition_values.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_aggregated_LF <- get_aggregated_composition_values(model[[i]])
    if (!is.null(this_aggregated_LF)) {
      this_aggregated_LF$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_aggregated_LF)
    }
  }
  return(full_DF)
}
