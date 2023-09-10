#' @title get_aggregated_composition_values_by_blocks
#' @description An accessor function that returns a data frame of aggregated composition
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple MPD runs (should be a named list)
#' @param n_year_blocks integer specifying the number of years in a time-block to aggregate over
#' @return data frame with aggregated observed and expected over all years for each observation
#' @rdname get_aggregated_composition_values_by_blocks
#' @export
"get_aggregated_composition_values_by_blocks" <- function(model, n_year_blocks = 5) {
  UseMethod("get_aggregated_composition_values_by_blocks", model)
}

#' @rdname get_aggregated_composition_values_by_blocks
#' @method get_aggregated_composition_values_by_blocks casal2MPD
#' @export
"get_aggregated_composition_values_by_blocks.casal2MPD" <- function(model, n_year_blocks = 5) {
  comp_df <- get_composition_observations(model)
  comp_df$number_observed <- comp_df$observed * comp_df$adjusted_error
  comp_df$number_expected <- comp_df$expected * comp_df$adjusted_error
  year_range <- range(comp_df$year)
  year_blocks <- seq(from = year_range[1] - (year_range[1] %% n_year_blocks), to = year_range[2] + (n_year_blocks - (year_range[2] %% n_year_blocks)), by = n_year_blocks)
  year_block_label <- paste0(year_blocks[1:(length(year_blocks) - 1)], "-", year_blocks[2:(length(year_blocks))])
  comp_df$year_blocks <- cut(comp_df$year, breaks = year_blocks, labels = year_block_label)
  is_age <- grepl(comp_df$observation_type, pattern = "_age")
  comp_df$is_age <- is_age
  obs_label <- unique(comp_df$observation_label)
  full_summarised_df <- NULL
  for (obs_ndx in 1:length(obs_label)) {
    this_obs <- comp_df %>% filter(observation_label == obs_label[obs_ndx])

    if (all(this_obs$is_age)) {
      # age observation
      summarise_obs <- this_obs %>%
        group_by(age, category, year_blocks) %>%
        summarise(aggregated_observed = sum(number_observed), aggregated_expected = sum(number_expected))
    } else {
      # length observation
      summarise_obs <- this_obs %>%
        group_by(length, category, year_blocks) %>%
        summarise(aggregated_observed = sum(number_observed), aggregated_expected = sum(number_expected))
    }
    summarise_obs$observation_label <- obs_label[obs_ndx]
    full_summarised_df <- rbind(full_summarised_df, summarise_obs)
  }
  return(full_summarised_df)
}

#' @rdname get_aggregated_composition_values_by_blocks
#' @method get_aggregated_composition_values_by_blocks list
#' @export
"get_aggregated_composition_values_by_blocks.list" <- function(model, n_year_blocks = 5) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_aggregated_LF <- get_aggregated_composition_values_by_blocks(model[[i]], n_year_blocks = n_year_blocks)
    if (!is.null(this_aggregated_LF)) {
      this_aggregated_LF$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_aggregated_LF)
    }
  }
  return(full_DF)
}

#' @rdname get_aggregated_composition_values_by_blocksit_F
#' @method get_aggregated_composition_values_by_blocks casal2TAB
#' @export
"get_aggregated_composition_values_by_blocks.casal2TAB" <- function(model, n_year_blocks = 5) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  stop("get_aggregated_composition_values_by_blocks for casal2TAB has not been implemented")
}
