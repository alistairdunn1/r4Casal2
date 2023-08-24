#' @title get_composition_mean_bin
#' @description
#' An accessor function that returns a data frame of mean age or length calculations
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple MPD runs (should be a named list )
#' @param ignore_plus_group <bool> we will assume the max age or length is a plus group and remove it.
#' @return dataframe with all mean age or length and standard errors
#' @rdname get_composition_mean_bin
#' @export get_composition_mean_bin
#' @details the dataframe returned has Oy: mean age/length observed, Ey: mean age/length predicted, SEy: Standard error, Nassumed: mean effective sample size.
#' Why use ignore_plus_group? not sure we need to chat to Jeremy and C Francis taken from the SNA code. My guess is that if there is a large plus group length or age, then this can skew the mean
#' statistic. At first thoughts I don't think it should be employed that frequently
"get_composition_mean_bin" <- function(model, ignore_plus_group = FALSE) {
  UseMethod("get_composition_mean_bin", model)
}

#' @rdname get_composition_mean_bin
#' @method get_composition_mean_bin casal2MPD
#' @export
get_composition_mean_bin.casal2MPD <- function(model, ignore_plus_group = FALSE) {
  comp_obs <- get_composition_observations(model)
  obs <- unique(comp_obs$observation_label)
  mean_bin_df <- NULL
  for (i in 1:length(obs)) {
    # age or length?
    this_obs <- comp_obs %>% filter(observation_label == obs[i])
    is_age <- grepl(unique(this_obs$observation_type), pattern = "_age")
    if (ignore_plus_group) {
      if (is_age) {
        this_obs <- this_obs %>% filter(age != max(age))
      } else {
        this_obs <- this_obs %>% filter(length != max(length))
      }
    }
    ## force proportions to sum = 1
    this_obs <- this_obs %>%
      group_by(year, observation_label) %>%
      mutate(expected = expected / sum(expected), observed = observed / sum(observed))

    if (is_age) {
      mean_age <- this_obs %>%
        group_by(year, observation_label) %>%
        summarise(Ey = sum(age * expected), Oy = sum(age * observed), E_squared_y = sum(age^2 * expected), Nassumed = mean(adjusted_error))
      mean_age$Ry <- mean_age$Oy - mean_age$Ey
      mean_age$SEy <- sqrt((mean_age$E_squared_y - mean_age$Ey^2) / mean_age$Nassumed)
      mean_bin_df <- rbind(mean_bin_df, mean_age)
    } else {
      mean_length <- this_obs %>%
        group_by(year, observation_label) %>%
        summarise(Ey = sum(length * expected), Oy = sum(length * observed), E_squared_y = sum(length^2 * expected), Nassumed = mean(adjusted_error))
      mean_length$Ry <- mean_length$Oy - mean_length$Ey
      mean_length$SEy <- sqrt((mean_length$E_squared_y - mean_length$Ey^2) / mean_length$Nassumed)
      mean_bin_df <- rbind(mean_bin_df, mean_length)
    }
    mean_bin_df$"Std.res" <- (mean_bin_df$Oy - mean_bin_df$Ey) / mean_bin_df$SEy
    ## I think this is the final Francis weighting value TODO: to check
    Nmult <- 1 / var(mean_bin_df$"Std.res", na.rm = TRUE)

    # Find the adjusted confidence intervals
    mean_bin_df$ObsloAdj <- mean_bin_df$Oy - 2 * mean_bin_df$SEy / sqrt(Nmult)
    mean_bin_df$ObshiAdj <- mean_bin_df$Oy + 2 * mean_bin_df$SEy / sqrt(Nmult)
  }
  return(mean_bin_df)
}

#' @rdname get_composition_mean_bin
#' @method get_composition_mean_bin list
#' @export
"get_composition_mean_bin.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance <- get_composition_mean_bin(model[[i]])
    if (!is.null(this_abundance)) {
      this_abundance$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_abundance)
    }
  }
  return(full_DF)
  invisible()
}
