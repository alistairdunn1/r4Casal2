#' @title error_value_table
#' @description This function will run through all observations and create a table by year and observation summarising type likelihood and error_values
#' @author Craig Marsh, A Dunn
#' @param model <casal2MPD> object that are generated from one of the extract() functions. If list then we expect multiple MPD runs (should be a named list)
#' @param as.table (if true) return the error values, process errors, and adjusted errors as tables, else returns a data frame
#' @importFrom tidyr pivot_wider expand
#' @return if as.table = TRUE, three data frames that can be used by ktable or other table functions, otherwise a data frame of all values suitable for plotting
#' @rdname error_value_table
#' @export error_value_table
#'
error_value_table <- function(model, as.table = TRUE) {
  complete_df <- NULL
  report_labels <- names(model)
  for (i in 1:length(model)) {
    if (report_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (any(names(this_report) == "type")) {
      if (this_report$type != "observation") {
        next
      }
      ## found an observation
      ##
      obs_type <- this_report$observation_type
      obs_likelihood <- this_report$likelihood
      obs_label <- report_labels[i]
      obs_error <- this_report$Values %>%
        group_by(year) %>%
        summarise(mean(error_value), mean(process_error), mean(adjusted_error))
      colnames(obs_error) <- c("year", "error_value", "process_error", "adjusted_error")
      temp_df <- data.frame(
        label = obs_label, type = obs_type, likelihood = obs_likelihood, error_value = obs_error$error_value,
        process_error = obs_error$process_error, error_value_multiplier = this_report$error_value_multiplier,
        likelihood_multiplier = this_report$likelihood_multiplier, adjusted_error = obs_error$adjusted_error, year = obs_error$year
      )
      complete_df <- rbind(complete_df, temp_df)
    }
  }
  chg_lab <- grepl(complete_df$type, pattern = "process_removals_by")
  complete_df$type[chg_lab] <- paste0("fishery_", substring(complete_df$type[chg_lab], first = 21))

  if (as.table) {
    ## make year a factor so we can expand it
    complete_df$year <- factor(complete_df$year, levels = seq(from = min(complete_df$year), to = max(complete_df$year), by = 1))
    ## now covert into a table somehow
    observation_error <- complete_df %>%
      group_by(year, label) %>%
      select(!c(process_error, adjusted_error)) %>%
      pivot_wider(names_from = year, names_expand = T, values_from = error_value, values_fill = NA)
    process_error <- complete_df %>%
      group_by(year, label) %>%
      select(!c(error_value, adjusted_error)) %>%
      pivot_wider(names_from = year, names_expand = T, values_from = process_error, values_fill = NA)
    adjusted_error <- complete_df %>%
      group_by(year, label) %>%
      select(!c(error_value, process_error)) %>%
      pivot_wider(names_from = year, names_expand = T, values_from = adjusted_error, values_fill = NA)
    df <- list(
      observation_error, process_error, this_report$error_value_multiplier, this_report$likelihood_multiplier,
      adjusted_error
    )
    names(df) <- c("observation_error", "process_error", "error_value_multiplier", "likelihood_multiplier", "adjusted_error")
  } else {
    df <- complete_df
  }
  return(df)
}
