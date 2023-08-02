#' @title aggregate_objective_report
#' @details
#' take a Casal2 objective_function report and aggregate components so easier to handle with
#' visualising likelihood components
#' @param objective_report casal2_objective_function which is a report from an extract.mpd() object
#' @return data frame of aggregated objective function negative log-likelihood components
#' @rdname aggregate_objective_report
#' @export aggregate_objective_report
aggregate_objective_report <- function(objective_report) {
  multi_parameter_input <- F
  if (is.null(objective_report$type)) {
    ## possible -i input
    if (is.null(objective_report[[1]]$type)) {
      stop("Unknown format for objective_report")
    }
    if (objective_report[[1]]$type != "objective_function") {
      stop("objective_report must be of type  'objective_function'")
    }
    multi_parameter_input <- T
  } else if (objective_report$type != "objective_function") {
    stop("objective_report must be of type  'objective_function'")
  }

  if (!multi_parameter_input) {
    return(aggregate_single_objective_report(objective_report$values))
  } else {
    full_df <- NULL
    for (i in 1:length(objective_report)) {
      this_par_set <- aggregate_single_objective_report(objective_report[[i]]$values)
      if (i == 1) {
        full_df <- this_par_set
      } else {
        full_df <- cbind(full_df, this_par_set$negative_loglik)
      }
    }
    return(full_df)
  }
}

#' aggregate_single_objective_report
#' @details used by aggregate_objective_report
#' @param values data.frame from a Casal2 objective_function report
#' @return data frame with aggregated values
aggregate_single_objective_report <- function(values) {
  obj_labels <- names(values)
  type <- Reduce(c, lapply(strsplit(obj_labels, split = "->", fixed = T), FUN = function(x) {
    x[1]
  }))
  label <- Reduce(c, lapply(strsplit(obj_labels, split = "-", fixed = T), FUN = function(x) {
    x[2]
  }))
  label <- substring(label, first = 2)
  new_label <- ifelse(!is.na(label), paste0("-", label), "")
  unique_vals <- paste0(type, new_label)
  aggregate_labs <- unique(unique_vals)
  ll_values <- vector()
  ## observations
  for (i in 1:length(aggregate_labs)) {
    ndx <- unique_vals %in% aggregate_labs[i]
    ll_values[i] <- sum(values[ndx])
  }
  obj_df <- data.frame(component = aggregate_labs, negative_loglik = ll_values)

  return(obj_df)
}
