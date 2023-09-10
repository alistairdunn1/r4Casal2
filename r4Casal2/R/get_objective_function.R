#' @title get_objective_function
#' @details Takes a Casal2 objective_function report and aggregate components so easier to handle with visualising likelihood components
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @param aggregate_obs <bool> whether to aggregate over observations, if FALSE will report objective function by year and observation
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return data frame of objective function negative log-likelihood components
#' @rdname get_objective_function
#' @export get_objective_function
"get_objective_function" <- function(model, ...) {
  UseMethod("get_objective_function", model)
}

#' @rdname get_objective_function
#' @method get_objective_function casal2MPD
#' @export
"get_objective_function.casal2MPD" <- function(model, aggregate_obs = TRUE, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  full_df <- NULL
  for (i in 1:length(model)) {
    if (report_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (exists(x = "type", where = this_report)) {
      if (tolower(this_report$type) != "objective_function") {
        next
      }
      if (aggregate_obs) {
        return(aggregate_single_objective_report(this_report$values))
      } else {
        return(this_report$values)
      }
    } else {
      if (tolower(this_report[[1]]$type) != "objective_function") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        this_par_set <- NULL
        if (aggregate_obs) {
          this_par_set <- aggregate_single_objective_report(this_report[[dash_i]]$values)
          if (dash_i == 1) {
            full_df <- this_par_set
          } else {
            full_df <- cbind(full_df, this_par_set$negative_loglik)
          }
        } else {
          if (dash_i == 1) {
            full_df <- data.frame(components = names(this_report[[dash_i]]$values), val = as.numeric(this_report[[dash_i]]$values))
          } else {
            full_df <- cbind(full_df, as.numeric(this_report[[dash_i]]$values))
          }
        }
      }
      return(full_df)
    }
  }
}

#' @rdname get_objective_function
#' @method get_objective_function list
#' @export
"get_objective_function.list" <- function(model, aggregate_obs = TRUE, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  if (!is.list(model)) {
    stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
  }
  report_labels <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_objective_function(model[[i]], aggregate_obs = aggregate_obs, reformat_labels = reformat_labels)
    this_dq$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
}

#' @rdname get_objective_function
#' @method get_objective_function casal2TAB
#' @export
"get_objective_function.casal2TAB" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  stop("get_objective_function for casal2TAB has not been implemented")
}
