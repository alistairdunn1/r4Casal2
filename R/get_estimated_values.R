#' @title get_estimated_values
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of estimate_value
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with all estimate_value reports from Casal2 model output
#' @rdname get_estimated_values
#' @export get_estimated_values
#' @importFrom reshape2 melt

"get_estimated_values" <-
  function(model) {
    UseMethod("get_estimated_values", model)
  }

#'
#' @rdname get_estimated_values
#' @method get_estimated_values casal2MPD
#' @export
"get_estimated_values.casal2MPD" <- function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  reports_labels <- reformat_default_labels(names(model))
  for (i in 1:length(model)) {
    if (reports_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (any(names(this_report) == "type")) {
      if (this_report$type != "estimate_value") {
        next
      }
      temp_df <- data.frame(par_set = 1, parameters = names(this_report$values), values = as.numeric(this_report$values))
      complete_df <- rbind(complete_df, temp_df)
    } else {
      if (this_report[[1]]$type != "estimate_value") {
        next
      }
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        temp_df <- data.frame(par_set = iter_labs[dash_i], parameters = names(this_report[[dash_i]]$values), values = as.numeric(this_report[[dash_i]]$values))
        complete_df <- rbind(complete_df, temp_df)
      }
      complete_df$par_set <- factor(complete_df$par_set, ordered = T)
    }
  }
  return(complete_df)
  invisible()
}

#'
#' @rdname get_estimated_values
#' @method get_estimated_values list
#' @export
"get_estimated_values.list" <- function(model) {
  ## not written yet, but will need to deal with non overlapping parameters across models
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_estimated_values(model[[i]])
    if (is.null(this_dq)) {
      next
    }
    this_dq$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
  invisible()
}
