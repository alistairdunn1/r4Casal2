#' @title get_timevarying_parameters
#' @description
#' An accessor function that returns a data frame of a time_varying class that can be easily plotted
#' @author Craig Marsh
#' @param model <casal2MPD> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with profile_values and likelihood components
#' @rdname get_timevarying_parameters
#' @export get_timevarying_parameters
"get_timevarying_parameters" <- function(model, ...) {
  UseMethod("get_timevarying_parameters", model)
}

#' @rdname get_timevarying_parameters
#' @method get_timevarying_parameters casal2MPD
#' @export
get_timevarying_parameters.casal2MPD <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  complete_df <- NULL
  for (i in 1:length(model)) {
    if (reports_label[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (exists(x = "type", where = this_report)) {
      if (this_report$type != "time_varying") {
        next
      }
      ## these reports are bespoke which sucks
      this_df <- this_report$values
      this_df$label <- report_labels[i]
      this_df$par_set <- 1
      complete_df <- rbind(complete_df, this_df)
    } else {
      if (this_report[[1]]$type != "time_varying") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        ## only a single trajectory
        temp_df <- this_report[[dash_i]]$values
        temp_df$label <- report_labels[i]
        temp_df$par_set <- iter_labs[dash_i]
        complete_df <- rbind(complete_df, temp_df)
      }
    }
  }
  return(complete_df)
}

#' @rdname get_timevarying_parameters
#' @method get_timevarying_parameters list
#' @export
"get_timevarying_parameters.list" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_df <- get_timevarying_parameters(model[[i]], reformat_labels = reformat_labels)
    if (is.null(this_df)) {
      next
    }
    this_df$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_df)
  }
  return(full_DF)
  invisible()
}

#' @rdname get_timevarying_parameters
#' @method get_timevarying_parameters casal2MPD
#' @export
"get_timevarying_parameters.casal2MPD" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  stop("get_timevarying_parameters is not implemented for casal2TAB output")
}
