#' @title get_category_transitions
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of process type category_transitions
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_category_transitions
#' @export get_category_transitions
"get_category_transitions" <- function(model) {
  UseMethod("get_category_transitions", model)
}

#' @rdname get_category_transitions
#' @method get_category_transitions casal2MPD
#' @export
"get_category_transitions.casal2MPD" <- function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  reports_labels <- reformat_default_labels(names(model))
  for (i in 1:length(model)) {
    if (reports_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (exists(x = "type", where = this_report)) {
      if (tolower(this_report$type) != "process") {
        next
      }
      if (tolower(this_report$type) == "process" & tolower(this_report$sub_type) != "transition_category") {
        next
      }
      ## only a single trajectory
      temp_df <- data.frame(par_set = 1, label = reports_labels[i], from = this_report$from, to = this_report$to, proportions = this_report$proportions, selectivity = this_report$selectivities)

      complete_df <- rbind(complete_df, temp_df)
    } else {
      if (this_report[[1]]$type != "process") {
        next
      }
      if (this_report[[1]]$type == "process" & this_report[[1]]$sub_type != "transition_category") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)

      for (dash_i in 1:n_runs) {
        ## only a single trajectory
        temp_df <- data.frame(par_set = iter_labs[dash_i], label = reports_labels[i], from = this_report[[dash_i]]$from, to = this_report[[dash_i]]$to, proportions = this_report[[dash_i]]$proportions, selectivity = this_report[[dash_i]]$selectivities)
        complete_df <- rbind(complete_df, temp_df)
      }
    }
  }
  return(complete_df)
  invisible()
}

#' @rdname get_category_transitions
#' @method get_category_transitions list
#' @export
"get_category_transitions.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_category_transitions(model[[i]])
    if (!is.null(this_dq)) {
      this_dq$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_dq)
    }
  }
  return(full_DF)
  invisible()
}

#' @rdname get_category_transitions
#' @method get_category_transitions casal2TAB
#' @export
"get_category_transitions.casal2TAB" <- function(model) {
  return("function not written yet =(")
}
