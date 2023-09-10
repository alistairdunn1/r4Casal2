#' @title get_estimated_values
#' @description An accessor function that returns a data frame from a Casal2 model output of estimate_value
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return A data frame with all estimate_value reports from Casal2 model output
#' @rdname get_estimated_values
#' @export get_estimated_values
#' @importFrom reshape2 melt
"get_estimated_values" <- function(model, ...) {
  UseMethod("get_estimated_values", model)
}

#' @rdname get_estimated_values
#' @method get_estimated_values casal2MPD
#' @export
"get_estimated_values.casal2MPD" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  for (i in 1:length(model)) {
    if (report_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (any(names(this_report) == "type")) {
      if (this_report$type != "estimate_value") {
        next
      }
      temp_df <- data.frame(par_set = 1, parameter = names(this_report$values), value = as.numeric(this_report$values))
      complete_df <- rbind(complete_df, temp_df)
    } else {
      if (this_report[[1]]$type != "estimate_value") {
        next
      }
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        temp_df <- data.frame(par_set = iter_labs[dash_i], parameter = names(this_report[[dash_i]]$values), value = as.numeric(this_report[[dash_i]]$values))
        complete_df <- rbind(complete_df, temp_df)
      }
      complete_df$par_set <- factor(complete_df$par_set, ordered = T)
    }
  }
  return(complete_df)
}

#' @rdname get_estimated_values
#' @method get_estimated_values list
#' @export
"get_estimated_values.list" <- function(mode, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  ## not written yet, but will need to deal with non overlapping parameters across models
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_estimated_values(model[[i]], reformat_labels = reformat_labels)
    if (is.null(this_dq)) {
      next
    }
    this_dq$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
}

#' @rdname get_estimated_values
#' @method get_estimated_values casal2TAB
#' @export
"get_estimated_values.casal2TAB" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  complete_df <- NULL
  for (i in 1:length(model)) {
    this_report <- model[[i]]
    if (is.null(this_report$type) || this_report$type != "estimate_value") {
      next
    }
    val_df <- this_report$values
    val_df$iteration <- as.numeric(rownames(val_df))

    val_molten <- suppressMessages({
      melt(as.matrix(val_df), variable.name = "colname", value.name = "estimate", factorsAsStrings = F)
    })
    colnames(val_molten) <- c("iteration", "parameter", "value")
    val_molten$report_label <- report_labels[i]
    complete_df <- rbind(complete_df, val_molten)
  }
  return(complete_df)
}
