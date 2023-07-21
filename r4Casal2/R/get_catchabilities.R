#' @title get_catchabilities
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of catchabilities
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with all derived quantity reports from Casal2 model output
#' @rdname get_catchabilities
#' @export get_catchabilities
#' @importFrom reshape2 melt

"get_catchabilities" <-
  function(model) {
    UseMethod("get_catchabilities", model)
  }

#'
#' @rdname get_catchabilities
#' @method get_catchabilities casal2MPD
#' @export
"get_catchabilities.casal2MPD" <- function(model) {
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
      if (this_report$type != "catchability") {
        next
      }
      temp_df <- data.frame(par_set = 1, label = reports_labels[i], catchability = this_report$q)
      complete_df <- rbind(complete_df, temp_df)
    } else {
      if (this_report[[1]]$type != "catchability") {
        next
      }
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        temp_df <- data.frame(par_set = iter_labs[dash_i], label = reports_labels[i], catchability = this_report[[dash_i]]$q)
        complete_df <- rbind(complete_df, temp_df)
      }
      complete_df$par_set <- factor(complete_df$par_set, ordered = T)
    }
  }
  return(complete_df)
  invisible()
}

#'
#' @rdname get_catchabilities
#' @method get_catchabilities list
#' @export
"get_catchabilities.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_df <- get_catchabilities(model[[i]])
    if (is.null(this_df)) {
      next
    }
    this_df$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_df)
  }
  return(full_DF)
  invisible()
}

#'
#' @rdname get_catchabilities
#' @method get_catchabilities casal2TAB
#' @export
"get_catchabilities.casal2TAB" <- function(model) {
  reports_labels <- reformat_default_labels(names(model))
  complete_df <- NULL

  for (i in 1:length(model)) {
    this_report <- model[[i]]
    if (is.null(this_report$type) || reports_labels[i] != "catchabilities") {
      next
    }
    val_df <- this_report$values
    val_molten <- suppressMessages({
      melt(as.matrix(val_df), variable.name = "colname", value.name = "catchability", factorsAsStrings = F)
    })
    colnames(val_molten) <- c("iteration", "parameter", "value")
    val_molten$report_label <- reports_labels[i]
    complete_df <- rbind(complete_df, val_molten)
  }
  return(complete_df)
  invisible()
}
