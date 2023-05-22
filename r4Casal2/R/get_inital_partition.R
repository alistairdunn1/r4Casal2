#' @title get_initial_partition
#' @description An accessor function that returns a data frame from a Casal2 model output of process type initialisation_partition
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_initial_partition
#' @export
#' @importFrom reshape2 melt
"get_initial_partition" <-
  function(model) {
    UseMethod("get_initial_partition", model)
  }

#' @rdname get_initial_partition
#' @method get_initial_partition casal2MPD
#' @export
"get_initial_partition.casal2MPD" <- function(model) {
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
      if (this_report$type != "initialisation_partition") {
        next
      }
      this_df <- melt(as.matrix(this_report$values))
      colnames(this_df) <- c("category", "bin", "value")
      this_df$par_set <- 1
      this_df$label <- reports_labels[i]
      complete_df <- rbind(complete_df, this_df)
    } else {
      if (this_report[[1]]$type != "initialisation_partition") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      for (dash_i in 1:n_runs) {
        ## only a single trajectory
        this_df <- melt(as.matrix(this_report[[dash_i]]$values))
        colnames(this_df) <- c("category", "bin", "value")
        this_df$par_set <- dash_i
        this_df$label <- reports_labels[i]
        complete_df <- rbind(complete_df, this_df)
      }
    }
  }
  return(complete_df)
  invisible()
}

#' @rdname get_initial_partition
#' @method get_initial_partition list
#' @export
"get_initial_partition.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_initial_partition(model[[i]])

    if (is.null(this_dq)) {
      next
    }
    this_dq$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
  invisible()
}
