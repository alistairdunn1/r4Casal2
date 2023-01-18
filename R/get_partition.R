#' @title get_partition
#'
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of process type partition
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_partition
#' @export get_partition
#' @importFrom reshape2 melt


"get_partition" <-
  function(model) {
    UseMethod("get_partition", model)
  }

#'
#' @rdname get_partition
#' @method get_partition casal2MPD
#' @export
"get_partition.casal2MPD" <- function(model) {
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
      if (this_report$type != "partition") {
        next
      }
      temp_df <- NULL
      years <- names(this_report)
      for (y in 1:length(years)) {
        if (years[y] == "type") {
          next
        }
        this_df <- melt(as.matrix(this_report[[y]]$values))
        colnames(this_df) <- c("category", "bin", "value")
        this_df$year <- years[y]
        this_df$time_step <- this_report[[y]]$time_step
        temp_df <- rbind(temp_df, this_df)
      }
      temp_df$par_set <- 1
      complete_df <- rbind(complete_df, temp_df)
    } else {
      if (this_report[[1]]$type != "partition") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      for (dash_i in 1:n_runs) {
        temp_df <- NULL
        this_par_df <- this_report[[dash_i]]
        years <- names(this_par_df)
        for (y in 1:length(years)) {
          if (years[y] == "type") {
            next
          }
          this_df <- melt(as.matrix(this_par_df[[y]]$values))
          colnames(this_df) <- c("category", "bin", "value")
          this_df$year <- years[y]
          this_df$time_step <- this_report[[y]]$time_step
          temp_df <- rbind(temp_df, this_df)
        }
        temp_df$par_set <- dash_i
        complete_df <- rbind(complete_df, temp_df)
      }
    }
  }
  return(complete_df)
  invisible()
}

#'
#' @rdname get_partition
#' @method get_partition list
#' @export
"get_partition.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_partition(model[[i]])
    this_dq$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
  invisible()
}
