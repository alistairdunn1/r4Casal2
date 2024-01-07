#' @title get_selectivities_by_year
#' @description An accessor function that returns a data frame from a Casal2 model output of selectivities
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return A data frame with all selectivity reports from Casal2 model output
#' @rdname get_selectivities_by_year
#' @export get_selectivities_by_year
#' @importFrom reshape2 melt
#'
"get_selectivities_by_year" <- function(model, ...) {
  UseMethod("get_selectivities_by_year", model)
}

#' @rdname get_selectivities_by_year
#' @method get_selectivities_by_year casal2MPD
#' @export
"get_selectivities_by_year.casal2MPD" <- function(model, reformat_labels = TRUE) {
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
      if (this_report$type != "selectivity_by_year") {
        next
      }
      temp_df <- NULL
      years <- names(this_report)
      for (y in 1:length(years)) {
        if (years[y] == "type") {
          next
        }
        this_df <- melt(as.matrix(this_report[[y]]$Values))
        colnames(this_df) <- c("bin", "par_set", "selectivity")
        this_df$type <- this_report[[y]]$sub_type
        this_df$selectivity_label <- this_report[[y]]$selectivity
        this_df$year <- years[y]
        temp_df <- rbind(temp_df, this_df)
      }
      complete_df <- rbind(complete_df, temp_df)
    } else {
      multiple_iterations_in_a_report <- TRUE
      if (this_report[[1]]$type != "selectivity_by_year") {
        next
      }
      n_runs <- length(this_report)
      iter_labs <- names(this_report)

      for (dash_i in 1:n_runs) {
        temp_df <- NULL
        this_par_df <- this_report[[dash_i]]
        years <- names(this_par_df)
        for (y in 1:length(years)) {
          if (years[y] == "type") {
            next
          }
          this_df <- melt(as.matrix(this_par_df[[y]]$Values))
          colnames(this_df) <- c("bin", "par_set", "selectivity")
          this_df$type <- this_par_df[[y]]$sub_type
          this_df$selectivity_label <- this_par_df[[y]]$selectivity
          this_df$year <- years[y]
          temp_df <- rbind(temp_df, this_df)
        }
        temp_df$par_set <- iter_labs[dash_i]
        complete_df <- rbind(complete_df, temp_df)
      }
    }
  }
  complete_df$bin <- as.numeric(complete_df$bin)
  return(complete_df)
}

#' @rdname get_selectivities_by_year
#' @method get_selectivities_by_year list
#' @export
"get_selectivities_by_year.list" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD', you supplied = ", class(model[[i]])))
    }
    this_sel <- get_selectivities_by_year(model[[i]])
    this_sel$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_sel)
  }
  return(full_DF)
}

#' @rdname get_selectivities_by_year
#' @method get_selectivities_by_year list
#' @export
"get_selectivities_by_year.list" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  stop("get_selectivities_by_year is not implemented for casal2TAB output")
}
