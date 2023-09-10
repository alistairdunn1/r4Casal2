#' @title get_derived_quantities
#' @description An accessor function that returns a data frame from a Casal2 model output of derived_quantities
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return A data frame with all derived quantity reports from Casal2 model output
#' @rdname get_derived_quantities
#' @export get_derived_quantities
#' @importFrom reshape2 melt
"get_derived_quantities" <- function(model, ...) {
  UseMethod("get_derived_quantities", model)
}
## shorthand version
#' @title get_dqs
#' @description An accessor function that returns a data frame from a Casal2 model output of derived_quantities
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return A data frame with all derived quantity reports from Casal2 model output
#' @rdname get_dqs
#' @export get_dqs
"get_dqs" <- function(model) {
  UseMethod("get_derived_quantities", model)
}

#' @rdname get_derived_quantities
#' @method get_derived_quantities casal2MPD
#' @export
"get_derived_quantities.casal2MPD" <- function(model, reformat_labels = TRUE) {
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
      if (this_report$type != "derived_quantity") {
        next
      }
      DQ_types <- names(this_report)[!names(this_report) %in% "type"]
      dq_df <- NULL
      for (dq_iter in 1:length(DQ_types)) {
        temp_df <- data.frame(par_set = 1, years = names(this_report[[DQ_types[dq_iter]]]$values), values = this_report[[DQ_types[dq_iter]]]$values, initialisation_value = this_report[[DQ_types[dq_iter]]]$`initialisation_phase[1]`, dq_label = DQ_types[dq_iter])
        if (exists("initialisation_phase[2]", this_report[[DQ_types[dq_iter]]])) {
          temp_df$initialisation_value_2 <- this_report[[DQ_types[dq_iter]]]$`initialisation_phase[2]`
        } else {
          temp_df$initialisation_value_2 <- NA
        }
        dq_df <- rbind(dq_df, temp_df)
      }
      dq_df$par_set <- 1
      dq_df$label <- report_labels[i]
      complete_df <- rbind(complete_df, dq_df)
    } else {
      if (this_report[[1]]$type != "derived_quantity") {
        next
      }
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        ##
        DQ_types <- names(this_report[[dash_i]])[!names(this_report[[dash_i]]) %in% "type"]
        dq_df <- NULL
        for (dq_iter in 1:length(DQ_types)) {
          temp_df <- data.frame(par_set = iter_labs[dash_i], years = names(this_report[[dash_i]][[DQ_types[dq_iter]]]$values), initialisation_value = this_report[[dash_i]][[DQ_types[dq_iter]]]$`initialisation_phase[1]`, values = this_report[[dash_i]][[DQ_types[dq_iter]]]$values, dq_label = DQ_types[dq_iter], label = report_labels[i])
          if (exists("initialisation_phase[2]", this_report[[dash_i]][[DQ_types[dq_iter]]])) {
            temp_df$initialisation_value_2 <- this_report[[dash_i]][[DQ_types[dq_iter]]]$`initialisation_phase[2]`
          } else {
            temp_df$initialisation_value_2 <- NA
          }

          dq_df <- rbind(dq_df, temp_df)
        }
        complete_df <- rbind(complete_df, dq_df)
      }
      dq_df$label <- report_labels[i]
      complete_df$par_set <- factor(complete_df$par_set, ordered = T)
    }
  }
  complete_df$initialisation_value <- as.numeric(complete_df$initialisation_value)
  complete_df$years <- as.numeric(complete_df$years)
  return(complete_df)
}

#' @rdname get_derived_quantities
#' @method get_derived_quantities list
#' @export
"get_derived_quantities.list" <- function(model, reformat_labels = TRUE) {
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
    this_dq <- get_derived_quantities(model[[i]], reformat_labels = reformat_labels)
    this_dq$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
}

#' @rdname get_derived_quantities
#' @method get_derived_quantities casal2TAB
#' @export
"get_derived_quantities.casal2TAB" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  # Note: needs to be optimised
  complete_df <- NULL
  for (i in 1:length(model)) {
    this_report <- model[[i]]
    if (this_report$type != "derived_quantity") {
      next
    }
    colabs <- colnames(this_report$values)
    first_component <- Reduce(c, lapply(strsplit(colabs, split = "[", fixed = T), FUN = function(x) {
      x[1]
    }))
    second_component <- Reduce(c, lapply(strsplit(colabs, split = "[", fixed = T), FUN = function(x) {
      x[2]
    }))
    third_component <- Reduce(c, lapply(strsplit(colabs, split = "[", fixed = T), FUN = function(x) {
      x[3]
    }))
    ##
    second_component <- substring(second_component, first = 1, last = nchar(second_component) - 1)
    third_component <- substring(third_component, first = 1, last = nchar(third_component) - 1)
    newcolab <- paste(second_component, third_component, first_component, sep = "-")
    colnames(this_report$values) <- newcolab
    cat("getting values for ", report_labels[i], "\n")
    long_format <- suppressMessages({
      melt(as.matrix(this_report$values), variable.name = "colname", value.name = "values", factorsAsStrings = T)
    })
    colnames(long_format) <- c("iteration", "colname", "values")
    long_format$label <- report_labels[i]
    long_format$colname <- as.character(long_format$colname)
    split_cols <- strsplit(long_format$colname, split = "-", fixed = T)
    dq_label <- unlist(lapply(split_cols, function(x) {
      x[1]
    }))
    years <- unlist(lapply(split_cols, function(x) {
      x[2]
    }))
    type <- unlist(lapply(split_cols, function(x) {
      x[3]
    }))
    ##
    long_format$years <- years
    long_format$dq_label <- dq_label
    long_format$type <- type
    complete_df <- rbind(complete_df, long_format)
  }
  return(complete_df)
}
