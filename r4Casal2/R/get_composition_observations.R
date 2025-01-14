#' @title get_composition_observations
#' @description An accessor function that returns a data frame of all composition data sets in a model
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract() functions
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return data frame with all observations of type == 'observation' and observation_type !%in% c('biomass', 'abundance')
#' @rdname get_composition_observations
#' @export get_composition_observations
"get_composition_observations" <- function(model, ...) {
  UseMethod("get_composition_observations", model)
}

#' @rdname get_composition_observations
#' @method get_composition_observations casal2MPD
#' @export
"get_composition_observations.casal2MPD" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  # can be -r or -r -i
  observation_type_allowed <- c("proportions_at_age", "proportions_at_length", "process_removals_by_age", "process_removals_by_length")
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  for (i in 1:length(model)) {
    if (report_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (any(names(this_report) == "type")) {
      if (this_report$type != "observation") {
        next
      }
      if (this_report$observation_type %in% observation_type_allowed) {
        ## add it to full df
        this_ob <- this_report$Values
        this_ob$observation_label <- report_labels[i]
        this_ob$observation_type <- this_report$observation_type
        this_ob$likelihood <- this_report$likelihood
        this_ob$par_set <- 1 ## so compatible with -i runs
        ## check col compatibility some reports will print residuals and some wont
        if (!is.null(complete_df)) {
          if (any(!colnames(complete_df) %in% colnames(this_ob))) {
            drop_cols <- which(!colnames(complete_df) %in% colnames(this_ob))
            complete_df <- complete_df[, -drop_cols]
          }
          if (any(!colnames(this_ob) %in% colnames(complete_df))) {
            drop_cols <- which(!colnames(this_ob) %in% colnames(complete_df))
            this_ob <- this_ob[, -drop_cols]
          }
        }
        complete_df <- rbind(complete_df, this_ob)
        next
      }
    } else {
      multiple_iterations_in_a_report <- TRUE
      if (this_report$"1"$type != "observation") {
        next
      }
      if (this_report$"1"$observation_type %in% observation_type_allowed) {
        n_runs <- length(this_report)
        for (dash_i in 1:n_runs) {
          ## add it to full df
          this_ob <- this_report[[dash_i]]$Values
          this_ob$likelihood <- this_report[[dash_i]]$likelihood
          this_ob$observation_label <- report_labels[i]
          this_ob$observation_type <- this_report[[dash_i]]$observation_type
          this_ob$par_set <- dash_i
          ## check col compatibility some reports will print residuals and some wont
          if (!is.null(complete_df)) {
            if (any(!colnames(complete_df) %in% colnames(this_ob))) {
              drop_cols <- which(!colnames(complete_df) %in% colnames(this_ob))
              complete_df <- complete_df[, -drop_cols]
            }
            if (any(!colnames(this_ob) %in% colnames(complete_df))) {
              drop_cols <- which(!colnames(this_ob) %in% colnames(complete_df))
              this_ob <- this_ob[, -drop_cols]
            }
          }
          complete_df <- rbind(complete_df, this_ob)
        }
      }
    }
  }
  return(complete_df)
}

#' @rdname get_composition_observations
#' @method get_composition_observations list
#' @export
"get_composition_observations.list" <- function(model, reformat_labels = TRUE) {
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
    this_abundance <- get_composition_observations(model[[i]], reformat_labels = reformat_labels)
    if (!is.null(this_abundance)) {
      this_abundance$model_label <- report_labels[i]
      full_DF <- rbind(full_DF, this_abundance)
    }
  }
  return(full_DF)
}

#' @rdname get_composition_observations
#' @method get_composition_observations casal2TAB
#' @export
"get_composition_observations.casal2TAB" <- function(mode, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  observation_type_allowed <- c("proportions_at_age", "proportions_at_length", "process_removals_by_age", "process_removals_by_length")
  complete_df <- NULL
  for (i in 1:length(model)) {
    this_report <- model[[i]]
    if (is.null(this_report$observation_type) || !(this_report$observation_type %in% observation_type_allowed)) {
      next
    }
    val_df <- this_report$values
    index <- match(names(this_report$values), unique(names(this_report$values)))
    index <- data.frame(index) %>%
      group_by(index) %>%
      mutate(count = 1:n())

    val_molten <- suppressMessages({
      melt(as.matrix(val_df), variable.name = "colname", value.name = "value", factorsAsStrings = F)
    })
    colnames(val_molten) <- c("iteration", "parameter", "value")
    val_molten$category <- this_report$categories[index$count]
    val_molten$report_label <- report_labels[i]
    complete_df <- rbind(complete_df, val_molten)
  }
  return(complete_df)
}
