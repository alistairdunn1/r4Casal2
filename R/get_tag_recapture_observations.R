#' @title get_tag_recapture_observations
#'
#' @description
#' An accessor function that returns a data frame of all tag-recapture observations
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @return dataframe with all observations of type == 'observation' and observation_type %in% c('biomass', 'abundance')
#' @rdname get_tag_recapture_observations
#' @export get_tag_recapture_observations

"get_tag_recapture_observations" <- function(model) {
  UseMethod("get_tag_recapture_observations", model)
}

#' @rdname get_tag_recapture_observations
#' @method get_tag_recapture_observations casal2MPD
#' @export

"get_tag_recapture_observations.casal2MPD" <- function(model) {
  observation_type_allowed <- c("tag_recapture_by_length_for_growth", "tag_recapture_by_length", "tag_recapture_by_age")
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  reports_labels <- names(model)
  for (i in 1:length(model)) {
    if (reports_labels[i] == "header") {
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
        this_ob$observation_label <- reports_labels[i]
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
      # print("multi iteration report found")
      multiple_iterations_in_a_report <- TRUE
      if (this_report$"1"$type != "observation") {
        next
      }
      if (this_report$"1"$observation_type %in% observation_type_allowed) {
        n_runs <- length(this_report)
        for (dash_i in 1:n_runs) {
          ## add it to full df
          this_ob <- this_report[[dash_i]]$Values
          this_ob$observation_label <- reports_labels[i]
          this_ob$observation_type <- this_report[[dash_i]]$observation_type
          this_ob$likelihood <- this_report[[dash_i]]$likelihood
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


#' @rdname get_tag_recapture_observations
#' @method get_tag_recapture_observations list
#' @export

"get_tag_recapture_observations.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance <- get_tag_recapture_observations(model[[i]])
    if (!is.null(this_abundance)) {
      this_abundance$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_abundance)
    }
  }
  return(full_DF)
  invisible()
}



