#' @title get_abundance_observations
#' @description
#' An accessor function that returns a data frame of all relative abundance data sets in a model
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @return dataframe with all observations of type == 'observation' and observation_type %in% c('biomass', 'abundance')
#' @rdname get_abundance_observations
#' @export get_abundance_observations
"get_abundance_observations" <- function(model) {
  UseMethod("get_abundance_observations", model)
}

#' @rdname get_abundance_observations
#' @method get_abundance_observations casal2MPD
#' @export
"get_abundance_observations.casal2MPD" <- function(model) {
  observation_type_allowed <- c("biomass", "abundance")
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
      if (this_report[[1]]$type != "observation") {
        next
      }
      if (this_report[[1]]$observation_type %in% observation_type_allowed) {
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
  ## calculate U_CI only implemented for normal and lognormal
  complete_df$U_CI <- NA
  complete_df$L_CI <- NA
  ## deal with normal
  normal_ndx <- complete_df$likelihood == "normal"
  total_sigma <- complete_df$observed * complete_df$adjusted_error
  complete_df$U_CI[normal_ndx] <- complete_df$observed[normal_ndx] + 1.96 * total_sigma[normal_ndx]
  complete_df$L_CI[normal_ndx] <- complete_df$observed[normal_ndx] - 1.96 * total_sigma[normal_ndx]
  ## deal with lognormal
  lognormal_ndx <- complete_df$likelihood == "lognormal"
  total_sigma <- sqrt(log(1 + complete_df$adjusted_error[lognormal_ndx]^2))
  Mean <- log(complete_df$observed[lognormal_ndx]) - 0.5 * (total_sigma^2)
  complete_df$U_CI[lognormal_ndx] <- exp(Mean + 1.96 * total_sigma)
  complete_df$L_CI[lognormal_ndx] <- exp(Mean - 1.96 * total_sigma)
  return(complete_df)
}

#' @rdname get_abundance_observations
#' @method get_abundance_observations list
#' @export
"get_abundance_observations.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance <- get_abundance_observations(model[[i]])
    if (!is.null(this_abundance)) {
      this_abundance$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_abundance)
    }
  }
  return(full_DF)
  invisible()
}

#' @rdname get_abundance_observations
#' @method get_abundance_observations casal2TAB
#' @export
"get_abundance_observations.casal2TAB" <- function(model) {
  observation_type_allowed <- c("biomass", "abundance")
  reports_labels <- reformat_default_labels(names(model))
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
    val_molten$report_label <- reports_labels[i]
    complete_df <- rbind(complete_df, val_molten)
  }
  return(complete_df)
  invisible()
}
