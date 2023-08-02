#' @title get_growth
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of type age_length
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_growth
#' @export get_growth
#' @importFrom reshape2 melt
"get_growth" <- function(model) {
  UseMethod("get_growth", model)
}

#' @rdname get_growth
#' @method get_growth casal2MPD
#' @export
"get_growth.casal2MPD" <- function(model) {
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
      if (this_report$type == "age_length") {
        temp_df <- NULL
        years <- names(this_report)
        for (y in 1:length(years)) {
          if (years[y] == "type") {
            next
          }
          this_df <- data.frame(
            age = this_report[[y]]$age, year = as.numeric(years[y]), time_step = this_report[[y]]$time_step,
            cvs_by_age = this_report[[y]]$cvs_by_age, mean_length_at_age = this_report[[y]]$mean_length_at_age, mean_weight_at_age = this_report[[y]]$mean_weight_at_age,
            label = reports_labels[i]
          )
          temp_df <- rbind(temp_df, this_df)
        }
        temp_df$par_set <- 1
        complete_df <- rbind(complete_df, temp_df)
      }
      if (this_report$type == "growth_increment") {
        temp_df <- NULL
        years <- names(this_report)
        for (y in 1:length(years)) {
          if (years[y] == "type") {
            next
          }
          this_df <- this_report[[y]]$values
          this_df$year <- as.numeric(years[y])
          this_df$label <- reports_labels[i]
          this_df$distribution <- this_report[[y]]$distribution
          this_df$time_step <- this_report[[y]]$time_step
          temp_df <- rbind(temp_df, this_df)
        }
        temp_df$par_set <- 1
        complete_df <- rbind(complete_df, temp_df)
      }
    } else {
      if (this_report[[1]]$type == "age_length") {
        ## Multiple parameter inputs
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
            this_df <- data.frame(
              age = this_par_df[[y]]$age, year = as.numeric(years[y]), time_step = this_par_df[[y]]$time_step,
              cvs_by_age = this_par_df[[y]]$cvs_by_age, mean_length_at_age = this_par_df[[y]]$mean_length_at_age, mean_weight_at_age = this_par_df[[y]]$mean_weight_at_age,
              label = reports_labels[i], par_set = iter_labs[dash_i]
            )
            temp_df <- rbind(temp_df, this_df)
          }
          complete_df <- rbind(complete_df, temp_df)
        }
      }
      if (this_report[[1]]$type == "growth_increment") {
        ## Multiple parameter inputs
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
            this_df <- this_par_df[[y]]$values
            this_df$year <- as.numeric(years[y])
            this_df$label <- reports_labels[i]
            this_df$distribution <- this_par_df[[y]]$distribution
            this_df$time_step <- this_par_df[[y]]$time_step
            temp_df <- rbind(temp_df, this_df)
          }
          complete_df <- rbind(complete_df, temp_df)
        }
      }
    }
  }
  return(complete_df)
  invisible()
}

#' @rdname get_growth
#' @method get_growth list
#' @export
"get_growth.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_growth(model[[i]])
    this_dq$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
  invisible()
}
