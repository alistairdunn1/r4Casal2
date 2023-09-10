#' @title get_transformed_parameters
#' @description
#' An accessor function that returns a data frame of a parameter_transformations class that can be easily plotted
#' @author Craig Marsh
#' @param model <casal2MPD> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with profile_values and likelihood components
#' @importFrom dplyr bind_cols
#' @rdname get_transformed_parameters
#' @export get_transformed_parameters
"get_transformed_parameters" <- function(model) {
  UseMethod("get_transformed_parameters", model)
}

#' @rdname get_transformed_parameters
#' @method get_transformed_parameters casal2MPD
#' @export
get_transformed_parameters.casal2MPD <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  parameter_df <- NULL
  for (i in 1:length(model)) {
    if (report_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (exists(x = "type", where = this_report)) {
      if (this_report$type != "parameter_transformations") {
        next
      }
      ## these reports are bespoke which sucks
      if (this_report$transformation_type == "log") {
        temp_df <- data.frame(label = report_labels[i], type = this_report$transformation_type, parameter = this_report$parameters, untransformed = this_report$parameter_values, transformed_value = this_report$log_parameter)
        parameter_df <- rbind(parameter_df, temp_df)
      } else if (this_report$transformation_type == "logistic") {
        temp_df <- data.frame(label = report_labels[i], type = this_report$transformation_type, parameter = this_report$parameters, untransformed = this_report$parameter_values, transformed_value = this_report$logistic_parameter)
        parameter_df <- rbind(parameter_df, temp_df)
      } else if (this_report$transformation_type == "orthogonal") {
        temp_df <- data.frame(label = report_labels[i], type = this_report$transformation_type, parameter = this_report$parameters, untransformed = this_report$parameter_values, transformed_value = c(this_report$product_parameter, this_report$quotient_parameter))
        parameter_df <- rbind(parameter_df, temp_df)
      } else if (this_report$transformation_type == "log_sum") {
        temp_df <- data.frame(label = report_labels[i], type = this_report$transformation_type, parameter = this_report$parameters, untransformed = this_report$parameter_values, transformed_value = c(this_report$log_total_parameter, this_report$total_proportion_parameter))
        parameter_df <- rbind(parameter_df, temp_df)
      }
    } else {
      # -i multi run output
      if (this_report[[1]]$type != "parameter_transformations") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        ## these reports are bespoke which sucks
        if (this_report[[dash_i]]$transformation_type == "log") {
          temp_df <- data.frame(label = report_labels[i], type = this_report[[dash_i]]$transformation_type, parameter = this_report[[dash_i]]$parameters, untransformed = this_report[[dash_i]]$parameter_values, transformed_value = this_report[[dash_i]]$log_parameter)
        } else if (this_report[[dash_i]]$transformation_type == "logistic") {
          temp_df <- data.frame(label = report_labels[i], type = this_report[[dash_i]]$transformation_type, parameter = this_report[[dash_i]]$parameters, untransformed = this_report[[dash_i]]$parameter_values, transformed_value = this_report[[dash_i]]$logistic_parameter)
        } else if (this_report[[dash_i]]$transformation_type == "orthogonal") {
          temp_df <- data.frame(label = report_labels[i], type = this_report[[dash_i]]$transformation_type, parameter = this_report[[dash_i]]$parameters, untransformed = this_report[[dash_i]]$parameter_values, transformed_value = c(this_report[[dash_i]]$product_parameter, this_report[[dash_i]]$quotient_parameter))
        } else if (this_report[[dash_i]]$transformation_type == "log_sum") {
          temp_df <- data.frame(label = report_labels[i], type = this_report[[dash_i]]$transformation_type, parameter = this_report[[dash_i]]$parameters, untransformed = this_report[[dash_i]]$parameter_values, transformed_value = c(this_report[[dash_i]]$log_total_parameter, this_report[[dash_i]]$total_proportion_parameter))
        }
        temp_df$par_set <- iter_labs[dash_i]
        parameter_df <- rbind(parameter_df, temp_df)
      }
    }
  }
  return(parameter_df)
}

#' @rdname get_transformed_parameters
#' @method get_transformed_parameters list
#' @export
"get_transformed_parameters.list" <- function(model, reformat_labels = TRUE) {
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
    this_df <- get_transformed_parameters(model[[i]], reformat_labels = reformat_labels)
    if (is.null(this_df)) {
      next
    }
    this_df$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_df)
  }
  return(full_DF)
}

#' @rdname get_transformed_parameters
#' @method get_transformed_parameters casal2TAB
#' @export
get_transformed_parameters.casal2TAB <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  cat("this can take a few minutes for large models and big mcmc chains. Please be patient :~) \n")

  complete_df <- NULL

  for (i in 1:length(report_labels)) {
    this_report <- model[[i]]
    if (this_report$type != "parameter_transformations") {
      next
    }
    colnames(this_report$values) <- paste0(report_labels[i], "-", colnames(this_report$values))
    if (is.null(complete_df)) {
      complete_df <- this_report$values
    } else {
      complete_df <- bind_cols(complete_df, this_report$values)
    }
  }
  complete_df$sample <- 1:nrow(complete_df)
  return(complete_df)
}
