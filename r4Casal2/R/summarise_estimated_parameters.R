#' summarise_estimated_parameters
#' @description  this function will find estimate_summary report and summarise the priors, mpd and initial value
#' @author C Marsh
#' @param model an mpd model
#' @param ignore_ycs ignore YCS parameters
#' @param plot_it if true plot MPD value, initial value, and prior, otherwise report MPD, init, lower_bound, upper_bound, prior_type
#' @return either a ggplot or a list with two elements. The first is for plotting, the second is a useful data frame
#' outlining priors, bounds mpd values initial values etc.



#' @rdname summarise_estimated_parameters
#' @export summarise_estimated_parameters
"summarise_estimated_parameters" <- function(model, plot_it = F, ignore_ycs = F) {
  UseMethod("summarise_estimated_parameters", model)
}

#' @rdname summarise_estimated_parameters
#' @method summarise_estimated_parameters casal2MPD
#' @export
summarise_estimated_parameters.casal2MPD <- function(model, plot_it = F, ignore_ycs = F) {
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  found_report <- FALSE
  plot_df <- NULL
  reports_labels <- reformat_default_labels(names(model))
  for (i in 1:length(model)) {
    if (reports_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (!any(names(this_report) == "type")) {
      if (this_report[[1]]$type != "estimate_summary") {
        next
      }

      if (length(this_report) != 1) {
        message("cannot deal with multi input parameter model runs.")
        return(NULL)
      } else {
        this_report <- this_report[[1]]
      }
    }
    if (this_report$type != "estimate_summary") {
      next
    }
    if (found_report) {
      cat("There are multiple 'estimate_summary', skipping the last one")
      next
    }
    n_params <- length(this_report)
    ## create a ggplot
    for (p in 1:n_params) {
      if (names(this_report)[p] == "type") {
        next
      }
      this_param <- this_report[[p]]
      temp_param_label <- gsub(x = this_param$parameter, pattern = "\\{", replacement = "_")
      temp_param_label <- gsub(x = temp_param_label, pattern = "\\}", replacement = "_")
      temp_param_label <- gsub(x = temp_param_label, pattern = "\\[", replacement = "_")
      temp_param_label <- gsub(x = temp_param_label, pattern = "\\]", replacement = "_")

      if (grepl(temp_param_label, pattern = "ycs_values")) {
        if (ignore_ycs) {
          next
        } ## skip this parameter it is a ycs value and we are ignoring them.
      }
      hyper_params <- this_param$hyperparameters
      hyper_param_values <- as.numeric(this_param$hyperparameter_values)
      hyper_params_labels <- paste(paste0(hyper_params, " = ", hyper_param_values), collapse = ", ")
      ## create object for list
      temp_df <- with(this_param, {
        data.frame(
          parameter = parameter, initial_value = initial_value,
          prior = sub_type, bounds = paste0(lower_bound, " - ", upper_bound), phase = phase,
          MPD = value, std_dev = std_dev
        )
      })
      temp_df$"Hyper parameters" <- hyper_params_labels
      type <- temp_df$prior
      complete_df <- rbind(complete_df, temp_df)

      param_increment <- (as.numeric(this_param$upper_bound) - as.numeric(this_param$lower_bound)) / 50
      param_value <- seq(from = as.numeric(this_param$lower_bound), to = as.numeric(this_param$upper_bound), by = param_increment)

      if (type == "lognormal") {
        sigma <- log_sigma(hyper_param_values[2])
        res <- exp(-(log(param_value) + 0.5 * ((log(param_value / hyper_param_values[1]) / sigma + sigma / 2)^2)))
      } else if (type == "normal_by_stdev") {
        res <- exp(-(0.5 * (((param_value - hyper_param_values[1]) / hyper_param_values[2])^2)))
      } else if (type == "normal") {
        sigma <- hyper_param_values[1] * hyper_param_values[2]
        res <- exp(-(0.5 * (((param_value - hyper_param_values[1]) / sigma)^2)))
      } else if (type == "uniform") {
        res <- rep(1, length(param_value))
      } else if (type == "uniform_log") {
        res <- exp(-log(param_value))
      } else if (type == "normal_log") {
        res <- exp(-(log(param_value) + 0.5 * (((log(param_value) - hyper_param_values[1]) / hyper_param_values[2])^2)))
      } else if (type == "beta") {
        new.mu <- (hyper_param_values[1] - hyper_param_values[3]) / (hyper_param_values[4] - hyper_param_values[3])
        new.t <- (((hyper_param_values[1] - hyper_param_values[3]) * (hyper_param_values[4] - hyper_param_values[1])) / (hyper_param_values[2]^2)) - 1
        if (new.t <= 0) {
          stop(paste0("Standard deviation too large, for parameter ", temp_df$parameter, "\n"))
        }
        if ((as.numeric(temp_df$lower_bound) < hyper_param_values[3]) || (as.numeric(temp_df$upper_bound) > hyper_param_values[4])) {
          stop(paste0("Bad bounds on Beta prior ", temp_df$parameter, "\n"))
        }
        Bm <- new.t * new.mu
        Bn <- new.t * (1 - new.mu)
        res <- (1 - Bm) * log(param_value - hyper_param_values[3]) + (1 - Bn) * log(hyper_param_values[4] - param_value)
        res <- exp(-res)
      } else {
        cat(paste0("Unknown prior type skipping ", temp_df$parameter, "\n"))
        next
      }
      res <- c(res / max(res, na.rm = T))
      temp_plot_df <- data.frame(parameter_value = param_value, likelihood = res, parameter = this_param$parameter)
      plot_df <- rbind(plot_df, temp_plot_df)
    }
  }
  if (is.null(plot_df)) {
    return("Could not find 'estimate_summary' report in Casal2 output")
  }

  if (plot_it) {
    ggplot(plot_df) +
      geom_line(aes(x = parameter_value, y = likelihood, col = "Prior")) +
      geom_point(data = complete_df, aes(x = initial_value, y = 0, col = "Initial_value")) +
      geom_vline(data = complete_df, aes(xintercept = MPD, col = "MPD")) +
      labs(x = "", y = "Likelihood") +
      facet_wrap(~parameter, scales = "free_x") +
      theme(legend.position = "bottom")
  } else {
    return(list(plot_df = plot_df, summary_df = complete_df))
  }
}
