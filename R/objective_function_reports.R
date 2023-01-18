#' @title get_objective_function
#' @details
#' take a Casal2 objective_function report and aggregrate components so easier to handle with
#' visualising likelihood components
#' @param model casal2 model or a list of model
#' @param aggregate_obs <bool> whether to aggretate over observations, if false will report objective function by year and observation.
#' @return data frame of objective function negative log-likelihood components
#' @rdname get_objective_function
#' @export get_objective_function
#'

"get_objective_function" <- function(model, aggregate_obs = T) {
  UseMethod("get_objective_function", model)
}
#'
#' @rdname get_objective_function
#' @method get_objective_function casal2MPD
#' @export
"get_objective_function.casal2MPD" = function(model, aggregate_obs = T) {
  # can be -r or -r -i
  multiple_iterations_in_a_report = FALSE
  full_df = NULL
  reports_labels = reformat_default_labels(names(model))
  for(i in 1:length(model)) {
    if (reports_labels[i] == "header")
      next;
    this_report = model[[i]]
    if(exists(x = "type", where = this_report)) {
      if(tolower(this_report$type) != "objective_function")
        next;
      if(aggregate_obs) {
        return(aggregate_single_objective_report(this_report$values));
      } else {
        return(this_report$values);
      }
    } else {
      if(tolower(this_report[[1]]$type) != "objective_function")
        next;
      ## Multiple parameter inputs
      n_runs = length(this_report)
      iter_labs = names(this_report)
      for(dash_i in 1:n_runs) {
        this_par_set = NULL
        if(aggregate_obs) {
          this_par_set = aggregate_single_objective_report(this_report[[dash_i]]$values)
          if(dash_i == 1) {
            full_df = this_par_set
          } else {
            full_df = cbind(full_df, this_par_set$negative_loglik)
          }
        } else {
          if(dash_i == 1) {
            full_df = data.frame(components = names(this_report[[dash_i]]$values), val = as.numeric(this_report[[dash_i]]$values))
          } else {
            full_df = cbind(full_df, as.numeric(this_report[[dash_i]]$values))
          }
        }

      }
      return(full_df)
    }
  }
}

#'
#' @rdname get_objective_function
#' @method get_objective_function list
#' @export
"get_objective_function.list" = function(model) {
  run_labs = names(model)
  full_DF = NULL
  ## iterate over the models
  for(i in 1:length(model)) {

    if(class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq = get_objective_function(model[[i]])
    this_dq$model_label = run_labs[i]
    full_DF = rbind(full_DF, this_dq);
  }
  return(full_DF)
  invisible()
}

#' @title aggregate_objective_report
#' @details
#' take a Casal2 objective_function report and aggregate components so easier to handle with
#' visualising likelihood components
#' @param objective_report casal2_objective_function which is a report from an extract.mpd() object
#' @return data frame of aggregated objective function negative log-likelihood components
#' @rdname aggregate_objective_report
#' @export aggregate_objective_report
#'

aggregate_objective_report <- function(objective_report) {
  multi_parameter_input = F
  if(is.null(objective_report$type)) {
    ## possible -i input
    if(is.null(objective_report[[1]]$type)) {
      stop("Unknown format for objective_report")
    }
    if(objective_report[[1]]$type != "objective_function")
      stop("objective_report must be of type  'objective_function'")
    multi_parameter_input = T
  } else if(objective_report$type != "objective_function") {
    stop("objective_report must be of type  'objective_function'")
  }

  if(!multi_parameter_input) {
    return(aggregate_single_objective_report(objective_report$values))
  } else {
    full_df = NULL
    for(i in 1:length(objective_report)) {
      this_par_set = aggregate_single_objective_report(objective_report[[i]]$values)
      if(i == 1) {
        full_df = this_par_set
      } else {
        full_df = cbind(full_df, this_par_set$negative_loglik)
      }
    }
    return(full_df)
  }
}

#' aggregate_single_objective_report
#' @details used by aggregate_objective_report
#' @param values data.frame from a Casal2 objective_function report
#' @return data frame with aggregated values
aggregate_single_objective_report <- function(values) {
  obj_labels = names(values)
  type = Reduce(c, lapply(strsplit(obj_labels, split = "->", fixed = T), FUN = function(x){x[1]}))
  label = Reduce(c, lapply(strsplit(obj_labels, split = "-", fixed = T), FUN = function(x){x[2]}))
  label = substring(label, first = 2)
  new_label = ifelse(!is.na(label), paste0("-",label),"")
  unique_vals = paste0(type,new_label)
  aggregate_labs = unique(unique_vals)
  ll_values = vector()
  ## observations
  for(i in 1:length(aggregate_labs)) {
    ndx = unique_vals  %in%  aggregate_labs[i]
    ll_values[i] = sum(values[ndx])
  }
  obj_df = data.frame(component = aggregate_labs, negative_loglik = ll_values)

  return(obj_df)
}
