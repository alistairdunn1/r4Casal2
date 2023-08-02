#' @title get_init_F
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of process type mortality_initialisation_baranov
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_init_F
#' @export get_init_F
#' @importFrom reshape2 melt
#' @importFrom tidyr gather separate spread %>%
"get_init_F" <- function(model) {
  UseMethod("get_init_F", model)
}

#' @rdname get_init_F
#' @method get_init_F casal2MPD
#' @export
"get_init_F.casal2MPD" <- function(model) {
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  reports_labels <- reformat_default_labels(names(model))
  for (i in 1:length(model)) {
    full_df <- NULL
    if (reports_labels[i] == "header") {
      next
    }

    this_report <- model[[i]]

    if (any(names(this_report) == "type")) {
      if (tolower(this_report$type) != "process") {
        next
      }
      if (tolower(this_report$type) == "process" & tolower(this_report$sub_type) != "mortality_initialisation_baranov") {
        next
      }
      ## only a single trajectory
      full_df <- data.frame(par_set = 1, fishing_mortality = this_report$fishing_mortality, label = reports_labels[i])
      complete_df <- rbind(complete_df, full_df)
    } else {
      if (this_report[[1]]$type != "process") {
        next
      }
      if (this_report[[1]]$type == "process" & tolower(this_report[[1]]$sub_type) != "mortality_initialisation_baranov") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)

      for (dash_i in 1:n_runs) {
        full_df <- data.frame(par_set = iter_labs[dash_i], fishing_mortality = this_report[[dash_i]]$fishing_mortality, label = reports_labels[i])
        complete_df <- rbind(complete_df, full_df)
      }
    }
  }
  return(complete_df)
  invisible()
}

#' @rdname get_init_F
#' @method get_init_F list
#' @export
"get_init_F.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_init_F(model[[i]])
    if (is.null(this_dq)) {
      next
    }
    this_dq$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
  invisible()
}

#' @rdname get_init_F
#' @method get_init_F casal2TAB
#' @export
"get_init_F.casal2TAB" <- function(model) {
  print("Not written yet")
}
