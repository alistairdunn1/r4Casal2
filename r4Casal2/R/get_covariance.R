#' @title get_covariance
#' @description An accessor function that returns a covariance function
#' @author Craig Marsh
#' @param model <casal2MPD> object that are generated from the extract.mpd() function
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return A data frame from Casal2 model output
#' @rdname get_covariance
#' @export get_covariance
"get_covariance" <- function(model, ...) {
  UseMethod("get_covariance", model)
}

#' @rdname get_covariance
#' @method get_covariance casal2MPD
#' @export
"get_covariance.casal2MPD" <- function(model, reformat_labels = TRUE) {
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
      if (this_report$type != "covariance_matrix") {
        next
      }
      return(this_report$covariance_matrix)
    } else {
      if (this_report[[1]]$type != "covariance_matrix") {
        next
      }
      return(this_report[[1]]$covariance_matrix)
    }
  }
  invisible()
}

#' @rdname get_covariance
#' @method get_covariance casal2TAB
#' @export
"get_covariance" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  return("get_covariance for casal2TAB has not been implemented")
}

#' @rdname get_covariance
#' @method get_covariance casal2TAB
#' @export
"get_covariance.casal2TAB" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  stop("get_covariance for casal2TAB has not been implemented")
}
