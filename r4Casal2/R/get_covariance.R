#' @title get_covariance
#'
#' @description
#' An accessor function that returns a covariance function
#'
#' @author Craig Marsh
#' @param model <casal2MPD, list> object that are generated from the extract.mpd() functions.
#' @return A data frame from Casal2 model output
#' @rdname get_covariance
#' @export get_covariance

"get_covariance" <-
  function(model) {
    UseMethod("get_covariance", model)
  }

#'
#' @rdname get_covariance
#' @method get_covariance casal2MPD
#' @export
"get_covariance.casal2MPD" <- function(model) {
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
