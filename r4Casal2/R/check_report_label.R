#' Utility function to check reports inputs are consistent
#'
#' @author Craig Marsh
#' @param report_label string report label
#' @param model casal2 model
#' @keywords internal
#'
check_report_label <- function(report_label, model) {
  ## check report label exists
  passed <- TRUE
  error_msg <- ""
  if (!report_label %in% names(model)) {
    passed <- FALSE
    error_msg <- paste0("The report label '", report_label, "' was not found. The report labels available are: ", paste(names(model), collapse = ", "))
  }
  return(list(check = passed, msg = error_msg))
}

#' Utility function to reformat report labels created from
#' Casal2s default report. cut off leading and trailing __
#' so the report label '__selectivities__' will be changed to 'selectivities'
#' if a default report has been created with the same label as an exiting report
#' we change its name from '__report_label__' to 'report_label_default'
#'
#' @author Craig Marsh
#' @param report_labels vector of report labels
#' @keywords internal
#'
reformat_default_labels <- function(report_labels) {
  ## find elements that start and end with '__'
  default_label_ndx <- (substring(report_labels, first = 0, last = 2) == "__") & (substring(report_labels, first = nchar(report_labels) - 1, last = nchar(report_labels)) == "__")
  ## trim the labels and return
  new_labels <- report_labels
  ## trim
  new_labels[default_label_ndx] <- substring(report_labels[default_label_ndx], first = 3, last = nchar(report_labels[default_label_ndx]) - 2)
  ## check for duplicates
  duplicated_labels <- anyDuplicated(new_labels)
  if (duplicated_labels != 0) {
    for (i in 1:length(duplicated_labels)) {
      repeats <- which(new_labels %in% new_labels[duplicated_labels[i]])
      ## which one is original and which one created from default
      original_ndx <- which(report_labels %in% new_labels[repeats])
      default_ndx <- repeats[original_ndx != repeats]
      ## add something for the
      new_labels[default_ndx] <- paste0(new_labels[default_ndx], "_default")
    }
  }
  return(new_labels)
}
