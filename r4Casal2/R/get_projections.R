#' @title get_projections
#' @description
#' An accessor function that returns a data frame from a Casal2 model output of projections
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions.
#' @return A data frame with all selectivity reports from Casal2 model output
#' @rdname get_projections
#' @export get_projections
#' @importFrom reshape2 melt
"get_projections" <- function(model) {
  UseMethod("get_projections", model)
}

#' @rdname get_projections
#' @method get_projections casal2TAB
#' @export
"get_projections.casal2TAB" <- function(model) {
  reports_labels <- reformat_default_labels(names(model))
  complete_df <- NULL
  for (i in 1:length(model)) {
    if (reports_labels[i] == "header") {
      next
    }
    this_report <- model[[i]]
    if (this_report$type != "project") {
      next
    }
    proj_df <- this_report$values
    proj_molten <- suppressMessages({
      melt((proj_df), variable.name = "colname", value.name = "value", factorsAsStrings = F)
    })
    param <- unlist(lapply(strsplit(as.character(proj_molten$colname), split = ".", fixed = T), FUN = function(x) {
      x[1]
    }))
    year <- unlist(lapply(strsplit(as.character(proj_molten$colname), split = ".", fixed = T), FUN = function(x) {
      x[2]
    }))

    proj_molten$year <- as.numeric(year)
    proj_molten$param <- param
    complete_df <- rbind(complete_df, proj_molten)
  }
  return(complete_df)
  invisible()
}
