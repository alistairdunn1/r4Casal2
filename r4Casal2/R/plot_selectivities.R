#' @title plot_selectivities
#' @description
#' A Generic plotting function for selectivities that are derived from get_selectivities function()
#' @author Craig Marsh
#' @param model data.frame
#' @param selectivity_labels optional command if you only want to plot a subset of the available selectivities
#' @importFrom ggplot2 ggplot geom_line aes
#' @return a ggplot
#' @rdname plot_selectivities
#' @export plot_selectivities
"plot_selectivities" <- function(model, selectivity_labels = NULL) {
  full_DF <- get_selectivities(model)
  if (is.null(full_DF)) {
    message("No selectivies found in Casal2 output")
    return(NULL)
  }
  if (!is.null(selectivity_labels)) {
    full_DF <- subset(full_DF, subset = full_DF$selectivity_label %in% selectivity_labels)
  }
  plt <- ggplot(full_DF, aes(x = bin, group = selectivity_label, col = selectivity_label)) +
    geom_line(aes(y = selectivity), size = 2) +
    facet_wrap(~selectivity_label)
  return(plt)
}
