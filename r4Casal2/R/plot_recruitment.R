#' @title plot_recruitment plot recruitment
#' @description A plotting function to plot recruitment for 'casal2TAB' and 'casal2MPD' objects.
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions using the Casal2 base library
#' @param report_label <string> optional if you want to plot a specific recruitment report
#' @param quantity
#'  \itemize{
#'   \item recruitment_multipliers
#'   \item standardised_recruitment_multipliers
#'   \item recruits
#' }
#' @return generate a plot over time uses get_BH_recruitment
#' @rdname plot_recruitment
#' @export plot_recruitment
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap aes_string
#' @details
#' If you have multiple time-steps and Recruitment
"plot_recruitment" <- function(model, report_label = NULL, quantity = "ycs_values") {
  if (!quantity %in% c("recruitment_multipliers", "recruits", "standardised_recruitment_multipliers")) {
    stop("quantity, has incorrect values please check ?plot_recruitment")
  }
  UseMethod("plot_recruitment", model)
}
#' @return \code{NULL}
#'
#' @rdname plot_recruitment
#' @method plot_recruitment casal2MPD
#' @export
"plot_recruitment.casal2MPD" <- function(model, report_label = NULL, quantity = "ycs_values") {
  recruit_df <- get_BH_recruitment(model)
  if (!is.null(report_label)) {
    recruit_df <- subset(recruit_df, subset = label %in% report_label)
  }
  ## create a plot
  plt <- ggplot(recruit_df, aes_string(x = "model_year", y = quantity)) +
    xlab("Recruited year") +
    geom_line(size = 2) +
    facet_wrap(~label)
  return(plt)
}

## method for class list
#'
#' @rdname plot_recruitment
#' @method plot_recruitment list
#' @export
"plot_recruitment.list" <- function(model, report_label = NULL, quantity = "ycs_values") {
  recruit_df <- get_BH_recruitment(model)
  if (!is.null(report_label)) {
    recruit_df <- subset(recruit_df, subset = label %in% report_label)
  }
  ggplot(recruit_df, aes_string(x = "model_year", y = quantity, col = "model_label", linetype = "model_label")) +
    geom_line(size = 1.5) +
    labs(colour = "Model", linetype = "Model", x = "Recruited year", y = quantity) +
    facet_wrap(~label)
}

## method for class casal2TAB
#' @return \code{NULL}
#'
#' @rdname plot_recruitment
#' @method plot_recruitment casal2TAB
#' @export
"plot_recruitment.casal2TAB" <- function(model, report_label = NULL, quantity = "ycs_values") {
  stop("plot_recruitment is not implemented for casal2TAB output")
}
