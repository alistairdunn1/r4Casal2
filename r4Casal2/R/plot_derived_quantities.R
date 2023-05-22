#' @title plot_derived_quantities default
#'
#' @description
#' A plotting function for Casal2 derived_quantities
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract() functions. If list then we expect multiple mpd runs (should be a named list )
#' @param plot.it Whether to generate a default plot or return the values as a matrix.
#' @param plot_type string
#' @return A ggplot object
#' @importFrom ggplot2 ggplot geom_line aes theme scale_fill_manual scale_alpha ylim labs
#' @rdname plot_derived_quantities
#' @export plot_derived_quantities
#' @examples
#' \donttest{
#' library(Casal2)
#' # plotting Standard Output
#' data <- extract.mpd(file = system.file("extdata", "estimate.out", package = "r4Casal2"))
#' names(data)
#' par(mfrow = c(1, 2))
#' plot_derived_quantities(model = data)
#' }
#'
"plot_derived_quantities" <- function(model) {
  UseMethod("plot_derived_quantities", model)
}

#'
#' @rdname plot_derived_quantities
#' @method plot_derived_quantities casal2MPD
#' @export
"plot_derived_quantities.casal2MPD" <- function(model) {
  ssb_df <- get_derived_quantities(model)
  ssb_df_percent <- ssb_df
  ssb_df_percent$values <- ssb_df_percent$values / ssb_df_percent$initialisation_value * 100
  ssb_df$type <- "Absolute"
  ssb_df_percent$type <- "Percent initial"
  full_ssb_df <- rbind(ssb_df, ssb_df_percent)
  ggplot(full_ssb_df, aes(x = years, y = values, col = dq_label, linetype = dq_label)) +
    geom_line(size = 2) +
    ylim(0, NA) +
    labs(colour = "Label", linetype = "Label", x = "Years", y = "") +
    facet_wrap(~type, scales = "free_y")
}

#'
#' @rdname plot_derived_quantities
#' @method plot_derived_quantities casal2TAB
#' @export
"plot_derived_quantities.casal2TAB" <- function(model) {
  stop("function not coded yet")
}

#'
#' @rdname plot_derived_quantities
#' @method plot_derived_quantities list
#' @export
"plot_derived_quantities.list" <- function(model) {
  # setwd("C://Software//r4Casal2")
  ssb_df <- get_derived_quantities(model)
  ssb_df_percent <- ssb_df
  ssb_df_percent$values <- ssb_df_percent$values / ssb_df_percent$initialisation_value * 100
  ssb_df$type <- "Absolute"
  ssb_df_percent$type <- "Percent initial"
  full_ssb_df <- rbind(ssb_df, ssb_df_percent)
  ggplot(full_ssb_df, aes(x = years, y = values, col = model_label, linetype = model_label)) +
    geom_line(size = 2) +
    ylim(0, NA) +
    labs(colour = "Label", linetype = "Label", x = "Years", y = "") +
    facet_wrap(label ~ type, scales = "free_y", ncol = 2)
}
