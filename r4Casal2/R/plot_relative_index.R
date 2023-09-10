#' @title plot_relative_index default
#' @description A plotting function for Casal2 fits to abundance or biomass observations
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract() functions.
#' @param report_labels vector<string> report labels to plot, default is all
#' @param plot.it Whether to generate a default plot
#' @param plot_type string
#' \itemize{
#'   \item classic
#'   \item classic_ribbon
#'   \item residual
#' }
#' @return A ggplot object
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap facet_grid geom_hline geom_ribbon  geom_point scale_colour_manual scale_fill_manual scale_alpha geom_errorbar
#' @rdname plot_relative_index
#' @export plot_relative_index
#' @examples
#' \dontrun{
#' library(casal2)
#' # plotting Standard Output
#' data <- extract.mpd(file = system.file("extdata", "estimate.out", package = "casal2"))
#' names(data)
#' par(mfrow = c(1, 2))
#' plot.fits(model = data, report_labels = "westF_at_age")
#' plot.fits(model = data, report_labels = "eastF_at_age")
#' # to create a user-specified plot, use plot.it = FALSE.
#' Tangaroa_fits <- plot.fits(model = data, report_label = "eastF_at_age", plot.it = FALSE)
#' }
"plot_relative_index" <- function(model, report_labels = NULL, plot_type = "classic", plot.it = TRUE) {
  UseMethod("plot_relative_index", model)
}
#' @return \code{NULL}
#'
#' @rdname plot_relative_index
#' @method plot_relative_index casal2MPD
#' @export
"plot_relative_index.casal2MPD" <- function(model, report_labels = NULL, plot_type = "classic", plot.it = TRUE) {
  abundance_obs <- get_abundance_observations(model)
  if (is.null(abundance_obs)) {
    return("Did not find any abundance observations")
  }
  multiple_iterations_in_a_report <- F
  if (length(unique(abundance_obs$par_set)) > 1) {
    multiple_iterations_in_a_report <- T
  }
  ## subset to specific obs
  if (!is.null(report_labels)) {
    abundance_obs <- subset(abundance_obs, subset = abundance_obs$observation_label %in% report_labels)
  }
  ## create plot
  plt <- NULL
  col_pallete <- c("observed" = "#f04546", "expected" = "#3591d1", "95% CI" = "#62c76b")
  if (plot_type == "classic") {
    plt <- ggplot(abundance_obs, aes(x = year, group = observation_label, col = observation_label)) +
      geom_errorbar(aes(ymin = L_CI, ymax = U_CI, col = "95% CI")) +
      geom_point(aes(y = observed, col = "observed"), size = 2) +
      geom_point(aes(y = expected, col = "expected"), size = 2) +
      geom_line(aes(y = expected, col = "expected"), size = 1.5, linetype = "dashed") +
      scale_colour_manual(name = "Key", values = col_pallete)

    if (multiple_iterations_in_a_report) {
      plt <- plt + facet_grid(par_set ~ observation_label, scales = "free_y")
    } else {
      plt <- plt + facet_wrap(~observation_label, scales = "free_y")
    }
  } else if (plot_type == "classic_ribbon") {
    plt <- ggplot(abundance_obs, aes(x = year, group = observation_label, col = observation_label)) +
      geom_ribbon(aes(ymin = L_CI, ymax = U_CI, alpha = 0.2, col = "95% CI", fill = "95% CI")) +
      geom_point(aes(y = observed, col = "observed"), size = 2) +
      geom_point(aes(y = expected, col = "expected"), size = 2) +
      scale_colour_manual(name = "Key", values = col_pallete) +
      scale_fill_manual(name = "Key", values = col_pallete) +
      scale_alpha(guide = "none") +
      if (multiple_iterations_in_a_report) {
        plt <- plt + facet_grid(par_set ~ observation_label, scales = "free_y")
      } else {
        plt <- plt + facet_wrap(~observation_label, scales = "free_y")
      }
  } else if (plot_type == "residual") {
    if ("pearsons_residuals" %in% colnames(abundance_obs)) {
      plt <- ggplot(abundance_obs, aes(x = year, y = pearsons_residuals, group = observation_label, col = observation_label)) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
        ylab("Pearson Residuals")
    } else if ("normalised_residuals" %in% colnames(abundance_obs)) {
      plt <- ggplot(abundance_obs, aes(x = year, y = normalised_residuals, group = observation_label, col = observation_label)) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
        ylab("Normalised Residuals")
    } else {
      plt <- ggplot(abundance_obs, aes(x = year, y = residuals, group = observation_label, col = observation_label)) +
        geom_point(size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", size = 1.2) +
        ylab("Raw Residuals (O - E)")
    }

    if (multiple_iterations_in_a_report) {
      plt <- plt + facet_grid(par_set ~ observation_label, scales = "free_y")
    } else {
      plt <- plt + facet_wrap(~observation_label, scales = "free_y")
    }
  }

  if (plot.it) {
    print(plt)
  }
  invisible(plt)
}
#' @return \code{NULL}
#'
#' @rdname plot_relative_index
#' @method plot_relative_index casal2TAB
#' @export
"plot_relative_index.casal2TAB" <- function(model, report_labels = NULL, plot_type = "classic", plot.it = T) {
  stop("plot_recruitment is not implemented for casal2TAB output")
}
