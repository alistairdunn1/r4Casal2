#' @title plot_compositional_observations
#'
#' @description
#' A plotting function for Casal2 fits to age and length compositional data
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract() functions.
#' @param report_labels vector<string> report labels to plot, default is all
#' @param plot.it Whether to generate a default plot or return the values as a matrix.
#' @param plot_type string
#' \itemize{
#'   \item classic
#'   \item residual
#'}
#' @return A ggplot object
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap facet_grid geom_hline geom_ribbon  geom_point scale_colour_manual scale_fill_manual scale_alpha geom_errorbar
#' @rdname plot_compositional_observations
#' @export plot_compositional_observations
#' @examples
#' \dontrun{
#'
#' }

"plot_compositional_observations" <- function(model, report_labels = NULL, plot_type = "classic", plot.it = T) {
  UseMethod("plot_compositional_observations", model)
}

#' @return \code{NULL}
#'
#' @rdname plot_compositional_observations
#' @method plot_compositional_observations casal2MPD
#' @export
"plot_compositional_observations.casal2MPD" <- function(model, report_labels = NULL, plot_type = "classic", plot.it = T) {
  comp_obs = get_composition_observations(model)
  if(is.null(comp_obs)) {
    return("Did not find any composition observations")
  }
  if(!is.null(report_labels)) {
    comp_obs = subset(comp_obs, subset = comp_obs$observation_label %in% report_labels)
  }
  obs = unique(comp_obs$observation_label)
  multiple_iterations_in_a_report = F
  if(length(unique(comp_obs$par_set)) > 1)
    stop("this function cannot deal with multiple -i model runs")


  for(i in 1:length(obs)) {
    this_obs = subset(comp_obs, subset = comp_obs$observation_label == obs[i])
    n_years = length(unique(this_obs$year))
    ## we may want to cut this up TODO later

    ## age or length
    if(grepl(unique(this_obs$observation_type), pattern = "_age")) {
      plt = ggplot(this_obs, aes(x = age)) +
        geom_line(aes(y = expected, col = "Model fit"), size = 1.5, linetype = "dashed") +
        geom_point(aes(y = observed, col = "Observation")) +
        xlab("Age") +
        ylab("") +
        ggtitle(obs[i]) +
        facet_wrap(~year) +
        theme(legend.position = "bottom", axis.text = element_text(size = 14),
              axis.title = element_text(size = 16))

    } else if (grepl(unique(this_obs$observation_type), pattern = "_length")) {
      plt = ggplot(this_obs, aes(x = length)) +
        geom_line(aes(y = expected, col = "Model fit"), size = 1.5, linetype = "dashed") +
        geom_point(aes(y = observed, col = "Observation")) +
        xlab("Age") +
        ylab("") +
        ggtitle(obs[i]) +
        facet_wrap(~year) +
        theme(legend.position = "bottom", axis.text = element_text(size = 14),
              axis.title = element_text(size = 16))
    } else {
      stop(paste0(unique(this_obs$observation_label), " has unknown type for this plot '",unique(this_obs$observation_type), "'"))
    }
    print(plt)
  } 
}
