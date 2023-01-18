#' plot_abundance_predictive_dist
#' Plots a violin plot of the predictive distribution by year for an observation
#'
#' @param sim_data matrix of simulated data rows = year, col = simulations. This function assumes rownames(sim_data) = year
#' @param obs a data frame with columns obs and year. Can include a column labelled 'mpd_fit' if it exists the plot will add mpd fits on the plot
#' @param lab a label for the plot which clearly identifies the observation
#' @param plot_type = "violin" currently either violin or boxplot.
#' @param probs vector of proportions that you want the violin plot to add lines at (optional)
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_violin aes geom_point geom_boxplot
#' @return a ggplot
#' @rdname plot_abundance_predictive_dist
#' @export plot_abundance_predictive_dist
plot_abundance_predictive_dist <- function(sim_data, obs, lab, plot_type = "violin", probs = c(0.025, 0.5, 0.975)) {
  ## quantile function

  legend <- c("Posterior Prediction" = "#56B4E9", "Observation" = "black", "MPD" = "#D55E00")
  sim_data_long <- melt(sim_data)
  if (!any(class(obs) == "data.frame")) {
    stop("obs needs to be a dataframe")
  }
  if (sum(c("obs", "year") %in% colnames(obs)) != 2) {
    stop("obs parameter needs colnames 'obs', 'year'")
  }
  add_mpd_fit <- FALSE
  if (c("mpd_fit") %in% colnames(obs)) {
    add_mpd_fit <- TRUE
  }

  colnames(sim_data_long) <- c("year", "sim", "CPUE")
  yr_lvls <- range(sim_data_long$year)
  sim_data_long$year <- factor(sim_data_long$year, ordered = T, levels = yr_lvls[1]:yr_lvls[2])
  base_plot <- ggplot(sim_data_long, aes(x = year, y = CPUE)) +
    ggtitle(lab) +
    scale_color_manual(values = legend) +
    guides(fill = "none") +
    labs(colour = "Legend", x = "Year", y = "Posterior Predictive Distribution") +
    scale_x_discrete(drop = FALSE)
  if (plot_type == "violin") {
    if (is.null(probs)) {
      base_plot <- base_plot + geom_violin(aes(color = "Posterior Prediction", fill = "Posterior Prediction"))
    } else {
      base_plot <- base_plot + geom_violin(aes(color = "Posterior Prediction", fill = "Posterior Prediction"), draw_quantiles = probs)
    }
  } else {
    base_plot <- base_plot + geom_boxplot(aes(color = "Posterior Prediction", fill = "Posterior Prediction"))
  }

  base_plot <- base_plot + geom_point(data = obs, aes(x = factor(year), y = obs, color = "Observation", fill = "Observation"), size = 3.4, inherit.aes = F) +
    theme(axis.text.x = element_text(angle = 90))
  if (add_mpd_fit) {
    base_plot <- base_plot + geom_point(data = obs, aes(x = factor(year), y = mpd_fit, color = "MPD", fill = "MPD"), size = 2.4, inherit.aes = F)
  }
  return(print(base_plot))
}
