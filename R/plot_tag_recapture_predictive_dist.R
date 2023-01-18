#' plot_tag_recapture_predictive_dist
#' Plots a violin plot of the predictive distribution by year for an observation
#'
#' @param sim_data matrix of simulated data rows = simulation, col = length or age
#' @param obs a data frame with column names obs and bin (). Can include a column labelled 'mpd_fit' if it exists the plot will add mpd fits on the plot
#' @param lab a label for the plot which clearly identifies the observation
#' @param plot_type = "violin" currently either violin or boxplot.
#' @param type age assumes tag_recapture_by_age or length tag_recapture_by_length
#' @param probs vector of proportions that you want the violin plot to add lines at (optional)
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_violin aes geom_point geom_boxplot aes_string
#' @return a ggplot
#' @rdname plot_tag_recapture_predictive_dist
#' @export plot_tag_recapture_predictive_dist
plot_tag_recapture_predictive_dist <- function(sim_data, obs, lab, plot_type = "violin", type = "length", probs = c(0.025, 0.5, 0.975)) {
  ## quantile function
  if (!all(c("obs", "bin") %in% colnames(obs))) {
    stop("obs must have colnames 'obs', 'bin'. Where bin is the age or length")
  }
  if (!type %in% c("age", "length")) {
    stop("type must be either age or length")
  }

  legend <- c("Posterior Prediction" = "#56B4E9", "Observation" = "black", "MPD" = "#D55E00")
  sim_data_long <- melt(sim_data)
  if (!any(class(obs) == "data.frame")) {
    stop("obs needs to be a dataframe")
  }

  add_mpd_fit <- FALSE
  if (c("mpd_fit") %in% colnames(obs)) {
    add_mpd_fit <- TRUE
  }

  colnames(sim_data_long) <- c("sim", type, "recaptures")
  sim_data_long[, 2] <- factor(sim_data_long[, 2], ordered = T)

  base_plot <- ggplot(sim_data_long, aes_string(x = type)) +
    ggtitle(lab) +
    scale_color_manual(values = legend) +
    guides(fill = "none") +
    labs(colour = "Legend", x = type, y = "Posterior Predictive Distribution") +
    scale_x_discrete(drop = FALSE)
  if (plot_type == "violin") {
    if (is.null(probs)) {
      base_plot <- base_plot + geom_violin(aes(y = recaptures, color = "Posterior Prediction", fill = "Posterior Prediction"), adjust = 2)
    } else {
      base_plot <- base_plot + geom_violin(aes(y = recaptures, color = "Posterior Prediction", fill = "Posterior Prediction"), adjust = 2, draw_quantiles = probs)
    }
  } else {
    base_plot <- base_plot + geom_boxplot(aes(color = "Posterior Prediction", fill = "Posterior Prediction"))
  }
  obs$bin <- factor(obs$bin, ordered = T)
  base_plot <- base_plot + geom_point(data = obs, aes(x = bin, y = obs, color = "Observation", fill = "Observation"), size = 3.4, inherit.aes = F) +
    theme(axis.text.x = element_text(angle = 90))
  if (add_mpd_fit) {
    base_plot <- base_plot + geom_point(data = obs, aes(x = bin, y = mpd_fit, color = "MPD", fill = "MPD"), size = 2.4, inherit.aes = F)
  }
  return(print(base_plot))
}
