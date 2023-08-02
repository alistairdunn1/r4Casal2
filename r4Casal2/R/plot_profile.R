#' plot_profile
#' @author C Marsh
#' @param profile the profile Casal2 output read in from extract.mpd()
#' @param mpd the mpd Casal2 output read in by extract.mpd()
#' @param plot_style a character specifying the plot type
#'  \itemize{
#'   \item individual: separate plots for each component, with filled lines underneath
#'   \item classic: All components on a single plot with lines.
#' }
#' @param objective_function_components either specify 'all' which will plot all likelihood contributions, or a vector of characters for the contirbutions you want. If not sure how to specify these see ask_for_obj_labels
#' @param ask_for_obj_labels boolean if True the function will print the values available for objective_function_components
#' @param likelihood_cut_off ylim for likelihood to remove large values obscuring the trend of the profile
#' @param aggregate_obs bool, whether aggregate_obs observations log-likelihoods over all years
#' @rdname plot_profile
#' @export plot_profile
#' @importFrom RColorBrewer brewer.pal
plot_profile <- function(profile, mpd = NULL, objective_function_components = "all", ask_for_obj_labels = FALSE, likelihood_cut_off = 100, plot_style = "individual", aggregate_obs = TRUE) {
  if (class(profile) != "casal2MPD") {
    stop("profile not of the expected 'class'. We expect 'casal2MPD'")
  }
  if (!is.null(mpd)) {
    if (class(mpd) != "casal2MPD") {
      stop("mpd not of the expected 'class'. We expect 'casal2MPD'")
    }
  }

  if (!plot_style %in% c("individual", "classic")) {
    stop("plot_style needs to be 'individual', or 'classic'")
  }

  this_profile <- get_profile(profile, aggregate_obs)
  this_param <- unique(this_profile$parameter)

  ## reformat labels to help with readability
  this_profile$component <- gsub(this_profile$component, pattern = "observation-", replacement = "O-")
  this_profile$component <- gsub(this_profile$component, pattern = "additional_prior-", replacement = "APr-")
  this_profile$component <- gsub(this_profile$component, pattern = "prior-", replacement = "Pr-")
  this_profile$component <- gsub(this_profile$component, pattern = "penalty-", replacement = "Pen-")
  this_profile$component <- gsub(this_profile$component, pattern = "jacobian-", replacement = "J-")

  if (ask_for_obj_labels) {
    message("Here are the available components to plot:\n")
    return(unique(this_profile$component))
  }
  ## Rescale so min = 0
  this_profile <- this_profile %>%
    group_by(component) %>%
    mutate(rescaled_negative_loglikelihood = abs(negative_loglikelihood - min(negative_loglikelihood)))

  ## subset profile if
  vars_of_interest <- unique(this_profile$component)
  if (length(objective_function_components) > 1 || objective_function_components != "all") {
    if (!all(objective_function_components %in% vars_of_interest)) {
      stop(cat("the objective_function_components are not consistent with outputed values. The options available are 'all', or a subset of \n", paste(unique(this_profile$component), collapse = "\n")))
    }
    vars_of_interest <- objective_function_components
  }
  vars_of_interest <- c(vars_of_interest)
  ## add cut off for plot
  this_profile$rescaled_negative_loglikelihood[this_profile$rescaled_negative_loglikelihood > likelihood_cut_off] <- likelihood_cut_off

  ## get number of components needed to expand colors
  colourCount <- length(vars_of_interest)
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  ##
  plt <- NULL
  if (plot_style == "individual") {
    plt <- ggplot(data = this_profile %>% filter(component %in% vars_of_interest), aes(x = parameter_values, y = rescaled_negative_loglikelihood)) +
      geom_area(stat = "identity", col = "#56B4E9", fill = "#56B4E9") +
      xlab(this_param) +
      ylab("obj - min(obj)") +
      theme_bw() +
      facet_wrap(~component, ncol = 1, scale = "fixed")
  } else {
    plt <- ggplot(data = this_profile %>% filter(component %in% vars_of_interest), aes(x = parameter_values, y = rescaled_negative_loglikelihood)) +
      geom_line(aes(col = component), size = 1.1, linetype = "dashed") +
      labs(x = this_param, y = "obj - min(obj)", col = "") +
      theme_bw() +
      scale_color_manual(values = getPalette(colourCount)) +
      theme(
        legend.position = "right",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)
      ) +
      guides(linetype = "none")
  }
  ## get MPD value if
  if (!is.null(mpd)) {
    est_values <- get_estimated_values(mpd)
    mpd_value <- (est_values %>% filter(parameters == this_param))$values
    plt <- plt + geom_vline(data = NULL, xintercept = mpd_value, color = "black", size = 1, lty = 2)
  }
  plt
}
