#' generate.starting.pars Generates a parameter file that is formatted for -i input into Casal2.
#'
#' This function takes an  Casal2 MPD  object with an estimate_summary report, and returns a free parameters file. Where each parameter is drawn from the prior defined in an @estimate block.
#'
#' @author Alistair Dunn
#' @param MPD <casal2MPD> object that are generated from one of the extract() functions. If list then we expect multiple MPD runs (should be a named list)
#' @param path Optionally, the path to location of the output file
#' @param n_sim the number of randomly generated sets of free parameters
#' @param start_free_filename = the filename of the file created
#' @param all_uniform = logical if TRUE draw from a uniform between bounds regardless of the prior distribution
#' @param quiet Optional, suppresses printing statements

#' @return a file (with default name 'start_free.dat') in the path directory
#' @export
#'
generate.starting.pars <- function(MPD = MPD, n_sim = 10, start_free_filename = "start_free.dat", path = "", all_uniform = FALSE, quiet = T, write = TRUE) {
  rlognorm <- function(number, mu, cv) {
    logvar <- sqrt(log(cv^2 + 1))
    logmean <- log(mu) - (logvar^2) / 2
    return(exp(rnorm(number, logmean, logvar)))
  }

  estimate_blocks <- list()
  found_estimate <- FALSE
  for (i in 1:length(MPD)) {
    ndx <- ifelse(!is.null(MPD[[i]]$type) && MPD[[i]]$type == "estimate_summary", TRUE, FALSE)
    if (ndx) estimate_blocks <- c(estimate_blocks, MPD[[i]])
    if (ndx) found_estimate <- TRUE
  }
  if (!found_estimate) {
    stop("No estimate_summary reports were found in the MPD file supplied")
  }

  param_values <- c()
  N <- length(estimate_blocks) - 1
  if (!quiet) {
    message(paste("A total of", N, "parameters were found"))
  }

  for (i in 1:N) {
    this_estimate <- estimate_blocks[[i]]
    values <- NULL
    if (this_estimate$sub_type == "uniform" || all_uniform) {
      values <- runif(n = n_sim, min = as.numeric(this_estimate$lower_bound), max = as.numeric(this_estimate$upper_bound))
    } else if (this_estimate$sub_type == "uniform_log") {
      values <- exp(runif(n = n_sim, min = log(as.numeric(this_estimate$lower_bound)), max = log(as.numeric(this_estimate$upper_bound))))
    } else if (this_estimate$sub_type == "normal") {
      std_dev <- as.numeric(this_estimate$hyperparameter_values[1]) * as.numeric(this_estimate$hyperparameter_values[2])
      values <- rnorm(n = n_sim, mean = as.numeric(this_estimate$this_estimate$hyperparameter_values[1]), sd = std_dev)
    } else if (this_estimate$sub_type == "lognormal") {
      values <- rlognorm(n = n_sim, mu = as.numeric(this_estimate$hyperparameter_values[1]), cv = as.numeric(this_estimate$hyperparameter_values[2]))
    } else if (this_estimate$sub_type == "normal_by_stdev") {
      values <- rnorm(n = n_sim, mean = as.numeric(this_estimate$hyperparameter_values[1]), sd = as.numeric(this_estimate$hyperparameter_values[2]))
    } else if (this_estimate$sub_type == "normal_log") {
      values <- exp(rnorm(n = n_sim, mean = as.numeric(this_estimate$hyperparameter_values[1]), sd = as.numeric(this_estimate$hyperparameter_values[2])))
    }
    ## set to bounds if generated past them
    values[values < as.numeric(this_estimate$lower_bound)] <- as.numeric(this_estimate$lower_bound)
    values[values > as.numeric(this_estimate$upper_bound)] <- as.numeric(this_estimate$upper_bound)
    param_values <- cbind(param_values, values)
  }
  param_labels <- names(estimate_blocks)[-length(names(estimate_blocks))]
  colnames(param_values) <- param_labels

  filename <- make.filename(path = path, file = start_free_filename)
  cat(param_labels, file = filename, sep = " ", fill = F, labels = NULL, append = F)
  cat("\n", file = filename, sep = " ", fill = F, labels = NULL, append = T)
  for (i in 1:nrow(param_values)) {
    cat(param_values[i, ], file = filename, sep = " ", fill = F, labels = NULL, append = T)
    cat("\n", file = filename, sep = " ", fill = F, labels = NULL, append = T)
  }
  invisible(param_values)
}
# MPD <- extract.mpd(make.filename("estimate.log", PATH))
# a <- generate.starting.pars(MPD = MPD, n_sim = 10, start_free_filename = "start_free.dat", path =PATH, all_uniform = FALSE, quiet = T)
