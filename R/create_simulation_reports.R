#' @title create_simulation_reports
#'
#' @description
#' A function to create a simulated report csl file so that you can add it with an !include statement for simulations.
#'
#' @author Craig Marsh
#' @param config_dir path directory
#' @param config_files vector of string names that correspond to Casal2
#' @param output_folder folder directory label to write simulated reports into
#' @param prefix prefex for simulated observations
#' @param single_sim_filename optional filename, if provided then will report simulated observations into a single file, rather than separate files.
#' @param verbose true print out statements through function to help debug
#' @return will create an output directory based on parameter output_folder and also a Casal2 report config called simulated_reports.csl2
#' @rdname create_simulation_reports
#' @export create_simulation_reports
#' @importFrom Casal2 extract.csl2.file write.csl2.file
#' @examples
#' \donttest{
#' library(Casal2)
#' # plotting Standard Output
#' config_dir <- system.file("extdata", "PosteriorPredictiveChecks", package = "r4Casal2", mustWork = TRUE)
#' config_files <- "Observation.csl2"
#' obs <- create_simulation_reports(config_dir = config_dir, config_files = config_files)
#' }
create_simulation_reports <- function(config_dir = "", config_files, output_folder = "simulated_observations", prefix = "sim_", single_sim_filename = NULL, verbose = FALSE) {
  if (verbose) {
    cat("enter: create_simulation_reports\n")
  }
  if (config_dir == "") {
    config_dir <- getwd()
    if (verbose) {
      cat("set config_dir to getwd()\n")
    }
  }
  output_dir <- file.path(config_dir, output_folder)
  config_path_file <- file.path(config_dir, config_files)
  for (i in 1:length(config_path_file)) {
    if (!file.exists(config_path_file[i])) {
      stop(paste0("couldn't find the file ", config_path_file[i]))
    }
  }
  if (dir.exists(output_dir)) {
    if (verbose) {
      cat(paste0("output_folder ", output_folder, " exists so don't need to create it\n"))
    }
  } else {
    dir.create(output_dir)
    if (verbose) {
      cat("creating output folder")
    }
  }
  observations_found <- vector()
  sim_report_csl <- list()
  for (i in 1:length(config_files)) {
    this_config <- tryCatch(expr = extract.csl2.file(file = config_path_file[i], quiet = T), error = function(e) {
      e
    }, warning = function(w) {
      w
    })
    if (inherits(this_config, "error") | inherits(this_config, "warning")) {
      stop(paste0("failed to extract.csl2.file for ", config_path_file[i], " error = ", this_config$message))
    }
    block_type_labels <- names(this_config)
    block_labels <- sapply(strsplit(block_type_labels, split = "\\["), "[", 2)
    block_labels <- sapply(strsplit(block_labels, split = "\\]"), "[", 1)
    obs_ndx <- which(tolower(substring(block_type_labels, first = 0, last = 11)) == "observation")
    if (length(obs_ndx) > 0) {
      csl_list_entry <- paste0("report[", prefix, block_labels[obs_ndx], "]")
      for (j in 1:length(obs_ndx)) {
        sim_report_csl[[csl_list_entry[j]]] <- NULL
        sim_report_csl[[csl_list_entry[j]]]$type <- NULL
        sim_report_csl[[csl_list_entry[j]]]$observation <- NULL
        sim_report_csl[[csl_list_entry[j]]]$file_name <- NULL
        sim_report_csl[[csl_list_entry[j]]]$type$value <- "simulated_observation"
        sim_report_csl[[csl_list_entry[j]]]$observation$value <- block_labels[obs_ndx][j]
        if (is.null(single_sim_filename)) {
          sim_report_csl[[csl_list_entry[j]]]$file_name$value <- paste0(output_folder, .Platform$file.sep, paste0(prefix, block_labels[obs_ndx][j]))
        } else {
          sim_report_csl[[csl_list_entry[j]]]$file_name$value <- paste0(output_folder, .Platform$file.sep, single_sim_filename)
          sim_report_csl[[csl_list_entry[j]]]$write_mode <- NULL
          sim_report_csl[[csl_list_entry[j]]]$write_mode$value <- "append"
        }
        observations_found <- c(observations_found, block_labels[obs_ndx][j])
      }
    } else {
      cat(paste0("couldn't find @observation in file ", config_path_file[i]))
    }
  }
  if (verbose) {
    cat("write report csl2\n")
  }
  write.csl2.file(object = sim_report_csl, file = "simulated_reports.csl2", path = config_dir)
  if (verbose) {
    cat("exit: create_simulation_reports\n")
  }
  return(list(observations_found = observations_found))
}
