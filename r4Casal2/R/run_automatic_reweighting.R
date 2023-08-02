#' run_automatic_reweighting
#' @description a function that will automatically conduct an iterative reweighting re-estimation.
#' @author Craig Marsh
#' @param config_dir directory that contains config. See details for info on nested folder configs
#' @param config_filename the config file that describes all Casal2 input files. Expected to be in 'config_dir'.
#' @param weighting_folder_name A folder name created in 'config_dir' where re-estimation is done and output saved.
#' @param mpd_file_name filename with mpd output to start calculating weights for. Expected to be in 'config_dir'.
#' @param n_loops number of iterative loops
#' @param dash_i_par_filename parameter filename compatible with -i format. Useful to start models close to solution for complex models
#' @param prompt_user_before_deleting if FALSE will delete previous weighting_folder_name without prompting user.
#' @param observation_labels_to_exclude if you do not want to weight a compositional data set, include it in this parameter using the label i.e. observation[label].
#' @param verbose print additional information to screen
#' @param approximate_single_year_obs whether to try and approximate a weight for observations with a single year
#' @details Sometimes users may have subdirectories containing config files. This function is untested for this config model structure.
#' To read in the reweighted outputs from this function see the function extract_reweighted_mpds.
#' @rdname run_automatic_reweighting
#' @export run_automatic_reweighting
#' @return data frame of weights in each loop. Will also create estimated mpd output in weighting_folder_name with the format 'estimate_"iteration_number".log'
run_automatic_reweighting <- function(config_dir,
                                      config_filename = "config.csl2",
                                      weighting_folder_name = "Reweight",
                                      mpd_file_name = "estimate.log",
                                      n_loops = 3,
                                      observation_labels_to_exclude = NULL,
                                      dash_i_par_filename = NULL,
                                      prompt_user_before_deleting = TRUE,
                                      verbose = T,
                                      approximate_single_year_obs = FALSE) {
  ## check files exists
  if (!file.exists(file.path(config_dir, config_filename))) {
    stop(paste0("Could not find ", config_filename, " at ", config_dir))
  }
  if (!file.exists(file.path(config_dir, mpd_file_name))) {
    stop(paste0("Could not find ", mpd_file_name, " at ", config_dir))
  }
  if (!is.null(dash_i_par_filename)) {
    if (!file.exists(file.path(config_dir, dash_i_par_filename))) {
      stop(paste0("Could not find ", dash_i_par_filename, " at ", config_dir))
    }
  }
  ## check if weighting_directory already exists and
  working_dir <- file.path(config_dir, weighting_folder_name)
  if (dir.exists(working_dir)) {
    if (prompt_user_before_deleting) {
      result <- menu(c("Yes", "No"), title = "Do you want to delete the existing weighting_folder_name?")
      if (result == 2) {
        return(stop("exiting function because you don't want to delete weighting_folder_name"))
      }
    }
    unlink(working_dir, recursive = T, force = T)
    if (verbose) {
      print("deleting 'working_dir'")
    }
  }
  dir.create(working_dir)
  if (verbose) {
    print(paste0("creating 'working_dir' ", working_dir))
  }

  ## get the csl files and copy them to
  file.copy(from = file.path(config_dir, config_filename), to = file.path(working_dir, config_filename))
  if (!is.null(dash_i_par_filename)) {
    file.copy(from = file.path(config_dir, dash_i_par_filename), to = file.path(working_dir, dash_i_par_filename))
  }
  ##
  config_file_in <- scan(file = file.path(config_dir, config_filename), what = "", sep = "\n", quiet = T)
  ## deal with comments
  config_file_in <- StripComments(config_file_in)
  ## ignore all file lines that are not an !include
  include_lines <- grepl(pattern = "!include", config_file_in)
  config_file_in <- config_file_in[include_lines]
  ## get includes assumes file names have \" \"
  config_file_in <- substring(config_file_in, first = 10) # '!include ' is 10 characters
  ## check for '"'
  ndx <- regexpr("\"", config_file_in) > 0
  if (any(ndx)) {
    for (i in 1:length(config_file_in)) {
      if (ndx[i]) {
        config_file_in[i] <- substring(config_file_in[i], first = 2, last = nchar(config_file_in[i]) - 1)
      }
    }
  }
  if (verbose) {
    cat("found the following files to read in ", config_file_in, "\n")
  }

  ## copy these files over
  for (i in 1:length(config_file_in)) {
    file.copy(from = file.path(config_dir, config_file_in[i]), to = file.path(working_dir, config_file_in[i]))
  }
  ## @observation blocks can be any where any in these which is a pain but we will cope

  ## now we work
  mpd <- extract.mpd(path = config_dir, file = mpd_file_name)
  final_weights <- initial_stage_two_weights <- calculate_composition_stage_two_weights(mpd, approximate_single_year_obs = approximate_single_year_obs)
  ##
  for (loop_iter in 1:n_loops) {
    if (verbose) {
      cat("\nloop iter ", loop_iter, "\n")
    }
    if (loop_iter > 1) {
      ## read in mpd and calculate stage two weights
      mpd <- extract.mpd(path = working_dir, file = paste0("estimate_", loop_iter - 1, ".log"))
      initial_stage_two_weights <- calculate_composition_stage_two_weights(mpd, approximate_single_year_obs = approximate_single_year_obs)
      final_weights <- cbind(final_weights, initial_stage_two_weights$weight)
    }
    ## loop through all the config_file_in, if it has an @observation block
    ## that matches our weighting observations then change it
    for (file_iter in 1:length(config_file_in)) {
      this_csl2 <- extract.csl2.file(path = working_dir, file = config_file_in[file_iter], quiet = T)
      blocks <- (sapply(strsplit(names(this_csl2), split = "\\["), "[", 1))
      labels <- (sapply(strsplit(names(this_csl2), split = "\\["), "[", 2))
      labels <- (sapply(strsplit(labels, split = "\\]"), "[", 1))
      if (any(blocks == "observation")) {
        obs_ndx <- which(blocks %in% "observation")
        for (obs_iter in obs_ndx) {
          if (labels[obs_iter] %in% observation_labels_to_exclude) {
            next
          }
          if (labels[obs_iter] %in% initial_stage_two_weights$observation) {
            ## adjust the observation
            stage_ndx <- initial_stage_two_weights$observation %in% labels[obs_iter]
            if (is.na(initial_stage_two_weights$weight[stage_ndx])) {
              next
            }
            if (verbose) {
              cat("adjusting ", labels[obs_iter], " with weight = ", initial_stage_two_weights$weight[stage_ndx], "\n")
            }
            this_obs <- this_csl2[[obs_iter]]
            # change the subcommand 'error_value_multiplier'
            if (exists(x = "error_value_multiplier", where = this_obs)) {
              this_obs$error_value_multiplier$value <- as.numeric(this_obs$error_value_multiplier$value) * initial_stage_two_weights$weight[stage_ndx]
            } else {
              this_obs$error_value_multiplier <- list()
              this_obs$error_value_multiplier$value <- initial_stage_two_weights$weight[stage_ndx]
            }
            ## save back in
            this_csl2[[obs_iter]] <- this_obs
          }
        }
        ## write the file back out
        write.csl2.file(object = this_csl2, path = working_dir, file = config_file_in[file_iter])
      }
    }
    ## now re-estimate
    current_dir <- getwd()
    setwd(working_dir)
    args <- paste0("-e -o est_pars_", loop_iter, ".par")
    if (!is.null(dash_i_par_filename)) {
      args <- paste0("-e -o est_pars_", loop_iter, ".par -i ", dash_i_par_filename)
    }
    if (verbose) {
      cat(paste0("Running casal2 ", args, ", and waiting for it..."))
    }
    system2(
      command = "casal2", args = args,
      stdout = file.path(working_dir, paste0("estimate_", loop_iter, ".log")),
      stderr = file.path(working_dir, paste0("estimate_", loop_iter, ".err")), wait = TRUE
    )
    setwd(current_dir)
  }
  return(final_weights)
}
