#' @title read_simulated_data
#'
#' @description
#' A utility function for reading in Casal2 simulated data sets. For help on simulated data from Casal2 see
#'
#' @author Craig Marsh
#' @param dir path directory to a folder containing simulated observation from a casal2 -s run
#' @param mean_age <bool> if data is composition. True will return mean age/length for each year and data set. Otherwise False will return a list. each element will be a year which will contain a matrix of simulations x bin simulated values.
#' @param verbose true print out statements through function to help debug
#' @return a list of all the simulated data sets. in each element there is a matrix with rows = year, col = simulation reps
#' @export
#' @rdname read_simulated_data
#' @export read_simulated_data

read_simulated_data <- function(dir, verbose = FALSE, mean_age = TRUE) {
  ## add to this as the function grows
  currently_implemented_obs <- c("biomass", "abundance", "process_removals_by_length", "proportions_at_length", "process_removals_by_age", "proportions_at_age", "process_proportions_migrating", "tag_recapture_by_length")

  if (verbose) {
    cat("enter: read_simulated_data\n")
  }

  sim_file_names <- unique(sapply(strsplit(list.files(dir), split = "\\."), "[", 1))
  extensions <- unique(sapply(strsplit(list.files(dir), split = "\\."), "[", 2))
  # check obs are formatted as expected
  if (!all(grepl(extensions, pattern = "_"))) {
    stop(paste0("found a filename with an extension '", extensions[!grepl(extensions, pattern = "_")], "', this is unexpected. Possibly have file_name with multiple '.' in it."))
  }
  n_obs <- length(sim_file_names)
  if (verbose) {
    cat("found ", n_obs, " simulated observation sets\n")
    cat("with ", length(extensions), " simulations\n")
  }

  labs <- sim_file_names
  sim_obs <- list() ##
  ## initialise
  for (n in 1:n_obs) {
    sim_obs[[n]] <- NULL
  }
  ## start the main loop
  failed_files <- vector()
  for (n in 1:n_obs) {
    if (verbose) {
      cat("reading obs = ", sim_file_names[n], "\n")
    }
    for (i in 1:length(extensions)) {
      cas_sim_obs <- tryCatch(expr = extract.csl2.file(file = paste0(sim_file_names[n], ".", extensions[i]), path = dir, quiet = T), error = function(e) {
        e
      }, warning = function(w) {
        w
      })
      if (inherits(cas_sim_obs, "error") | inherits(cas_sim_obs, "warning")) {
        failed_files <- c(failed_files, paste0(paste0(sim_file_names[n], ".", extensions[i])))
        next
      }
      this_ob <- cas_sim_obs[[1]]
      if (!this_ob$type$value %in% currently_implemented_obs) {
        cat(paste0("file ", sim_file_names[n], ".", extensions[i], " is of type = ", this_ob$type$value, ". Currently only ", paste(currently_implemented_obs, collapse = ", "), " are implemented"))
        break
      }
      if (this_ob$type$value %in% c("tag_recapture_by_length")) {
        ## relative index obs
        recaptures_table <- Reduce(rbind, this_ob$Table$recaptured[this_ob$years$value])
        scanned_table <- Reduce(rbind, this_ob$Table$scanned[this_ob$years$value])

        class(recaptures_table) <- "numeric"
        if (is.null(dim(recaptures_table))) {
          recaptures_table <- matrix(recaptures_table, nrow = 1)
        }
        rownames(recaptures_table) <- NULL
        class(scanned_table) <- "numeric"
        if (is.null(dim(scanned_table))) {
          scanned_table <- matrix(scanned_table, nrow = 1)
        }
        rownames(scanned_table) <- NULL
        colnames(recaptures_table) <- colnames(scanned_table) <- this_ob$length_bins$value
        ## number of recaptures
        sim_obs[n][[1]] <- rbind(sim_obs[n][[1]], recaptures_table * scanned_table)
      }
      if (this_ob$type$value %in% c("biomass", "abundance")) {
        ## relative index obs
        obs_table <- Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(obs_table) <- "numeric"
        if (is.null(dim(obs_table))) {
          obs_table <- matrix(obs_table, nrow = 1)
        }
        rownames(obs_table) <- NULL
        sim_obs[n][[1]] <- cbind(sim_obs[n][[1]], obs_table[, 1])
      } else if (this_ob$type$value %in% c("process_removals_by_length", "proportions_at_length")) {
        comp <- Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(comp) <- "numeric"
        rownames(comp) <- NULL
        if (is.null(dim(comp))) {
          comp <- matrix(comp, nrow = 1)
        }
        bins <- this_ob$length_bins$value
        if (!is.null(this_ob$length_plus$value) | this_ob$length_plus$value %in% c("False", "FALSE", "F", "no", "0")) {
          bins <- bins[-length(bins)]
        }
        if (mean_age) {
          if (is.null(this_ob$simulated_data_sum_to_one) || this_ob$simulated_data_sum_to_one %in% c("True", "TRUE", "T", "yes", "1")) {
            stop(paste0("file ", sim_file_names[n], ".", extensions[i], " doesn't sum = 1. If mean_age = T then re-simulate data with simulated_data_sum_to_one = true in Casal2 observation block"))
          }
          mean_bin <- comp %*% as.numeric(bins)
          sim_obs[n][[1]] <- cbind(sim_obs[n][[1]], mean_bin)
        } else {
          if (i == 1) {
            temp_list <- list()
            for (y in 1:length(this_ob$years$value)) {
              temp_list[[this_ob$years$value[y]]] <- matrix(NA, nrow = ncol(comp), ncol = length(extensions))
            }
            sim_obs[[n]] <- temp_list
          }
          for (y in 1:length(this_ob$years$value)) {
            sim_obs[n][[1]][[this_ob$years$value[y]]][, i] <- comp[y, ]
          }
        }
      } else if (this_ob$type$value %in% c("process_removals_by_age", "proportions_at_age")) {
        comp <- Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(comp) <- "numeric"
        rownames(comp) <- NULL
        if (is.null(dim(comp))) {
          comp <- matrix(comp, nrow = 1)
        }
        bins <- as.numeric(this_ob$min_age):as.numeric(this_ob$max_age$value)
        if (mean_age) {
          if (is.null(this_ob$simulated_data_sum_to_one) || this_ob$simulated_data_sum_to_one$value %in% c("True", "TRUE", "T", "yes", "1")) {
            stop(paste0("file ", sim_file_names[n], ".", extensions[i], " doesn't sum = 1. If mean_age = T then re-simulate data with simulated_data_sum_to_one = true in Casal2 observation block"))
          }
          mean_bin <- comp %*% as.numeric(bins)
          sim_obs[n][[1]] <- cbind(sim_obs[n][[1]], mean_bin)
        } else {
          if (i == 1) {
            temp_list <- list()
            for (y in 1:length(this_ob$years$value)) {
              temp_list[[this_ob$years$value[y]]] <- matrix(NA, nrow = ncol(comp), ncol = length(extensions))
            }
            sim_obs[[n]] <- temp_list
          }
          for (y in 1:length(this_ob$years$value)) {
            sim_obs[n][[1]][[this_ob$years$value[y]]][, i] <- comp[y, ]
          }
        }
      } else if (this_ob$type$value %in% c("process_proportions_migrating")) {
        sim_mat <- Reduce(rbind, this_ob$Table$obs[this_ob$years$value])
        class(sim_mat) <- "numeric"
        rownames(sim_mat) <- NULL
        bins <- as.numeric(this_ob$min_age$value):as.numeric(this_ob$max_age$value)
        if (is.null(dim(sim_mat))) {
          sim_mat <- matrix(sim_mat, nrow = 1)
        }
        colnames(sim_mat) <- bins
        rownames(sim_mat) <- this_ob$years$value
        if (i == 1) {
          temp_list <- list()
          for (y in 1:length(this_ob$years$value)) {
            temp_list[[this_ob$years$value[y]]] <- matrix(NA, nrow = ncol(sim_mat), ncol = length(extensions))
          }
          sim_obs[[n]] <- temp_list
        }
        for (y in 1:length(this_ob$years$value)) {
          sim_obs[n][[1]][[this_ob$years$value[y]]][, i] <- sim_mat[y, ]
        }
      }
    }
  }
  names(sim_obs) <- sim_file_names

  if (verbose) {
    cat("exit: read_simulated_data\n")
  }
  return(list(sim_obs = sim_obs, failed_files = failed_files))
}
