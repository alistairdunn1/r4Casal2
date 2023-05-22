#' @title summarise_config
#' @description
#' utility function that reads in config.csl2 file and summarises a model in table formats and plots
#'
#' @author Craig Marsh
#' @param config_dir path directory
#' @param config_file the starting config that starts a Casal2 model default config.csl2, but can be overridden with casal2 -c my_config. in the later case it should be the my_config name
#' @param quiet <bool> print out additional information to console. Useful for debugging
#' @param fileEncoding <string> for different utf encodings
#' @return data.frame with different Casal2 input values
#' @importFrom Casal2 extract.csl2.file
#' @importFrom reshape2 melt
#' @examples
#' \dontrun{
#' library(r4Casal2)
#' library(ggplot2)
#' config_dir <- system.file("extdata", "TestModelComplex", package = "r4Casal2", mustWork = TRUE)
#' summary <- summarise_config(config_dir, config_file = "config.csl2", quiet = T)
#' names(summary)
#' # visualise observations in model
#' ggplot(summary$obs_year_df, aes(x = year, y = observation, col = observation, size = active)) +
#'   geom_point() +
#'   guides(colour = "none", size = "none")
#' config_dir <- "C:\\Users\\marshc\\OneDrive - NIWA\\22_23\\SNA1\\csl\\P30 Base HGBP\\Casal2"
#' }
#' @rdname summarise_config
#' @export summarise_config
summarise_config <- function(config_dir = "", config_file = "config.csl2", quiet = T, fileEncoding = "") {
  ## check file exists
  if (!file.exists(file.path(config_dir, config_file))) {
    stop(paste0("Could not find ", config_file, " at ", config_dir))
  }

  config_file_in <- scan(file = file.path(config_dir, config_file), what = "", sep = "\n", quiet = T)
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
  if (!quiet) {
    cat("found the following files to read in ", config_file_in, "\n")
  }
  ## now read in these config files
  model_block <- list()
  observation_blocks <- list()
  derived_quantity_blocks <- list()
  process_blocks <- list()
  estimate_blocks <- list()
  time_steps_list <- list()
  categories_list <- list()
  age_length_list <- list()
  length_weight_list <- list()
  growth_list <- list()
  category_labels <- NULL
  observation_labels <- NULL
  category_age_lengths <- NULL
  category_growth_increments <- NULL
  category_format <- NULL
  model_years <- NULL
  model_length_bins <- NULL
  ages <- NULL
  time_steps <- NULL
  length_based_model <- F
  for (i in 1:length(config_file_in)) {
    if (!file.exists(file.path(config_dir, config_file_in[i]))) {
      cat("couldn't find file = ", file.path(config_dir, config_file_in[i]))
    }
    this_file <- tryCatch(extract.csl2.file(path = config_dir, file = config_file_in[i], quiet = quiet), error = function(e) {
      e
    }, warning = function(w) {
      w
    })
    if (inherits(this_file, "error") | inherits(this_file, "warning")) {
      cat("failed to readin the following file ", config_file_in[i], " so skipping it.\n\nthe error\n", this_file$message, "\n")
      next
    }
    blocks <- get_block(names(this_file))
    labels <- get_label(names(this_file))
    for (j in 1:length(this_file)) {
      if (tolower(blocks[j]) == "model") {
        if (is.null(this_file[[j]]$type)) {
          length_based_model <- FALSE
          ## default is age based
        } else {
          if (this_file[[j]]$type$value == "length") {
            length_based_model <- TRUE
          } else {
            length_based_model <- FALSE
          }
        }
        model_block[["model"]] <- this_file[[j]]
        model_years <- as.numeric(this_file[[j]]$start_year$value):as.numeric(this_file[[j]]$final_year$value)
        # could be a length based model
        if (!is.null(this_file[[j]]$min_age)) {
          ages <- as.numeric(this_file[[j]]$min_age$value):as.numeric(this_file[[j]]$max_age$value)
        }
        time_steps <- this_file[[j]]$time_steps$value
        if (!is.null(this_file[[j]]$length_bins)) {
          for (k in 1:length(this_file[[j]]$length_bins$value)) {
            model_length_bins <- c(model_length_bins, expand_shorthand_syntax(syntax = this_file[[j]]$length_bins$value[k]))
          }
        }
      } else if (tolower(blocks[j]) == "time_step") {
        time_steps_list[[labels[j]]] <- this_file[[j]]$processes$value
      } else if (tolower(blocks[j]) == "categories") {
        for (k in 1:length(this_file[[j]]$names$value)) {
          category_labels <- c(category_labels, expand_category_block(categories = this_file[[j]]$names$value[k]))
        }
        if (!is.null(this_file[[j]]$age_lengths)) {
          for (k in 1:length(this_file[[j]]$age_lengths$value)) {
            category_age_lengths <- c(category_age_lengths, expand_shorthand_syntax(syntax = this_file[[j]]$age_lengths$value[k]))
          }
        }
        if (!is.null(this_file[[j]]$growth_increment)) {
          for (k in 1:length(this_file[[j]]$growth_increment$value)) {
            category_growth_increments <- c(category_growth_increments, expand_shorthand_syntax(syntax = this_file[[j]]$growth_increment$value[k]))
          }
        }
        if (!is.null(this_file[[j]]$format)) {
          category_format <- this_file[[j]]$format$value
        }
      } else if (tolower(blocks[j]) == "age_length") {
        age_length_list[[labels[j]]] <- this_file[[j]]
      } else if (tolower(blocks[j]) == "growth_increment") {
        growth_list[[labels[j]]] <- this_file[[j]]
      } else if (tolower(blocks[j]) == "process") {
        process_blocks[[labels[j]]] <- this_file[[j]]
      } else if (tolower(blocks[j]) == "length_weight") {
        length_weight_list[[labels[j]]] <- this_file[[j]]
      } else if (tolower(blocks[j]) == "estimate") {
        estimate_blocks[[labels[j]]] <- this_file[[j]]
      } else if (tolower(blocks[j]) == "observation") {
        observation_blocks[[labels[j]]] <- this_file[[j]]
        observation_labels <- c(observation_labels, labels[j])
      } else if (tolower(blocks[j]) == "derived_quantity") {
        derived_quantity_blocks[[labels[j]]] <- this_file[[j]]
      }
    }
  }
  ## now summarise...
  ## categories
  category_df <- full_category_df <- NULL
  age_length_time_step_growth <- NULL
  for (i in 1:length(category_labels)) {
    if (!length_based_model) {

      ## get age-length label type
      this_age_length <- age_length_list[[category_age_lengths[i]]]
      ## get length-weight label type
      this_length_weight <- length_weight_list[[this_age_length$length_weight$value]]
      distribution <- "normal" # default
      if (!is.null(this_age_length$distribution)) {
        distribution <- this_age_length$distribution
      }
      this_cat_df <- data.frame("Category" = category_labels[i], "AgeLength" = category_age_lengths[i], "LengthWeight" = this_age_length$length_weight$value, "Distribution" = distribution)

      this_cat_full_df <- data.frame(
        "Category" = category_labels[i], "AgeLength" = paste0(category_age_lengths[i], " (", this_age_length$type$value, ")"),
        "LengthWeight" = paste0(this_age_length$length_weight$value, " (", this_length_weight$type$value, ")"), "Distribution" = distribution
      )

      category_df <- rbind(category_df, this_cat_df)
      if (is.null(this_age_length$time_step_proportions$value)) {
        this_age_length$time_step_proportions$value <- 0.0
      }

      full_category_df <- rbind(full_category_df, this_cat_full_df)
      if (is.null(age_length_time_step_growth)) {
        age_length_time_step_growth <- rbind(age_length_time_step_growth, data.frame(AgeLength = category_age_lengths[i], time_step_proportions = this_age_length$time_step_proportions$value))
      }
      if (!category_age_lengths[i] %in% age_length_time_step_growth$AgeLength) {
        age_length_time_step_growth <- rbind(age_length_time_step_growth, data.frame(AgeLength = category_age_lengths[i], time_step_proportions = this_age_length$time_step_proportions$value))
      }
    } else {
      ## get age-length label type
      this_growth <- growth_list[[category_growth_increments[i]]]
      ## get length-weight label type
      this_length_weight <- length_weight_list[[this_growth$length_weight$value]]
      distribution <- "normal" # default
      if (!is.null(this_growth$distribution)) {
        distribution <- this_growth$distribution
      }
      this_cat_df <- data.frame("Category" = category_labels[i], "GrowthIncrement" = category_growth_increments[i], "LengthWeight" = this_growth$length_weight$value, "Distribution" = distribution)

      this_cat_full_df <- data.frame(
        "Category" = category_labels[i], "GrowthIncrement" = paste0(category_growth_increments[i], " (", this_growth$type$value, ")"),
        "LengthWeight" = paste0(this_growth$length_weight$value, " (", this_length_weight$type$value, ")"), "Distribution" = distribution
      )

      category_df <- rbind(category_df, this_cat_df)
      full_category_df <- rbind(full_category_df, this_cat_full_df)
      if (is.null(age_length_time_step_growth)) {
        age_length_time_step_growth <- rbind(age_length_time_step_growth, data.frame(GrowthIncrement = category_growth_increments[i]))
      }
      if (!category_growth_increments[i] %in% age_length_time_step_growth$GrowthIncrement) {
        age_length_time_step_growth <- rbind(age_length_time_step_growth, data.frame(GrowthIncrement = category_growth_increments[i]))
      }
    }
  }
  ## Observations
  obs_year_df <- NULL
  if (!is.null(observation_labels)) {
    for (i in 1:length(observation_labels)) {
      this_obs <- observation_blocks[[observation_labels[i]]]
      years <- NULL
      for (y in 1:length(this_obs$years$value)) {
        years <- c(years, expand_shorthand_syntax(this_obs$years$value[y]))
      }
      active_ndx <- model_years %in% years
      obs_year_df <- rbind(obs_year_df, data.frame(year = model_years, observation = observation_labels[i], type = this_obs$type$value, active = ifelse(active_ndx, 1, NA)))
    }
  }

  ## these are converted to dfs for easy conversion to tables.
  ## Do this last so we can bring growth props and M props
  time_step_df <- NULL
  time_step_df_just_lab <- NULL
  for (i in 1:length(time_steps)) {
    ## get process type
    processes <- time_steps_list[[time_steps[i]]]
    process_type <- vector()
    df_entry <- vector()
    for (j in 1:length(processes)) {
      process_type[j] <- process_blocks[[processes[j]]]$type$value
      df_entry[j] <- paste0(processes[j], " (", process_type[j], ")")
    }
    # check when derived quantities are calculated if any
    for (j in 1:length(derived_quantity_blocks)) {
      if (time_steps[i] == derived_quantity_blocks[[j]]$time_step$value) {
        prop_mortality <- 0.5
        if (exists(x = "time_step_proportion", where = derived_quantity_blocks[[j]])) {
          prop_mortality <- as.numeric(derived_quantity_blocks[[j]]$time_step_proportion$value)
        }
        ## this dq in this time-step
        process_type <- c(process_type, "derived-quantity")
        df_entry <- c(df_entry, paste0(names(derived_quantity_blocks)[j], " (derived-quantity ", round(prop_mortality, 2), ")"))
      }
    }
    this_step <- data.frame(time_step = time_steps[i], processes = paste(df_entry, collapse = ", "))
    time_step_df <- rbind(time_step_df, this_step)
    time_step_df_just_lab <- rbind(time_step_df_just_lab, data.frame(time_step = time_steps[i], processes = paste(processes, collapse = ", ")))
  }
  if (!length_based_model) {
    age_length_labs <- unique(age_length_time_step_growth$AgeLength)
    for (i in 1:length(age_length_labs)) {
      this_growth <- age_length_time_step_growth[which(age_length_time_step_growth$AgeLength == age_length_labs[i]), ]
      time_step_df <- cbind(time_step_df, this_growth$time_step_proportions)
      time_step_df_just_lab <- cbind(time_step_df_just_lab, this_growth$time_step_proportions)
    }
    age_length_labs <- paste0(age_length_labs, " (assumed growth)")
    colnames(time_step_df_just_lab) <- c("Time-step", "Processes", age_length_labs)
    colnames(time_step_df) <- c("Time-step", "Processes (type)", age_length_labs)
  }

  ## Catch and M
  M_by_category <- NULL
  M_time_steps <- NULL
  catch_df <- NULL
  method_df <- NULL
  for (i in 1:length(process_blocks)) {
    this_process <- process_blocks[[i]]
    if (tolower(this_process$type$value) == "mortality_instantaneous") {
      m <- expand_shorthand_syntax(this_process$m$value)
      categories <- NULL
      for (j in 1:length(this_process$categories$value)) {
        categories <- c(categories, expand_category_shorthand(shorthand_categories = this_process$categories$value[j], reference_categories = category_labels, category_format = category_format))
      }
      selectivty <- NULL
      for (j in 1:length(this_process$relative_m_by_age$value)) {
        selectivty <- c(selectivty, expand_shorthand_syntax(this_process$relative_m_by_age$value[j]))
      }
      M_by_category <- rbind(data.frame(process = names(process_blocks)[i], category = categories, M = m, relative_M = selectivty))

      time_prop <- NULL
      if (is.null(this_process$time_step_proportions$value)) {
        time_prop <- rep(1, nrow(time_step_df))
      } else {
        time_prop <- this_process$time_step_proportions$value
      }

      M_time_steps <- rbind(M_time_steps, data.frame(process = names(process_blocks)[i], time_step_proportions = time_prop))

      ## sometimes people add catches as an !include. if that is the case
      ## we will ignore it: TODO add code to get all the includes go and get it
      if (!is.null(this_process$Table$catches)) {
        this_catch <- Reduce(cbind, this_process$Table$catches)
        class(this_catch) <- "numeric"
        colnames(this_catch) <- names(this_process$Table$catches)
        this_catch <- as.data.frame(this_catch)
        this_catch$process <- names(process_blocks)[i]
        molten_catch <- melt(this_catch, id.vars = c("year", "process"), value.name = "catch", variable.name = "fishery")
        catch_df <- rbind(catch_df, molten_catch)

        this_method <- Reduce(cbind, this_process$Table$method)
        colnames(this_method) <- names(this_process$Table$method)
        this_method <- as.data.frame(this_method, stringsAsFactors = F)
        for (k in 1:nrow(this_method)) {
          this_method$category[k] <- paste(expand_category_shorthand(this_method$category[k], category_labels, category_format = category_format), collapse = ",")
        }

        this_method$process <- names(process_blocks)[i]
        method_df <- rbind(method_df, this_method)
      }
    } else if (tolower(this_process$type$value) == "mortality_instantaneous_retained") {
      print("not yet implemented for mortality_instantaneous_retained")
    }
  }

  ## Summarise @estimate blocks
  ## priors type, bounds, starting values
  estimate_df <- NULL
  if (length(estimate_blocks) > 0) {
    for (i in 1:length(estimate_blocks)) {
      this_estimate <- estimate_blocks[[i]]
      label <- names(estimate_blocks)[i]
      parameter <- this_estimate$parameter$value
      type <- this_estimate$type$value
      lower_bound <- this_estimate$lower_bound$value
      upper_bound <- this_estimate$upper_bound$value
      same <- this_estimate$save$value
      if (!is.null(same)) {
        if (length(same) > 1) {
          same <- paste(same, collapse = ", ")
        }
      } else {
        same <- "-"
      }
      if (length(lower_bound) > 1) {
        lower_bound <- paste(lower_bound, collapse = " ")
      }
      if (length(upper_bound) > 1) {
        upper_bound <- paste(upper_bound, collapse = " ")
      }

      this_df <- data.frame(label = label, same = same, prior = type, lower_bound = lower_bound, upper_bound = upper_bound)
      estimate_df <- rbind(estimate_df, this_df)
    }
  }

  return(list(category_df = category_df, estimate_df = estimate_df, full_category_df = full_category_df, method_df = method_df, catch_df = catch_df, time_step_df = time_step_df, time_step_df_just_lab = time_step_df_just_lab, obs_year_df = obs_year_df, model_years = model_years, model_ages = ages, model_length_bins = model_length_bins, M_by_category = M_by_category, model_block = model_block[["model"]]))
}
