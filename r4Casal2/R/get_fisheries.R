#' @title get_fisheries
#' @description An accessor function that returns a data frame from a Casal2 model output of process type mortality_instantaneous
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @param reformat_labels <bool> Reformat default Casal2 report labels to remove leading and trailing underscores (default = TRUE)
#' @return A data frame from Casal2 model output
#' @rdname get_fisheries
#' @export get_fisheries
#' @importFrom reshape2 melt
#' @importFrom tidyr gather separate spread %>%
#'
"get_fisheries" <- function(model, ...) {
  UseMethod("get_fisheries", model)
}

#' @rdname get_fisheries
#' @method get_fisheries casal2MPD
#' @export
"get_fisheries.casal2MPD" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  # can be -r or -r -i
  multiple_iterations_in_a_report <- FALSE
  complete_df <- NULL
  for (i in 1:length(model)) {
    if (report_labels[i] == "header") {
      next
    }

    this_report <- model[[i]]

    if (any(names(this_report) == "type")) {
      if (tolower(this_report$type) != "process") {
        next
      }
      if (tolower(this_report$type) == "process" & tolower(this_report$sub_type) != "mortality_instantaneous") {
        next
      }
      ## only a single trajectory
      f_ndx <- grepl(pattern = "fishing_pressure\\[", names(this_report))
      exploitation_ndx <- grepl(pattern = "exploitation_rate", names(this_report))
      catch_ndx <- grepl(pattern = "catch", substring(names(this_report), first = 1, last = 5))
      actual_catch_ndx <- grepl(pattern = "actual_catch", names(this_report))

      start_index <- as.numeric(regexpr(pattern = "\\[", text = names(this_report)[f_ndx])) + 1
      stop_index <- as.numeric(regexpr(pattern = "\\]", text = names(this_report)[f_ndx])) - 1
      fisheries <- substring(names(this_report)[f_ndx], start_index, last = stop_index)
      years <- this_report$year
      full_df <- NULL
      for (f in 1:length(fisheries)) {
        temp_df <- data.frame(
          year = this_report$year,
          fishing_pressure = this_report[[which(f_ndx)[f]]],
          exploitation = this_report[[which(exploitation_ndx)[f]]],
          catch = this_report[[which(catch_ndx)[f]]],
          actual_catch = this_report[[which(actual_catch_ndx)[f]]],
          fishery = fisheries[f],
          par_set = 1
        )
        full_df <- rbind(full_df, temp_df)
      }
      full_df$label <- report_labels[i]
      complete_df <- rbind(complete_df, full_df)
    } else {
      if (this_report[[1]]$type != "process") {
        next
      }
      if (this_report[[1]]$type == "process" & tolower(this_report[[1]]$sub_type) != "mortality_instantaneous") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)

      for (dash_i in 1:n_runs) {
        ## only a single trajectory
        f_ndx <- grepl(pattern = "fishing_pressure\\[", names(this_report[[dash_i]]))
        exploitation_ndx <- grepl(pattern = "exploitation_rate", names(this_report[[dash_i]]))
        catch_ndx <- grepl(pattern = "catch", substring(names(this_report[[dash_i]]), first = 1, last = 5))
        actual_catch_ndx <- grepl(pattern = "actual_catch", names(this_report[[dash_i]]))
        start_index <- as.numeric(regexpr(pattern = "\\[", text = names(this_report[[dash_i]])[f_ndx])) + 1
        stop_index <- as.numeric(regexpr(pattern = "\\]", text = names(this_report[[dash_i]])[f_ndx])) - 1
        fisheries <- substring(names(this_report[[dash_i]])[f_ndx], start_index, last = stop_index)
        years <- this_report[[dash_i]]$year
        full_df <- NULL
        for (f in 1:length(fisheries)) {
          temp_df <- data.frame(
            year = years,
            fishing_pressure = this_report[[dash_i]][[which(f_ndx)[f]]],
            exploitation = this_report[[dash_i]][[which(exploitation_ndx)[f]]],
            catch = this_report[[dash_i]][[which(catch_ndx)[f]]],
            actual_catch = this_report[[dash_i]][[which(actual_catch_ndx)[f]]],
            fishery = fisheries[f],
            par_set = iter_labs[dash_i]
          )
          full_df <- rbind(full_df, temp_df)
        }
        full_df$label <- report_labels[i]
        complete_df <- rbind(complete_df, full_df)
      }
    }
  }
  return(complete_df)
}

#' @rdname get_fisheries
#' @method get_fisheries list
#' @export
"get_fisheries.list" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_fisheries(model[[i]], reformat_labels = reformat_labels)
    if (is.null(this_dq)) {
      next
    }
    this_dq$model_label <- report_labels[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
}

#' @rdname get_fisheries
#' @method get_fisheries casal2TAB
#' @param m_by_age if TRUE, return the m_by_age data else exclude m_by_age (the default and consistent with older versions of Casal2)
#'
#' @export
"get_fisheries.casal2TAB" <- function(model, m_by_age = FALSE, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  cat("this can take a few minutes for large models and big mcmc chains. Please be patient :~) \n")
  complete_df <- NULL
  for (i in 1:length(model)) {
    this_report <- model[[i]]
    if (this_report$type != "process") {
      next
    }
    if (this_report$type == "process" & this_report$sub_type != "mortality_instantaneous") {
      next
    }
    fish_vals <- this_report$values
    colabs <- colnames(fish_vals)
    first_component <- Reduce(c, lapply(strsplit(colabs, split = "[", fixed = T), FUN = function(x) {
      x[1]
    }))
    second_component <- Reduce(c, lapply(strsplit(colabs, split = "[", fixed = T), FUN = function(x) {
      x[2]
    }))
    third_component <- Reduce(c, lapply(strsplit(colabs, split = "[", fixed = T), FUN = function(x) {
      x[3]
    }))
    ##
    second_component <- substring(second_component, first = 1, last = nchar(second_component) - 1)
    third_component <- substring(third_component, first = 1, last = nchar(third_component) - 1)
    newcolab <- paste(second_component, third_component, first_component, sep = ".")
    colnames(fish_vals) <- newcolab
    cat("getting values for ", report_labels[i], "\n")

    fish_vals$iteration <- rownames(fish_vals)
    fishery_long_format <- fish_vals %>%
      gather("key", "value", -iteration) %>%
      separate(key, c("fishery", "year", "type"), sep = "\\.") %>%
      spread(type, value)
    fishery_long_format$year <- as.numeric(fishery_long_format$year)
    fishery_long_format$label <- report_labels[i]
    complete_df <- rbind(complete_df, fishery_long_format)
  }
  if (!m_by_age) {
    complete_df <- complete_df %>%
      filter(!is.na(catch)) %>%
      select(!starts_with("m_by_age"))
  } else {
    complete_df <- complete_df %>%
      filter(is.na(catch)) %>%
      rename(category = fishery, age = year) %>%
      select(!c(actual_catches, catch, exploitation_rate, fishing_pressure))
  }
  return(complete_df)
}
