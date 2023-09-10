#' @title get_BH_recruitment
#' @description An accessor function that returns a data frame from a Casal2 model output of process type recruitment
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB, list> object that are generated from one of the extract.mpd() and extract.tabular() functions
#' @return A data frame from Casal2 model output
#' @rdname get_BH_recruitment
#' @export get_BH_recruitment
#' @importFrom reshape2 melt
"get_BH_recruitment" <- function(model, ...) {
  UseMethod("get_BH_recruitment", model)
}

#' @rdname get_BH_recruitment
#' @method get_BH_recruitment casal2MPD
#' @export
"get_BH_recruitment.casal2MPD" <- function(model, reformat_labels = TRUE) {
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
      if (this_report$type != "process") {
        next
      }
      if (this_report$type == "process" & this_report$sub_type != "recruitment_beverton_holt") {
        next
      }
      ## only a single trajectory
      full_df <- data.frame(
        model_year = this_report$model_year,
        spawn_event_year = this_report$spawn_event_year,
        ycs_years = this_report$spawn_event_year,
        standardised_recruitment_multipliers = this_report$standardised_recruitment_multipliers,
        recruitment_multipliers = this_report$recruitment_multipliers,
        recruits = this_report$recruits,
        true_ycs = this_report$true_ycs,
        r0 = this_report$r0,
        b0 = this_report$b0,
        recruit_event_SSB = this_report$recruit_event_SSB,
        recruit_event_SSB_percent = this_report$recruit_event_SSB_percent,
        ssb = this_report$ssb,
        par_set = 1,
        ssb_offset = this_report$ssb_offset,
        label = report_labels[i]
      )
      ## length based models don't have this subcommand
      if (exists(x = "age", where = this_report)) {
        full_df$age <- this_report$age
      } else {
        full_df$age <- NA
      }

      complete_df <- rbind(complete_df, full_df)
    } else {
      if (this_report[[1]]$type != "process") {
        next
      }
      if (this_report[[1]]$type == "process" & this_report[[1]]$sub_type != "recruitment_beverton_holt") {
        next
      }
      ## Multiple parameter inputs
      n_runs <- length(this_report)
      iter_labs <- names(this_report)
      for (dash_i in 1:n_runs) {
        ## only a single trajectory
        temp_df <- data.frame(
          ycs_years = this_report[[dash_i]]$spawn_event_year,
          model_year = this_report[[dash_i]]$model_year,
          spawn_event_year = this_report[[dash_i]]$spawn_event_year,
          standardised_recruitment_multipliers = this_report[[dash_i]]$standardised_recruitment_multipliers,
          recruitment_multipliers = this_report[[dash_i]]$recruitment_multipliers,
          recruits = this_report[[dash_i]]$recruits,
          true_ycs = this_report[[dash_i]]$true_ycs,
          r0 = this_report[[dash_i]]$r0,
          b0 = this_report[[dash_i]]$b0,
          recruit_event_SSB = this_report[[dash_i]]$recruit_event_SSB,
          recruit_event_SSB_percent = this_report[[dash_i]]$recruit_event_SSB_percent,
          ssb = this_report[[dash_i]]$ssb,
          par_set = iter_labs[dash_i],
          # age = this_report[[dash_i]]$age,
          ssb_offset = this_report[[dash_i]]$ssb_offset,
          label = report_labels[i]
        )
        ## length based models don't have this subcommand
        if (exists(x = "age", where = this_report[[dash_i]])) {
          temp_df$age <- this_report[[dash_i]]$age
        } else {
          full_df$age <- NA
        }
        complete_df <- rbind(complete_df, temp_df)
      }
    }
  }
  return(complete_df)
}

#' @rdname get_BH_recruitment
#' @method get_BH_recruitment list
#' @export
"get_BH_recruitment.list" <- function(model, reformat_labels = TRUE) {
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_dq <- get_BH_recruitment(model[[i]], reformat_labels = reformat_labels)
    this_dq$model_label <- run_labs[i]
    full_DF <- rbind(full_DF, this_dq)
  }
  return(full_DF)
}

#' @rdname get_BH_recruitment
#' @method get_BH_recruitment casal2TAB
#' @return A list frame from Casal2 model output. There is a non_multi_column_df which contains non year varying components such as B0, steepness etc and a multi_column_df which has ssb, recrutis, ycs etc
#' @export
"get_BH_recruitment.casal2TAB" <- function(model, reformat_labels = TRUE) {
  cat("This can take a few minutes for large models and big mcmc chains. Please be patient ... \n")
  if (reformat_labels) {
    report_labels <- reformat_default_labels(names(model))
  } else {
    report_labels <- names(model)
  }
  complete_df <- NULL
  recruit_multi_col_df <- recruit_df <- NULL
  for (i in 1:length(model)) {
    this_report <- model[[i]]
    if (this_report$type != "process") {
      next
    }
    if (this_report$sub_type != "recruitment_beverton_holt") {
      next
    }
    collabs <- colnames(this_report$values)
    first_component <- Reduce(c, lapply(strsplit(collabs, split = "[", fixed = T), FUN = function(x) {
      x[1]
    }))
    components <- unique(first_component)
    multi_column_df <- non_multi_column_df <- NULL
    non_multi_col_labs <- multi_col_labs <- "iteration"
    non_multi_column_df <- cbind(non_multi_column_df)
    multi_column_df <- cbind(multi_column_df)
    cols_saved <- vector()
    for (j in 1:length(components)) {
      col_ndx <- grepl(collabs, pattern = components[j])
      ## drop cols that have been saved. Problems with recruitment_multipliers and standardised_recruitment_multipliers
      if (any(which(col_ndx) %in% cols_saved)) {
        col_ndx[which(col_ndx) %in% cols_saved] <- FALSE
      }
      cols_saved <- c(cols_saved, which(col_ndx))

      if (sum(col_ndx, na.rm = T) == 1) {
        if (is.null(non_multi_column_df)) {
          non_multi_column_df <- cbind(as.numeric(rownames(this_report$values)))
        }
        non_multi_column_df <- cbind(non_multi_column_df, as.numeric(this_report$values[, col_ndx]))
        non_multi_col_labs <- c(non_multi_col_labs, components[j])
      } else {
        long_format <- suppressMessages({
          melt(as.matrix(this_report$values[, col_ndx]), variable.name = "colname", value.name = "values", factorsAsStrings = T)
        })
        colnames(long_format) <- c("iteration", "colname", "values")
        long_format$colname <- as.character(long_format$colname)
        second_component <- unlist(lapply(strsplit(long_format$colname, split = "[", fixed = T), FUN = function(x) {
          x[2]
        }))
        second_component <- substring(second_component, first = 1, last = nchar(second_component) - 1)
        long_format$years <- second_component
        ## drop colname
        if (is.null(multi_column_df)) {
          long_format <- long_format[, -which(colnames(long_format) %in% c("colname"))]
        } else {
          long_format <- long_format[, -which(colnames(long_format) %in% c("colname", "iteration"))]
        }
        multi_col_labs <- c(multi_col_labs, components[j], paste0(components[j], "_year"))
        temp_mat <- as.matrix(long_format)
        class(temp_mat) <- "as.numeric"
        multi_column_df <- cbind(multi_column_df, temp_mat)
      }
    }
    colnames(multi_column_df) <- multi_col_labs
    colnames(non_multi_column_df) <- non_multi_col_labs
    multi_column_df <- as.data.frame(multi_column_df)
    non_multi_column_df <- as.data.frame(non_multi_column_df)
    non_multi_column_df$label <- report_labels[i]
    multi_column_df$label <- report_labels[i]

    recruit_multi_col_df <- rbind(recruit_multi_col_df, multi_column_df)
    recruit_df <- rbind(recruit_df, non_multi_column_df)
  }
  return(list(multi_column_df = recruit_multi_col_df, non_multi_column_df = recruit_df))
}
