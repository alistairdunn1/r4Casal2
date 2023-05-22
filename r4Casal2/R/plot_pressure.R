#' @title plot_fishery plot fishing pressure if there has been an exploitation process reported.
#'
#' @description
#' A plotting function to plot a range of fishery attributes (only instantenous_mortality) for 'casal2TAB' and 'casal2MPD' objects.
#'
#' @author Craig Marsh
#' @param model <casal2MPD, casal2TAB> object that are generated from one of the extract.mpd() and extract.tabular() functions using the Casal2 base library
#' @param fisheryLabels if you only want to plot a subset of the fisheries supply a vector of characters which corresponds to the fisheries you want to plot.
#' @param quantity
#'  \itemize{
#'   \item fishing_pressure
#'   \item exploitation
#'   \item catch
#'   \item actual_catch
#' }
#' @return generate a plot over time if plot.it = T, if plot.it = F it will return a matrix of values.
#' @rdname plot_fishery
#' @export plot_fishery
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_line aes theme facet_wrap aes_string
#' @details
#' If you have multiple time-steps and fisheries happening at different time-steps it may be useful to use the fisheryLabels command to split out the plots.

"plot_fishery" <-
  function(model, fisheryLabels = NULL, quantity = "fishing_pressure") {
    if (!quantity %in% c("fishing_pressure", "exploitation", "catch", "actual_catch")) {
      stop("quantity, has incorrect values please check ?plot_recruitment")
    }
    UseMethod("plot_fishery", model)
  }

#' @return \code{NULL}
#'
#' @rdname plot_fishery
#' @method plot_fishery casal2MPD
#' @export
"plot_fishery.casal2MPD" <- function(model, fisheryLabels = NULL, quantity = "fishing_pressure") {
  fishery_df <- get_fisheries(model)
  ggplot(fishery_df, aes_string(x = "year", y = quantity, col = "fishery")) +
    geom_line(size = 2) +
    labs(colour = "Fishery", linetype = "Fishery", x = "Years", y = quantity) +
    facet_wrap(~fishery)
}

## method for class casal2TAB
#' @return \code{NULL}
#'
#' @rdname plot_fishery
#' @method plot_fishery casal2TAB
#' @export
"plot_fishery.casal2TAB" <- function(model, fisheryLabels = NULL, quantity = "fishing_pressure") {
  if (F) {
    ## check report label exists
    if (!report_label %in% names(model)) {
      stop(paste0("The report label '", report_label, "' was not found. The report labels available are: ", paste(names(model), collapse = ", ")))
    }
    ## get the report out
    this_report <- get(report_label, model)
    ## check that the report label is of type derived_quantity
    if (this_report$type != "process") {
      stop(paste0("The report label '", report_label, "' is not a derived quantity. Please check that the correct report_label was specified."))
    }
    if (!(this_report$process_type %in% c("mortality_instantaneous", "mortality_instantaneous_retained")) || is.null(this_report$process_type)) {
      stop(paste0("The process type in report '", report_label, "' is not 'mortality_instantaneous' or 'mortality_instantaneous_retained'. Please check that the correct report_label was specified."))
    }

    if (plot.it) {
      Labs <- colnames(this_report$values)
      start_index <- as.numeric(regexpr(pattern = "\\[", text = Labs)) + 1
      stop_index <- as.numeric(regexpr(pattern = "\\]", text = Labs)) - 1
      Fisheries <- unique(substring(Labs, start_index, last = stop_index))

      par(mfrow = c(1, length(Fisheries)))
      for (i in 1:length(Fisheries)) {
        ## pull out label and years
        ndx <- grepl(pattern = paste0("fishing_pressure\\[", Fisheries[i]), x = Labs)
        this_ssb <- this_report$values[, ndx]
        start_nd <- as.numeric(regexpr(pattern = "\\]", text = colnames(this_ssb))) + 2
        years <- as.numeric(substring(colnames(this_ssb), first = start_nd, last = nchar(colnames(this_ssb)) - 1))
        vals <- apply(this_ssb, 2, quantile, c(0.025, 0.5, 0.975))
        plot(years, vals["50%", ], ylim = c(0, max(vals)), ylab = "Fishing Pressure (exploitation rate)", xlab = "years", type = "l", main = Fisheries[i])
        polygon(x = c(years, rev(years)), y = c(vals["2.5%", ], rev(vals["97.5%", ])), col = "gray60")
        lines(years, vals["50%", ], col = "red", lwd = 2)
      }
    } else {
      return(this_report$values)
    }
    invisible()
  }
}

#'
#' @rdname plot_fishery
#' @method plot_fishery list
#' @export
"plot_fishery.list" <- function(model, fisheryLabels = NULL, quantity = "fishing_pressure") {
  fishery_df <- get_fisheries(model)
  ggplot(fishery_df, aes_string(x = "year", y = quantity, col = "model_label", linetype = "model_label")) +
    geom_line(size = 1.5) +
    labs(colour = "Model", linetype = "Model", x = "Years", y = quantity) +
    facet_wrap(~fishery)
}
