#' @rdname get_composition_mean_bin
#' @method get_composition_mean_bin list
#' @export

"get_composition_mean_bin.list" <- function(model) {
  run_labs <- names(model)
  full_DF <- NULL
  ## iterate over the models
  for (i in 1:length(model)) {
    if (class(model[[i]]) != "casal2MPD") {
      stop(paste0("This function only works on a named list with elements of class = 'casal2MPD'"))
    }
    this_abundance <- get_composition_mean_bin(model[[i]])
    if (!is.null(this_abundance)) {
      this_abundance$model_label <- run_labs[i]
      full_DF <- rbind(full_DF, this_abundance)
    }
  }
  return(full_DF)
  invisible()
}
