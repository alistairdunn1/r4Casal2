#' extract_reweighted_mpds
#' @description this function will read in a set of re-weighted models as list so you can plot and integrate the effect of data-weightings. This expects model outputs produced by the function run_automatic_reweighting
#' @param reweighting_path path of the directory that contains the estimated output
#' @details this function expects there to be a range of output files with the following filename structure "estimate_'weighting iteration'.log, where 'weighting iteration' is an integer
#' @return a names list of MPD that can be accessed with the get_ and plot_ methods.
#' @rdname extract_reweighted_mpds
#' @export extract_reweighted_mpds
extract_reweighted_mpds <- function(reweighting_path) {
  ### look at the effect of reweighting
  possible_files <- list.files(reweighting_path, pattern = ".log")
  estimate_files <- possible_files[grep(possible_files, pattern = "estimate_")]
  iteration <- substring(estimate_files, first = 10)
  iteration <- as.numeric(sapply(strsplit(iteration, split = "\\."), "[", 1))
  MPD_list <- list()
  for (i in 1:length(estimate_files)) {
    MPD_list[[paste0("Weighting-", iteration[i])]] <- extract.mpd(path = reweighting_path, file = estimate_files[i])
  }
  return(MPD_list)
}
