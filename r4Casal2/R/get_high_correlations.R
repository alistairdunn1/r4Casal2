#' get_high_correlations
#' @description Get highly correlated parameters from a correlation matrix
#' @param correlation_matrix symmetric correlation matrix
#' @param max_correlation the max correlation value. must be a value between (0 - 1)
#' @param labels parameter labels for the correlation matrix
#' @param tol tolerance for symmetric check, sometimes rounding can cause an issue when checking if symmetric
#' @return data frame of row and column index and paramater label if labels supplied along with the correlation value
#' @rdname get_high_correlations
#' @export get_high_correlations
#'
get_high_correlations <- function(correlation_matrix, max_correlation = 0.8, labels = NULL, tol = .Machine$double.eps) {
  if (!any(class(correlation_matrix) == "matrix")) {
    stop("correlation_matrix is not a matrix")
  }
  if (max_correlation <= 0 | max_correlation > 1) {
    stop("silly value of max_correlation specified i.e. values not > 0 & < 1")
  }
  if (!isSymmetric(correlation_matrix, tol = tol)) {
    stop("correlation_matrix is not symmetric. Something is wrong. Inceasing the tolerance (tol) in the test for symmetry may help")
  }
  # set lower triangle and diagonal = 0
  lower_tri_ndx <- lower.tri(correlation_matrix, diag = T)
  correlation_matrix[lower_tri_ndx] <- 0.0
  all_arr_ind <- which(abs(correlation_matrix) > max_correlation, arr.ind = T)
  all_max <- which(abs(correlation_matrix) > max_correlation, arr.ind = F)
  if (length(all_max) > 0) {
    corr_vals <- correlation_matrix[all_max]
    corr_df <- data.frame(row_ndx = all_arr_ind[, 1], col_ndx = all_arr_ind[, 2], correlation = corr_vals)
    if (!is.null(labels)) {
      if (ncol(correlation_matrix) != length(labels)) {
        stop(paste0("labels have different dimensions to correlation_matrix. Please check these are consistent"))
      }
      corr_df$row_param <- labels[all_arr_ind[, 1]]
      corr_df$col_param <- labels[all_arr_ind[, 2]]
    }
    return(corr_df)
  } else {
    message("No correlations were greater than max_correlation")
    return(NULL)
  }
}
