#' @title runs test
#' @description Run Wald-Wolfowitz Runs Test and calculate upper and lower control limits based on individual/moving-range chart see https://en.wikipedia.org/wiki/Shewhart_individuals_control_chart
#' @param x a time-series of residuals
#' @param test c("less","greater","two.sided"). Default less is checking for postive autocorrelation only
#' @return runs p value and upper and lower control limits
#' @importFrom randtests runs.test
#' @details for mean age or length residuals we suggest you use the residuals from the from the logged values.
#' H0: The order of the data is random
#' HA|two.sided: The order of the data is not random
#' HA|right.sided: a trend
#' HA|greater: a first order negative serial correlation
#' @rdname runs_test_residuals
#' @export runs_test_residuals
#'
runs_test_residuals <- function(x, test = "two.sided") {
  if (!test %in% c("left.sided", "right.sided", "two.sided")) {
    stop('test must be one of c("left.sided","right.sided","two.sided")')
  }

  # Average moving range
  mu <- 0
  mr <- abs(diff(x - mu))
  amr <- mean(mr, na.rm = TRUE)
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  # Calculate control limits
  lcl <- mu - 2.66 * stdev
  ucl <- mu + 2.66 * stdev
  if (nlevels(factor(sign(x))) > 1) {
    # Make the runs test non-parametric
    runstest <- runs.test(x, threshold = 0, alternative = test, plot = F)
    if (is.na(runstest$p.value)) p.value <- 0.001
    pvalue <- round(runstest$p.value, 3)
  } else {
    pvalue <- 0.001
  }
  return(list(siglim = c(lcl, ucl), p.runs = pvalue))
}
