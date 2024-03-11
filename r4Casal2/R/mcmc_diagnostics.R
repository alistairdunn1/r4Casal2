# This file is part of RStan
# Copyright (C) 2012, 2013, 2014, 2015, 2016, 2017, 2018 Trustees of Columbia University
# Copyright (C) 2018, 2019 Aki Vehtari, Paul Bürkner
#
# RStan is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# RStan is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

fft_next_good_size <- function(N) {
  # Find the optimal next size for the FFT so that
  # a minimum number of zeros are padded.
  if (N <= 2) {
    return(2)
  }
  while (TRUE) {
    m <- N
    while ((m %% 2) == 0) m <- m / 2
    while ((m %% 3) == 0) m <- m / 3
    while ((m %% 5) == 0) m <- m / 5
    if (m <= 1) {
      return(N)
    }
    N <- N + 1
  }
}

#' Convergence and efficiency diagnostics for Markov Chains These functions are
#' improved versions of the traditional Rhat (for convergence) and Effective Sample Size (for efficiency).
#' Credit for this code https://github.com/stan-dev/rstan/blob/develop/rstan/rstan/R/monitor.R
#' Protected under GPLv3
#' must remain free and available


#' Rank normalization
#'
#' Compute rank normalization for a numeric array. First replace each
#' value by its rank. Average rank for ties are used to conserve the
#' number of unique values of discrete quantities. Second, normalize
#' ranks via the inverse normal transformation.
#'
#' @param x A numeric array of values.
#'
#' @return A numeric array of rank normalized values with the same
#'     size as input.
#'
z_scale <- function(x) {
  n <- length(x)
  r <- rank(x, ties.method = "average")
  z <- qnorm((r - 1 / 2) / n)
  z[is.na(x)] <- NA
  if (!is.null(dim(x))) {
    z <- array(z, dim = dim(x), dimnames = dimnames(x))
  }
  return(z)
}

split_chains <- function(sims) {
  # split Markov chains
  # Args:
  #   sims: a 2D array of samples (# iter * # chains)
  if (is.vector(sims)) {
    dim(sims) <- c(length(sims), 1)
  }
  niter <- dim(sims)[1]
  if (niter == 1L) {
    return(sims)
  }
  half <- niter / 2
  cbind(sims[1:floor(half), ], sims[ceiling(half + 1):niter, ])
}

is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}
#' Autocovariance estimates
#'
#' Compute autocovariance estimates for every lag for the specified
#' input sequence using a fast Fourier transform approach. Estimate
#' for lag t is scaled by N-t.
#'
#' @param y A numeric vector forming a sequence of values.
#'
#' @return A numeric vector of autocovariances at every lag (scaled by N-lag).
autocovariance <- function(y) {
  N <- length(y)
  M <- fft_next_good_size(N)
  Mt2 <- 2 * M
  yc <- y - mean(y)
  yc <- c(yc, rep.int(0, Mt2 - N))
  transform <- fft(yc)
  ac <- fft(Conj(transform) * transform, inverse = TRUE)
  # use "biased" estimate as recommended by Geyer (1992)
  ac <- Re(ac)[1:N] / (N^2 * 2)
  ac
}

#' Autocorrelation estimates
#'
#' Compute autocorrelation estimates for every lag for the specified
#' input sequence using a fast Fourier transform approach. Estimate
#' for lag t is scaled by N-t.
#'
#' @param y A numeric vector forming a sequence of values.
#'
#' @return A numeric vector of autocorrelations at every lag (scaled by N-lag).
autocorrelation <- function(y) {
  ac <- autocovariance(y)
  ac <- ac / ac[1]
}


#' Effective sample size
#'
#' Compute the effective sample size estimate for a sample of several chains
#' for one parameter. For split-ESS, call this with split chains.
#'
#' @param sims A 2D array _without_ warmup samples (# iter * # chains).
#'
#' @return A single numeric value for the effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
ess_rfun <- function(sims) {
  if (is.vector(sims)) {
    dim(sims) <- c(length(sims), 1)
  }
  chains <- ncol(sims)
  n_samples <- nrow(sims)
  if (n_samples < 3L || should_return_NA(sims)) {
    return(NA)
  }
  acov <- lapply(seq_len(chains), function(i) autocovariance(sims[, i]))
  acov <- do.call(cbind, acov)
  chain_mean <- apply(sims, 2, mean)
  mean_var <- mean(acov[1, ]) * n_samples / (n_samples - 1)
  var_plus <- mean_var * (n_samples - 1) / n_samples
  if (chains > 1) {
    var_plus <- var_plus + var(chain_mean)
  }

  # Geyer's initial positive sequence
  rho_hat_t <- rep.int(0, n_samples)
  t <- 0
  rho_hat_even <- 1
  rho_hat_t[t + 1] <- rho_hat_even
  rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
  rho_hat_t[t + 2] <- rho_hat_odd
  while (t < nrow(acov) - 5 && !is.nan(rho_hat_even + rho_hat_odd) &&
    (rho_hat_even + rho_hat_odd > 0)) {
    t <- t + 2
    rho_hat_even <- 1 - (mean_var - mean(acov[t + 1, ])) / var_plus
    rho_hat_odd <- 1 - (mean_var - mean(acov[t + 2, ])) / var_plus
    if ((rho_hat_even + rho_hat_odd) >= 0) {
      rho_hat_t[t + 1] <- rho_hat_even
      rho_hat_t[t + 2] <- rho_hat_odd
    }
  }
  max_t <- t
  # this is used in the improved estimate
  if (rho_hat_even > 0) {
    rho_hat_t[max_t + 1] <- rho_hat_even
  }

  # Geyer's initial monotone sequence
  t <- 0
  while (t <= max_t - 4) {
    t <- t + 2
    if (rho_hat_t[t + 1] + rho_hat_t[t + 2] >
      rho_hat_t[t - 1] + rho_hat_t[t]) {
      rho_hat_t[t + 1] <- (rho_hat_t[t - 1] + rho_hat_t[t]) / 2
      rho_hat_t[t + 2] <- rho_hat_t[t + 1]
    }
  }
  ess <- chains * n_samples
  # Geyer's truncated estimate
  # tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t])
  # Improved estimate reduces variance in antithetic case
  tau_hat <- -1 + 2 * sum(rho_hat_t[1:max_t]) + rho_hat_t[max_t + 1]
  # Safety check for negative values and with max ess equal to ess*log10(ess)
  tau_hat <- max(tau_hat, 1 / log10(ess))
  ess <- ess / tau_hat
  ess
}
#' Traditional Rhat convergence diagnostic
#'
#' Compute the Rhat convergence diagnostic for a single parameter
#' For split-Rhat, call this with split chains.
#'
#' @param sims A 2D array _without_ warmup samples (# iter * # chains).
#'
#' @return A single numeric value for Rhat.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
rhat_rfun <- function(sims) {
  if (anyNA(sims)) {
    return(NA)
  }
  if (any(!is.finite(sims))) {
    return(NaN)
  }
  if (is_constant(sims)) {
    return(NA)
  }
  if (is.vector(sims)) {
    dim(sims) <- c(length(sims), 1)
  }
  chains <- ncol(sims)
  n_samples <- nrow(sims)
  chain_mean <- numeric(chains)
  chain_var <- numeric(chains)
  for (i in seq_len(chains)) {
    chain_mean[i] <- mean(sims[, i])
    chain_var[i] <- var(sims[, i])
  }
  var_between <- n_samples * var(chain_mean)
  var_within <- mean(chain_var)
  sqrt((var_between / var_within + n_samples - 1) / n_samples)
}

#' Rhat convergence diagnostic
#'
#' Compute Rhat convergence diagnostic as the maximum of rank normalized
#' split-Rhat and rank normalized folded-split-Rhat for one parameter.
#'
#' @param sims A 2D array _without_ warmup samples (# iter * # chains).
#'
#' @return A single numeric value for the effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
Rhat <- function(sims) {
  bulk_rhat <- rhat_rfun(z_scale(split_chains(sims)))
  sims_folded <- abs(sims - median(sims))
  tail_rhat <- rhat_rfun(z_scale(split_chains(sims_folded)))
  max(bulk_rhat, tail_rhat)
}
#' Bulk effective sample size (bulk-ESS)
#'
#' Compute bulk effective sample size estimate (bulk-ESS) for one parameter.
#' Bulk-ESS is useful as a generic diagnostic for the sampling
#' efficiency in the bulk of the posterior. It is defined as the
#' effective sample size for rank normalized values using split chains.
#'
#' @param sims A 2D array _without_ warmup samples (# iter * # chains).
#'
#' @return A single numeric value for the bulk effective sample size.
#'
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
#'
ess_bulk <- function(sims) {
  ess_rfun(z_scale(split_chains(sims)))
}

#' Tail effective sample size (tail-ESS)
#'
#' Compute tail effective sample size estimate (tail-ESS) for one parameter.
#' Tail-ESS is useful for generic diagnostic for the sampling
#' efficiency in the tails of the posterior. It is defined as
#' the minimum of the effective sample sizes for 5% and 95% quantiles.
#'
#' @param sims A 2D array _without_ warmup samples (# iter * # chains).
#' @return A single numeric value for the tail effective sample size.
#' @references
#' Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
#' Paul-Christian Bürkner (2019). Rank-normalization, folding, and
#' localization: An improved R-hat for assessing convergence of
#' MCMC. \emph{arXiv preprint} \code{arXiv:1903.08008}.
#'
#' @export
#'
ess_tail <- function(sims) {
  I05 <- sims <= quantile(sims, 0.05, na.rm = TRUE)
  q05_ess <- ess_rfun(split_chains(I05))
  I95 <- sims <= quantile(sims, 0.95, na.rm = TRUE)
  q95_ess <- ess_rfun(split_chains(I95))
  min(q05_ess, q95_ess)
}
