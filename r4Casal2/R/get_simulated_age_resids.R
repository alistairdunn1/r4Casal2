#' @title get_simulated_age_resids
#' @description
#' A function for calculating simulation-based scaled (quantile) residuals from simulated observations from Casal2 and
#' DHARMa inbuilt functionality
#' @details this package takes simulated data read into R from r4Casal2::read.simulated.data() function along with MPD observation report estimates
#' and uses DHARMa (vignette("DHARMa", package="DHARMa")) to calculate simulation-based scaled (quantile) residuals
#' @author Craig Marsh
#' @param simulated_obs <list> output from read.simulated.data() function
#' @param observation_report <data.frame> values data frame from casal2 report type = observation
#' @return A list of objects.
#' \itemize{
#'   \item mpd_df data frame with observed expected and quantile residuals (assumed U(0,1)) and normal map residuals (assumed N(0,1))
#'   \item full_simulated_values data frame with all simulated observations for years and ages
#'   \item auto_corr_p_val test for significant autocorrelation along age-bins for each year see ?testTemporalAutocorrelation for more information
#'   \item dispersion_p_val tests for under and over dispersion for each year see ?testDispersion
#'   \item zero_inflat_p_val tests for zero inflation in each year see ?testZeroInflation
#' }
#' @importFrom reshape2 melt
#' @importFrom DHARMa createDHARMa testTemporalAutocorrelation testZeroInflation testDispersion
#' @rdname get_simulated_age_resids
#' @export get_simulated_age_resids
#' @examples
#' \donttest{
#'
#' }
get_simulated_age_resids <- function(simulated_obs, observation_report) {
  if (class(simulated_obs) != "list") {
    stop("class of simulated_obs is expected to be of class list")
  }
  if (is.null(observation_report$type)) {
    stop("observation_report not expected")
  }
  if (observation_report$type != "observation") {
    stop("observation_report type needs to be observation")
  }
  if (observation_report$likelihood != "multinomial") {
    stop("currently only configured for multinomial age-comp likelihood.")
  }
  mpd_vals <- observation_report$Values
  years <- names(simulated_obs)
  mpd_vals$quantile_resid <- mpd_vals$normalised_quantile_resid <- NA
  ages <- unique(mpd_vals$age)
  quantile_resid <- norm_quantile_resids <- matrix(NA, nrow = length(years), ncol = length(ages), dimnames = list(years, ages))
  auto_corr_p_val <- dispersion_p_val <- zero_inflat_p_val <- vector()
  full_sim_data <- NULL
  for (y in 1:length(simulated_obs)) {
    obs_ndx <- mpd_vals$year == years[y]
    obs <- mpd_vals$observed[obs_ndx] * mpd_vals$error_value[obs_ndx]
    # ages = mpd_vals$age[obs_ndx]

    fitted_val <- mpd_vals$expected[obs_ndx] * mpd_vals$error_value[obs_ndx]
    sim_year_obs <- simulated_obs[[years[y]]]
    rownames(sim_year_obs) <- ages
    this_sim_data <- melt(sim_year_obs)
    colnames(this_sim_data) <- c("age", "iteration", "simulated_value")
    this_sim_data$year <- years[y]
    full_sim_data <- rbind(full_sim_data, this_sim_data)
    DHARMaResAF <- createDHARMa(
      simulatedResponse = sim_year_obs, observedResponse = obs,
      fittedPredictedResponse = fitted_val, integerResponse = T
    )
    quantile_resid[y, ] <- DHARMaResAF$scaledResiduals
    norm_quantile_resids[y, ] <- qnorm(DHARMaResAF$scaledResiduals)
    dispersion_test <- testDispersion(DHARMaResAF, plot = F)
    zero_inflat_test <- testZeroInflation(DHARMaResAF, plot = F)
    age_correlation <- testTemporalAutocorrelation(simulationOutput = DHARMaResAF, time = ages, plot = F) ## HO: rho = 0, HA: rho != 0
    auto_corr_p_val[y] <- age_correlation$p.value
    dispersion_p_val[y] <- dispersion_test$p.value
    zero_inflat_p_val[y] <- zero_inflat_test$p.value
    mpd_vals$quantile_resid[obs_ndx] <- DHARMaResAF$scaledResiduals
    mpd_vals$normalised_quantile_resid[obs_ndx] <- qnorm(DHARMaResAF$scaledResiduals)
  }
  return(list(mpd_df = mpd_vals, full_simulated_values = full_sim_data, auto_corr_p_val = auto_corr_p_val, dispersion_p_val = dispersion_p_val, zero_inflat_p_val = zero_inflat_p_val))
}
