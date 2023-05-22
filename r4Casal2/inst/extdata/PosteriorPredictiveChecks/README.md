## About
This folder serves as an example configuration model for demonstrating Posterior Predictive Checks (PPC) for model diagnostics.

The subfolder 'OM' has the model which generated simulated data with a time-varying survey selectivity (See population.csl2 @time_varying block).
This create the observations that are in Observation.csl2 in current directory. estimate.log is estimated with simulated data from the OM but with a constant
selectivity through time. This model is then assessed using PPC. To do this Casal2 is used to simulate data into the directory 'simulated_observations' which 
has parameter uncertainity expressed as sim_pars ~ MVN(MPD, Covar). Compared with 'simulated_observations_no_param_var' which only has observation error.

