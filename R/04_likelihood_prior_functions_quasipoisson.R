source("R/quasi_pois_dispersion.R")

# calculate log likelihood

log_likelihood_sum_quasi_pois <- function(parameters, init.state, times, data, log = FALSE) {
  traj <- simulate(parameters, init.state, times)
  
  # print(head(traj))
  
  setDT(traj)
  setDT(data)
  
  # Define quasi-Poisson log-likelihood function matching R's implementation
  quasi_pois_loglik <- function(y, mu, phi) {
    # Handle cases where y = 0 separately
    dev <- ifelse(y > 0,
                  2 * (y * log(ifelse(y == 0, 1, y/mu)) - (y - mu)),
                  2 * mu)
    
    # Convert deviance to log-likelihood
    ll <- -dev/(2 * phi)
    
    return(ll)
  }
  
  
  log_likelihoods <- traj[data, on = "time"][
    , c(
      "log_likelihood_noro_obs_1",
      "log_likelihood_noro_obs_2",
      "log_likelihood_noro_obs_3",
      "log_likelihood_noro_obs_4",
      "log_likelihood_aki_hosp_4",
      "log_likelihood_gastro_hosp_4",
      "log_likelihood_gastro_gp_1",
      "log_likelihood_gastro_gp_2"
    ) :=
      {
        log_likelihood_noro_obs_1 <- quasi_pois_loglik(y = noro_obs_1, mu = noro_model_1, phi = dispersion_params$noro_obs_1)
        log_likelihood_noro_obs_2 <- quasi_pois_loglik(y = noro_obs_2, mu = noro_model_2, phi = dispersion_params$noro_obs_2)
        log_likelihood_noro_obs_3 <- quasi_pois_loglik(y = noro_obs_3, mu = noro_model_3, phi = dispersion_params$noro_obs_3)
        log_likelihood_noro_obs_4 <- quasi_pois_loglik(y = noro_obs_4, mu = noro_model_4, phi = dispersion_params$noro_obs_4)
        log_likelihood_aki_hosp_4 <- quasi_pois_loglik(y = aki_hosp_obs_4, mu = aki_hosp_model_4, phi = dispersion_params$aki_hosp_4)
        log_likelihood_gastro_hosp_4 <- quasi_pois_loglik(y = gastro_hosp_obs_4, mu = gastro_hosp_model_4, phi = dispersion_params$gastro_hosp_obs_4)
        log_likelihood_gastro_gp_1 <- quasi_pois_loglik(y = gastro_gp_obs_1, mu = gastro_gp_model_1, phi = dispersion_params$gastro_gp_1)
        log_likelihood_gastro_gp_2 <- quasi_pois_loglik(y = gastro_gp_obs_2, mu = gastro_gp_model_2, phi = dispersion_params$gastro_gp_2)
        .(
          log_likelihood_noro_obs_1,
          log_likelihood_noro_obs_2,
          log_likelihood_noro_obs_3,
          log_likelihood_noro_obs_4,
          log_likelihood_aki_hosp_4,
          log_likelihood_gastro_hosp_4,
          log_likelihood_gastro_gp_1,
          log_likelihood_gastro_gp_2
        )
      }
  ]
  
  # Calculate the sum of log likelihoods
  log_likelihood_noro_obs_1 <- sum(log_likelihoods$log_likelihood_noro_obs_1, na.rm = TRUE)
  log_likelihood_noro_obs_2 <- sum(log_likelihoods$log_likelihood_noro_obs_2, na.rm = TRUE)
  log_likelihood_noro_obs_3 <- sum(log_likelihoods$log_likelihood_noro_obs_3, na.rm = TRUE)
  log_likelihood_noro_obs_4 <- sum(log_likelihoods$log_likelihood_noro_obs_4, na.rm = TRUE)
  log_likelihood_aki_hosp_4 <- sum(log_likelihoods$log_likelihood_aki_hosp_4, na.rm = TRUE)
  log_likelihood_gastro_hosp_4 <- sum(log_likelihoods$log_likelihood_gastro_hosp_4, na.rm = TRUE)
  log_likelihood_gastro_gp_1 <- sum(log_likelihoods$log_likelihood_gastro_gp_1, na.rm = TRUE)
  log_likelihood_gastro_gp_2 <- sum(log_likelihoods$log_likelihood_gastro_gp_2, na.rm = TRUE)
  
  # Calculate the sum of the sums
  total_log_likelihood <- sum(
    log_likelihood_noro_obs_1,
    log_likelihood_noro_obs_2,
    log_likelihood_noro_obs_3,
    log_likelihood_noro_obs_4,
    log_likelihood_aki_hosp_4, 
    log_likelihood_gastro_hosp_4,
    log_likelihood_gastro_gp_1,
    log_likelihood_gastro_gp_2
  )
  
  likelihoods <- list(total_log_likelihood = total_log_likelihood,
                      log_likelihood_noro_obs_1 = log_likelihood_noro_obs_1,
                      log_likelihood_noro_obs_2 = log_likelihood_noro_obs_2,
                      log_likelihood_noro_obs_3 = log_likelihood_noro_obs_3,
                      log_likelihood_noro_obs_4 = log_likelihood_noro_obs_4,
                      log_likelihood_aki_hosp_4 = log_likelihood_aki_hosp_4,
                      log_likelihood_gastro_hosp_4 = log_likelihood_gastro_hosp_4, 
                      log_likelihood_gastro_gp_1 = log_likelihood_gastro_gp_1,
                      log_likelihood_gastro_gp_2 = log_likelihood_gastro_gp_2)
  
  return(ifelse(log, total_log_likelihood, exp(total_log_likelihood)))
  
  #   if (log) {
  #   return(likelihoods)
  # } else {
  #   return(exp(total_log_likelihood))
  # }
  
}

#########################
########## Posterior function
#########################

log_posterior_sum_quasi_pois <- function(parameters, init.state, times, data, age_data) {
  
  # calculate the model prior for parameter vector using
  # log_prior_sum, and assign to variable log.prior
  log.prior <- log_prior_sum(parameters, log = TRUE)
  
  # calculate the log-likelihood of parameters
  # and `init.state` with respect to the data using log_likelihood_sum
  # and assign to a variable `log.likelihood`    
  log.likelihood <- log_likelihood_sum_quasi_pois(parameters, init.state, times, data, log = TRUE)
  
  # calcualte log_likelihood_iid2, and assign to variable log.prior
  log.likelihood.age.incidence <- log_likelihood_iid2(parameters, init.state, times, age_data, log = TRUE)
  
  # calulate the log-posterior using the log-prior and log-likelihood
  # log.posterior <- log.prior + log.likelihood$total_log_likelihood + log.likelihood.age.incidence
  log.posterior <- log.prior + log.likelihood + log.likelihood.age.incidence
  
  return(log.posterior)
  # return(list(log.posterior = log.posterior, 
  #             log_likelihood_noro_obs = log.likelihood$log_likelihood_noro_obs,
  #             log_likelihood_aki_hosp_4 = log.likelihood$log_likelihood_aki_hosp_4,
  #             log_likelihood_gastro_hosp_4 = log.likelihood$log_likelihood_gastro_hosp_4,
  #             log_likelihood_gastro_gp_1 = log.likelihood$log_likelihood_gastro_gp_1,
  #             log_likelihood_gastro_gp_2 = log.likelihood$log_likelihood_gastro_gp_2,
  #             log.prior = log.prior, 
  #             log.likelihood.age.incidence = log.likelihood.age.incidence))
  
}

#########################
########## Posterior function for given parameters
#########################

# posterior density for a given value of a parameter

parameters_posteriror_function <- function(theta) {
  
  return(log_posterior_sum_quasi_pois(parameters = c(theta, par),
                                     init.state = init.state,
                                     times = times,
                                     data = observation_data2,
                                     age_data = age_incidence))
}
