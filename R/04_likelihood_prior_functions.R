
#########################
########## Prior function
#########################

log_prior_sum <- function(parameters, log = FALSE) {
  
  ## uniform prior on R0: U[1,3]
  log.prior.surveillance_report_1 <- dunif(parameters[["surveillance_report_1"]], min = 0, max = 0.1, log = TRUE)

  ## uniform prior on R0: U[1,3]
  log.prior.surveillance_report_2 <- dunif(parameters[["surveillance_report_2"]], min = 0, max = 0.1, log = TRUE)

  ## uniform prior on R0: U[1,3]
  log.prior.surveillance_report_3 <- dunif(parameters[["surveillance_report_3"]], min = 0, max = 0.1, log = TRUE)

  ## uniform prior on R0: U[1,3]
  log.prior.surveillance_report_4 <- dunif(parameters[["surveillance_report_4"]], min = 0, max = 0.1, log = TRUE)

  ## uniform prior on proportion infections symptomatic: U[0,5]
  log.prior.sigma <- dnorm(exp(parameters[["sigma"]]), mean = 0.715, sd = 0.094, log = TRUE)
  
  ## uniform prior on infectiousness during asymptomatic period: U[0,5]
  # log.prior.rho <- dunif(parameters[["rho"]], min = 0, max = 0.1, log = TRUE)
  
  ## uniform prior on infectiousness during asymptomatic period: U[0,5]
  log.prior.w1<- dunif((parameters[["season_amp"]] / 100), min = 0, max = 0.5, log = TRUE)
  
  ## uniform prior on infectiousness during asymptomatic period: U[0,5]
  log.prior.w2<- dunif((parameters[["season_offset"]] / 100), min = 0, max = 0.5, log = TRUE)
  
  ## uniform prior on multiplication factor for over 65 seasonal amplitude period: U[0,5]
  log.prior.w3<- dunif(parameters[["season_amp_over65"]], min = 0, max = 10, log = TRUE)
  
  ## uniform prior on immunity waning period: U[0,5]
  log.prior.delta <- dunif(parameters[["D_immun"]], min = 3, max = 12, log = TRUE)
  # log.prior.delta <- dunif(parameters[["D_immun"]], min = 0, max = 20, log = TRUE)
  
  ## uniform prior on probability transmission under 5: U[0,5]
  log.prior.q1 <- dnorm(exp(parameters[["probT_under5"]]), mean = 0.21, sd = 0.008, log = TRUE)
  
  ## uniform prior on probability transmission under 5: U[0,5]
  log.prior.q2 <- dnorm(exp(parameters[["probT_over5"]]), mean = 0.0356, sd = 0.00023, log = TRUE)

  # ## uniform prior on aki hospitalisation: U[0,1]
  # log.prior.aki_hospitalisation1 <- dunif(log(parameters[["aki_hospitalisation_1"]]), min = log(0), max = log(0.1), log = TRUE)
  # 
  # ## uniform prior on aki hospitalisation: U[0,1]
  # log.prior.aki_hospitalisation2 <- dunif(log(parameters[["aki_hospitalisation_2"]]), min = log(0), max = (0.1), log = TRUE)
  # 
  # ## uniform prior on aki hospitalisation: U[0,1]
  # log.prior.aki_hospitalisation3 <- dunif(log(parameters[["aki_hospitalisation_3"]]), min = log(0), max = (0.3), log = TRUE)
  
  ## uniform prior on aki hospitalisation: U[0,1]
  log.prior.aki_hospitalisation4 <- dunif(exp(parameters[["aki_hospitalisation_4"]]), min = 0, max = 0.5, log = TRUE)

  ## uniform prior on gastro hospitalisation: U[0,1]
  # log.prior.gastro_hospitalisation1 <- dunif(exp(parameters[["gastro_hospitalisation_1"]]), min = 0, max = 1, log = TRUE)

  ## uniform prior on gastro hospitalisation: U[0,1]
  # log.prior.gastro_hospitalisation2 <- dunif(exp(parameters[["gastro_hospitalisation_2"]]), min = 0, max = 1, log = TRUE)

  ## uniform prior on gastro hospitalisation: U[0,1]
  # log.prior.gastro_hospitalisation3 <- dunif(log(parameters[["gastro_hospitalisation_3"]]), min = log(0.001), max = log(1), log = TRUE)

  ## uniform prior on gastro hospitalisation: U[0,1]
  log.prior.gastro_hospitalisation4 <- dunif(exp(parameters[["gastro_hospitalisation_4"]]), min = 0, max = 0.3, log = TRUE)

  ## uniform prior on gastro hospitalisation: U[0,1] was 0.9
  log.prior.gastro_gp_attend_1 <- dunif(exp(parameters[["gastro_gp_attend_1"]]), min = 0, max = 0.4, log = TRUE)
  # log.prior.gastro_gp_attend_1 <- dnorm(exp(parameters[["gastro_gp_attend_1"]]), mean = 0.05, sd = 0.032, log = TRUE)
  
  ## uniform prior on gastro hospitalisation: U[0,1] was 0.9
  log.prior.gastro_gp_attend_2 <- dunif(exp(parameters[["gastro_gp_attend_2"]]), min = 0, max = 0.4, log = TRUE)
  # log.prior.gastro_gp_attend_2 <- dnorm(exp(parameters[["gastro_gp_attend_2"]]), mean = 0.05, sd = 0.032, log = TRUE)

  log.sum <- log.prior.w1
  + log.prior.sigma
  + log.prior.surveillance_report_1
  + log.prior.surveillance_report_2
  + log.prior.surveillance_report_3
  + log.prior.surveillance_report_4
  + log.prior.w2
  + log.prior.w3
  + log.prior.delta 
  + log.prior.q1
  + log.prior.q2
  + log.prior.aki_hospitalisation4 
  + log.prior.gastro_hospitalisation4
  + log.prior.gastro_gp_attend_1
  + log.prior.gastro_gp_attend_2

  return(ifelse(log, log.sum, exp(log.sum)))
}

#########################
########## Log likelihood function
#########################

# calculate log likelihood

log_likelihood_sum <- function(parameters, init.state, times, data, log = FALSE) {
  traj <- simulate(parameters, init.state, times)
  
  # print(head(traj))
  
  setDT(traj)
  setDT(data)
  
  log_likelihoods <- traj[data, on = "time"][
    , c(
      "log_likelihood_noro_obs_1",
        "log_likelihood_noro_obs_2",
        "log_likelihood_noro_obs_3",
        "log_likelihood_noro_obs_4",
        # "log_likelihood_aki_hosp_1", 
        # "log_likelihood_aki_hosp_2",
        # "log_likelihood_aki_hosp_3", 
        "log_likelihood_aki_hosp_4",
        # "log_likelihood_gastro_hosp_1",
        # "log_likelihood_gastro_hosp_2",
        # "log_likelihood_gastro_hosp_3",
        "log_likelihood_gastro_hosp_4",
        "log_likelihood_gastro_gp_1",
        "log_likelihood_gastro_gp_2"
        ) :=
      {
        log_likelihood_noro_obs_1 <- dpois(x = noro_obs_1, lambda = noro_model_1, log = TRUE)
        log_likelihood_noro_obs_2 <- dpois(x = noro_obs_2, lambda = noro_model_2, log = TRUE)
        log_likelihood_noro_obs_3 <- dpois(x = noro_obs_3, lambda = noro_model_3, log = TRUE)
        log_likelihood_noro_obs_4 <- dpois(x = noro_obs_4, lambda = noro_model_4, log = TRUE)
        # log_likelihood_aki_hosp_1 <- dpois(x = aki_hosp_obs_1, lambda = aki_hosp_model_1, log = TRUE)
        # log_likelihood_aki_hosp_2 <- dpois(x = aki_hosp_obs_2, lambda = aki_hosp_model_2, log = TRUE)
        # log_likelihood_aki_hosp_3 <- dpois(x = aki_hosp_obs_3, lambda = aki_hosp_model_3, log = TRUE)
        log_likelihood_aki_hosp_4 <- dpois(x = aki_hosp_obs_4, lambda = aki_hosp_model_4, log = TRUE)
        # log_likelihood_gastro_hosp_1 <- dpois(x = gastro_hosp_obs_1, lambda = gastro_hosp_model_1, log = TRUE)
        # log_likelihood_gastro_hosp_2 <- dpois(x = gastro_hosp_obs_2, lambda = gastro_hosp_model_2, log = TRUE)
        # log_likelihood_gastro_hosp_3 <- dpois(x = gastro_hosp_obs_3, lambda = gastro_hosp_model_3, log = TRUE)
        log_likelihood_gastro_hosp_4 <- dpois(x = gastro_hosp_obs_4, lambda = gastro_hosp_model_4, log = TRUE)
        log_likelihood_gastro_gp_1 <- dpois(x = gastro_gp_obs_1, lambda = gastro_gp_model_1, log = TRUE)
        log_likelihood_gastro_gp_2 <- dpois(x = gastro_gp_obs_2, lambda = gastro_gp_model_2, log = TRUE)
        .(
          log_likelihood_noro_obs_1,
          log_likelihood_noro_obs_2,
          log_likelihood_noro_obs_3,
          log_likelihood_noro_obs_4,
          # log_likelihood_aki_hosp_1,
          # log_likelihood_aki_hosp_2,
          # log_likelihood_aki_hosp_3, 
          log_likelihood_aki_hosp_4,
          # log_likelihood_gastro_hosp_1,
          # log_likelihood_gastro_hosp_2,
          # log_likelihood_gastro_hosp_3,
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
  # log_likelihood_aki_hosp_1 <- sum(log_likelihoods$log_likelihood_aki_hosp_1)
  # log_likelihood_aki_hosp_2 <- sum(log_likelihoods$log_likelihood_aki_hosp_2)
  # log_likelihood_aki_hosp_3 <- sum(log_likelihoods$log_likelihood_aki_hosp_3)
  log_likelihood_aki_hosp_4 <- sum(log_likelihoods$log_likelihood_aki_hosp_4, na.rm = TRUE)
  # log_likelihood_gastro_hosp_1 <- sum(log_likelihoods$log_likelihood_gastro_hosp_1, na.rm = TRUE)
  # log_likelihood_gastro_hosp_2 <- sum(log_likelihoods$log_likelihood_gastro_hosp_2, na.rm = TRUE)
  # log_likelihood_gastro_hosp_3 <- sum(log_likelihoods$log_likelihood_gastro_hosp_3, na.rm = TRUE)
  log_likelihood_gastro_hosp_4 <- sum(log_likelihoods$log_likelihood_gastro_hosp_4, na.rm = TRUE)
  log_likelihood_gastro_gp_1 <- sum(log_likelihoods$log_likelihood_gastro_gp_1, na.rm = TRUE)
  log_likelihood_gastro_gp_2 <- sum(log_likelihoods$log_likelihood_gastro_gp_2, na.rm = TRUE)

  # Calculate the sum of the sums
  total_log_likelihood <- sum(
    log_likelihood_noro_obs_1,
                              log_likelihood_noro_obs_2,
                              log_likelihood_noro_obs_3,
                              log_likelihood_noro_obs_4,
                              # log_likelihood_aki_hosp_1, 
                              # log_likelihood_aki_hosp_2, 
                              # log_likelihood_aki_hosp_3,
                              log_likelihood_aki_hosp_4, 
                              # log_likelihood_gastro_hosp_1,
                              # log_likelihood_gastro_hosp_2,
                              # log_likelihood_gastro_hosp_3,
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
########## Log likelihood IID2
#########################

# calculate log likelihood

log_likelihood_iid2 <- function(parameters, init.state, times, age_data, log = FALSE) {
  age_incidence_total <- simulate(parameters, init.state, times, age.incidence = TRUE)
  
  # print(head(traj))
  
  setDT(age_incidence_total)
  setDT(age_data)
  
  
  
  log_likelihood_age_incidence <- age_incidence_total[age_data, on = "age"][
    , c("log_likelihood_age_incidence")
    :=
      {
        log_likelihood_age_incidence <- dpois(x = harris_incidence, lambda = model_incidence , log = TRUE)
        .(log_likelihood_age_incidence)
      }
  ]
  
  # Calculate the sum of log likelihoods
  log_likelihood_age_incidence <- sum(log_likelihood_age_incidence$log_likelihood_age_incidence, na.rm = TRUE)
  
  # print(paste("iid2_ll :", log_likelihood_age_incidence))
  
   return(ifelse(log, log_likelihood_age_incidence, exp(log_likelihood_age_incidence)))
}



#########################
########## Posterior function
#########################

log_posterior_sum <- function(parameters, init.state, times, data, age_data) {
  
  # calculate the model prior for parameter vector using
  # log_prior_sum, and assign to variable log.prior
  log.prior <- log_prior_sum(parameters, log = TRUE)
  
  # calculate the log-likelihood of parameters
  # and `init.state` with respect to the data using log_likelihood_sum
  # and assign to a variable `log.likelihood`    
  log.likelihood <- log_likelihood_sum(parameters, init.state, times, data, log = TRUE)
  
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
  
  return(log_posterior_sum(parameters = c(theta, par),
                          init.state = init.state,
                          times = times,
                          data = observation_data2,
                          age_data = age_incidence))
}
