# sensitivity analysis

iter = 50000

knots_number = 3

set.seed(456)

burn_value <- 10000

thin_factor <- 10

# limit AKI defintiion to primary and secondary codes within 0-2 days

parameters_posteriror_function <- function(theta) {
  
  return(log_posterior_sum_neg_binom(parameters = c(theta, par),
                                     init.state = init.state,
                                     times = times,
                                     data = observation_data_sensitivity_primary_secondary_aki,
                                     age_data = age_incidence))
}



mcmc_trace_sen_aki_primary_secondary <-
  mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 3,
    verbose = FALSE
  )

my_trace_sen_aki_primary_secondary <- mcmc(mcmc_trace_sen_aki_primary_secondary$trace)
xyplot(x = my_trace_sen_aki_primary_secondary)
tail(my_trace_sen_aki_primary_secondary)

saveRDS(my_trace_sen_aki_primary_secondary, "my_trace_sen_aki_primary_secondary.rds")

my_trace <- my_trace_sen_aki_primary_secondary

# extended when AKI is definied (greater)

parameters_posteriror_function <- function(theta) {
  
  return(log_posterior_sum_neg_binom(parameters = c(theta, par),
                                     init.state = init.state,
                                     times = times,
                                     data = observation_data_sensitivity_any_time,
                                     age_data = age_incidence))
}

mcmc_trace_sen_aki_any <-
  mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 3,
    verbose = FALSE
  )

my_trace_sen_aki_any <- mcmc(mcmc_trace_sen_aki_any$trace)
xyplot(x = my_trace_sen_aki_any)

saveRDS(mcmc_trace_sen_aki_any, "my_trace_sen_aki_any.rds")

my_trace <- my_trace_sen_aki_any

# reduce B-spline to 3 knots

source("R/04_likelihood_prior_functions_negbin.R")

knots_number = 1

mcmc_trace_knots1 <-
  mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 3,
    verbose = FALSE
  )

my_trace_knots1 <- mcmc(mcmc_trace_knots1$trace)
xyplot(x = my_trace_knots1)

saveRDS(mcmc_trace_knots1, "mcmc_trace_knots1.rds")

my_trace <- my_trace_knots1


####

knots_number = 2

mcmc_trace_knots2 <-
  mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 3,
    verbose = FALSE
  )

my_trace_knots2 <- mcmc(mcmc_trace_knots2$trace)
xyplot(x = my_trace_knots2)

saveRDS(mcmc_trace_knots2, "mcmc_trace_knots2.rds")
