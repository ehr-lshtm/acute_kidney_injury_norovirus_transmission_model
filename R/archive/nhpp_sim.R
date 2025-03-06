# First create a list to store all results
simulation_results <- list()

# Assuming your posterior is stored in a matrix/dataframe 
posterior_samples <- traceBurnThin

# Set up dimensions
n_parameter_sets <- 20
n_sims_per_param <- 5
n_days <- 365  # However many days you're simulating

# Sample and simulate
sampled_indices <- sample(1:nrow(posterior_samples), n_parameter_sets)
parameter_sets <- posterior_samples[sampled_indices, ]

# Setup dimensions first
# n_days <- 11000 # Based on your 'times' parameter
all_simulations <- array(NA, dim = c(n_parameter_sets, n_sims_per_param, n_days))

for(i in 1:n_parameter_sets) {
  current_params <- parameter_sets[i, ]
  simulation_results[[paste0("params_", i)]] <- current_params
  
  # Run SEIR model
  lambda <- simulate(parameters = c(current_params, par), init.state, times = times)
  simulation_results[[paste0("lambda_", i)]] <- lambda
  
  # Extract hospitalization counts
  lambda_hosp <- lambda$gastro_hosp_model_4
  n_days <- length(lambda_hosp)  # At the start of your function
  
  for(sim in 1:n_sims_per_param) {
    # Get counts from NHPoisson
    scaled_lambda <- lambda_hosp/mean(lambda_hosp)
    times <- simNHP.fun(lambda = scaled_lambda)$posNH
    simulated_counts <- tabulate(ceiling(times), nbins = length(lambda_hosp))

    all_simulations[i, sim, 1:length(simulated_counts)] <- simulated_counts
  }
}

simulation_results[["all_simulations"]] <- all_simulations

# Calculate summary statistics
# For each day, calculate median and 95% prediction intervals
summary_stats <- array(NA, dim = c(n_days, 3), 
                       dimnames = list(NULL, c("median", "lower_95", "upper_95")))

for(day in 1:n_days) {
  all_values <- as.vector(all_simulations[, , day])
  summary_stats[day, ] <- c(
    median = median(all_values),
    lower_95 = quantile(all_values, 0.025),
    upper_95 = quantile(all_values, 0.975)
  )
}

simulation_results[["summary_stats"]] <- summary_stats

# You might want to plot the results
library(ggplot2)

# Create a data frame for plotting
plot_data <- data.frame(
  day = 1:n_days,
  median = summary_stats[, "median"],
  lower = summary_stats[, "lower_95"],
  upper = summary_stats[, "upper_95"]
)

# Create plot
ggplot(plot_data, aes(x = day)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(aes(y = median)) +
  theme_minimal() +
  labs(x = "Day", y = "Hospitalization Count",
       title = "Predicted Hospitalizations with 95% Prediction Interval")


#Generation of the occurrence times of a homogeneours PP with constant intensity
#0.01 in a period of time of length 1000
aux<-simNHP.fun(lambda=rep(0.01,1000))
aux$posNH
#if we want reproducible results, we can fixed the seed in the generation process
aux<-simNHP.fun(lambda=rep(0.01,1000),fixed.seed=123)
aux$posNH
#and the result is:
# [1] 85 143 275 279 284 316 347 362 634 637 738 786 814 852 870 955
#Generation of the occurrence times of a NHPP with time-varying intensity t in
#a period of time of length 500
t<-runif(500, 0.01,0.1)
aux<-simNHP.fun(lambda=t)
aux$posNH
simulated_counts <- tabulate(aux$posNH, nbins = length(t))


scaled_lambda <- lambda_hosp/mean(lambda_hosp)

lambda_aki_hosp <- lambda$aki_hosp_model_4
times <- simNHP.fun(lambda = scaled_lambda)$posNH
hist(times)
daily_counts <- tabulate(ceiling(times), nbins = length(lambda_hosp))
print(summary(daily_counts))
print(sum(daily_counts))


lambda_aki_hosp <- lambda$aki_hosp_model_4
times <- simNHP.fun(lambda = lambda_aki_hosp/mean(lambda_aki_hosp))
times$posNH
daily_counts <- tabulate(ceiling(times$posNH), nbins = length(lambda_aki_hosp))
print(daily_counts)
