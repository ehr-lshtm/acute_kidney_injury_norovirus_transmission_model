# First function to generate LHS samples
generate_lhs_samples <- function(trace_df, n_samples = 100) {
  trace_matrix <- as.matrix(trace_df)
  param_names <- colnames(trace_matrix)
  lhs_points <- randomLHS(n_samples, ncol(trace_matrix))
  resampled <- matrix(0, nrow = n_samples, ncol = ncol(trace_matrix))
  colnames(resampled) <- param_names
  
  for(i in seq_along(param_names)) {
    resampled[,i] <- quantile(trace_matrix[,i], lhs_points[,i], names = FALSE)
  }
  
  sampled_thetas <- as.data.frame(resampled)
  names(sampled_thetas) <- param_names
  return(sampled_thetas)
}

# Second function to generate trajectories using pre-sampled parameters
generate_trajectories_with_uncertainty <- function(sampled_thetas, init.state, outcome = "aki_hosp_model_4") {
  n_samples <- nrow(sampled_thetas)
  trajectories <- matrix(0, nrow = n_samples, ncol = 365)
  
  pb <- progress_bar$new(total = n_samples)
  
  for(i in 1:n_samples) {
    pb$tick()
    theta_i <- sampled_thetas[i,]
    names(theta_i) <- colnames(sampled_thetas)
    theta_i <- c(theta_i, par)
    sim_result <- simulate(theta_i, init.state, times = 11000)
    trajectories[i,] <- as.numeric(sim_result[[outcome]])
    
    if (i %% 10 == 0) {
      cat(sprintf("\rProgress: %.2f%%", i / n_samples * 100))
    }
  }
  
  pb$terminate()
  return(trajectories)
}

# test
# lhs_samples <- generate_lhs_samples(my_trace, n_samples = 1000)
# aki_trajectories <- generate_trajectories_with_uncertainty(lhs_samples, init.state, "aki_hosp_model_4")

generate_age_group_incidence_with_uncertainty <- function(sampled_thetas, init.state, age_group = 1) {
  n_samples <- nrow(sampled_thetas)
  trajectories <- matrix(0, nrow = n_samples, ncol = 365)
  
  pb <- progress_bar$new(total = n_samples)
  
  for(i in 1:n_samples) {
    pb$tick()
    theta_i <- sampled_thetas[i,]
    names(theta_i) <- colnames(sampled_thetas)
    theta_i <- c(theta_i, par)
    sim_result <- simulate(theta_i, init.state, times = 11000, age.incidence = TRUE)
    trajectories[i,] <- as.numeric(sim_result[age_group, 2])
    
    if (i %% 10 == 0) {
      cat(sprintf("\rProgress: %.2f%%", i / n_samples * 100))
    }
  }
  
  pb$terminate()
  return(trajectories)
}

# test
# age_group_1_incidence <- generate_age_group_incidence_with_uncertainty(lhs_samples, init.state, 1)
