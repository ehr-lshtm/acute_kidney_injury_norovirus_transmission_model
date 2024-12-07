generate_trajectories_with_uncertainty <- function(trace_df, n_samples = 100, init.state, outcome = "aki_hosp_model_4") {
  trace_matrix <- as.matrix(trace_df)
  param_names <- colnames(trace_matrix)
  lhs_points <- randomLHS(n_samples, ncol(trace_matrix))
  resampled <- matrix(0, nrow = n_samples, ncol = ncol(trace_matrix))
  colnames(resampled) <- param_names
  
  for(i in seq_along(param_names)) {
    resampled[,i] <- quantile(trace_matrix[,i], lhs_points[,i])
  }
  
  sampled_thetas <- as.data.frame(resampled)
  trajectories <- matrix(0, nrow = n_samples, ncol = 365)
  
  pb <- progress_bar$new(total = n_samples)
  
  for(i in 1:n_samples) {
    pb$tick()
    
    theta_i <- sampled_thetas[i,]
    names(theta_i) <- param_names
    theta_i <- c(theta_i, par)
    sim_result <- simulate(theta_i, init.state, times = 11000)
    trajectories[i,] <- as.numeric(sim_result[[outcome]])
    
    if (i %% 10 == 0) {
      cat(sprintf("\rProgress: %.2f%%", i / n_samples * 100))
      flush.console()
    }
  }
  
  pb$terminate()
  return(trajectories)
}

# Run with:
# trajectories <- generate_trajectories_with_uncertainty(traceBurnThin, n_samples = 100, init.state = init.state, outcome = "noro_model_1")
# 
# # 95% credible intervals
ci_lower <- apply(aki_trajectories, 2, quantile, 0.025)
ci_upper <- apply(aki_trajectories, 2, quantile, 0.975)
median_traj <- apply(aki_trajectories, 2, median)

plot(1:365, median_traj, type='l', ylim=c(0,200),
     xlab='Day', ylab='Infections per 100,000')
lines(1:365, ci_lower, lty=2)
lines(1:365, ci_upper, lty=2)
