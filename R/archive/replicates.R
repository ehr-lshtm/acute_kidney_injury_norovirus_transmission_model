# Function to summarize the fit of 100 thetas sampled from an MCMC trace
replicates <- function(mcmc_trace, n_samples = 100) {
  # Extract posterior samples from the MCMC trace
  posterior_samples <- as.data.frame(mcmc_trace)
  
  # Sample 100 values of theta
  sampled_thetas <- posterior_samples %>% sample_n(n_samples)
  
  # Create an empty list to store the replicates
  replicate_list <- list()
  
  # Create a progress bar
  pb <- progress_bar$new(total = n_samples)
  
  # Generate model replicates for each sampled theta
  for (i in seq_len(n_samples)) {
    
    # Update progress bar
    pb$tick()
    
    theta_i <- sampled_thetas[i, ]
    theta_i <- c(theta_i, par)
    replicate_i <- simulate(theta_i, init.state, times = 11000)
    replicate_list[[i]] <- replicate_i
    
    # Print progress periodically
    if (i %% 10 == 0) {
      cat(sprintf("\rProgress: %.2f%%", i / n_samples * 100))
      flush.console()
      }
    
  }
  
  # Close progress bar
  pb$terminate()
  
  # Combine the replicates into a data frame
  replicates_df <- bind_rows(replicate_list, .id = "replicate")
  
  # Return the replicates data frame
  return(replicates_df)
}


# Function to summarize the fit of 100 thetas sampled from an MCMC trace
replicates_age_group <- function(mcmc_trace, n_samples = 100) {
  # Extract posterior samples from the MCMC trace
  posterior_samples <- as.data.frame(mcmc_trace)
  
  # Sample 100 values of theta
  sampled_thetas <- posterior_samples %>% sample_n(n_samples)
  
  # Create an empty list to store the replicates
  replicate_list <- list()
  
  # Create a progress bar
  pb <- progress_bar$new(total = n_samples)
  
  # Generate model replicates for each sampled theta
  for (i in seq_len(n_samples)) {
    
    # Update progress bar
    pb$tick()
    
    theta_i <- sampled_thetas[i, ]
    theta_i <- c(theta_i, par)
    replicate_i <- simulate(theta_i, init.state, times = 11000, age.incidence = TRUE)
    replicate_list[[i]] <- replicate_i
    
    # Print progress periodically
    if (i %% 10 == 0) {
      cat(sprintf("\rProgress: %.2f%%", i / n_samples * 100))
      flush.console()
    }
    
  }
  
  # Close progress bar
  pb$terminate()
  
  # Combine the replicates into a data frame
  replicates_df_age_group <- bind_rows(replicate_list, .id = "replicate")
  
  # Return the replicates data frame
  return(replicates_df_age_group)
}
