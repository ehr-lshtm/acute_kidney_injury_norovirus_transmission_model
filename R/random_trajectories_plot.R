# noro_1_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_1")
# noro_2_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_2")
# noro_3_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_3")
# noro_4_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_4")
# 
# gastro_gp_1_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "gastro_gp_model_1")
# gastro_gp_2_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "gastro_gp_model_2")
# gastro_hosp_4_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "gastro_hosp_model_4")
# aki_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "aki_hosp_model_4")

# Get total number of iterations
total_iterations <- nrow(noro_1_trajectory)

# Randomly select n_samples iteration numbers
random_iterations <- sample(1:total_iterations, 10)

plot_random_trajectories <- function(trajectory_data, observation_data, obs_variable, n_samples = 20, title = "Trajectories") {
  
  # Process the trajectory data
  plot_data <- trajectory_data %>%
    as.data.frame() %>%
    mutate(iteration = 1:n()) %>%
    # filter(iteration <= n_samples) %>%  # Changed to select first n_samples iterations
    filter(iteration %in% random_iterations) %>%  # Changed to select random iterations
    pivot_longer(
      cols = -iteration,
      names_to = "time",
      values_to = "count"
    ) %>%
    mutate(time = as.numeric(gsub("V", "", time))) %>%
    left_join(observation_data, by = "time")
  
  # Create the plot
  ggplot(plot_data, aes(x = time, y = count, group = iteration)) +
    geom_line(aes(color = factor(iteration)), alpha = 0.7) +
    geom_point(aes(y = !!sym(obs_variable)), color = "black", size = 1.5, show.legend = FALSE) +
    stat_summary(
      aes(group = 1),
      fun = mean,
      color = "black",
      size = 1.2,
      geom = "line"
    ) +
    scale_color_manual(name = "Iteration", 
                       values = rainbow(n_samples)) +
    theme_minimal() +
    labs(
      title = title,
      x = "Time",
      y = "Count") +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

noro_1 <- plot_random_trajectories(noro_1_trajectory, observation_data, obs_variable = "noro_obs_1", n_samples = 10, title = "Trajectories noro surveillance 0-4")
# noro_2 <- plot_random_trajectories(noro_2_trajectory, observation_data, obs_variable = "noro_obs_2", n_samples = 10, title = "Trajectories noro surveillance 5-14")
noro_3 <- plot_random_trajectories(noro_3_trajectory, observation_data, obs_variable = "noro_obs_3", n_samples = 10, title = "Trajectories noro surveillance 15-64")
noro_4 <- plot_random_trajectories(noro_4_trajectory, observation_data, obs_variable = "noro_obs_4", n_samples = 10, title = "Trajectories noro surveillance 65+")

# ggarrange(noro_1, noro_3, noro_4, ncol = 2, nrow = 2, common.legend = TRUE)

gastro_gp_1 <- plot_random_trajectories(gastro_gp_1_trajectory, observation_data, obs_variable = "gastro_gp_obs_1", n_samples = 10, title = "Trajectories gastro gp 0-4")
# gastro_gp_2 <- plot_random_trajectories(gastro_gp_2_trajectory, observation_data, obs_variable = "gastro_gp_obs_2", n_samples = 10, title = "Trajectories gastro gp 5-14")
gastro_hosp_4 <- plot_random_trajectories(gastro_hosp_4_trajectory, observation_data, obs_variable = "gastro_hosp_obs_4", n_samples = 10, title = "Trajectories gastro hosp 65+")
aki <- plot_random_trajectories(aki_trajectory, observation_data, obs_variable = "aki_hosp_obs_4", n_samples = 10, title = "Trajectories aki hosp 65+")

# ggarrange(gastro_gp_1, gastro_hosp_4, aki, ncol = 2, nrow = 2, common.legend = TRUE)
