# First function to generate random samples
generate_random_samples <- function(trace_df, n_samples = 100) {
  trace_matrix <- as.matrix(trace_df)
  param_names <- colnames(trace_matrix)
  resampled <- matrix(0, nrow = n_samples, ncol = ncol(trace_matrix))
  colnames(resampled) <- param_names
  
  for(i in seq_along(param_names)) {
    # Randomly sample from the trace matrix columns
    random_indices <- sample(1:nrow(trace_matrix), n_samples, replace = TRUE)
    resampled[,i] <- trace_matrix[random_indices, i]
  }
  
  sampled_thetas <- as.data.frame(resampled)
  names(sampled_thetas) <- param_names
  return(sampled_thetas)
}

# Second function remains the same
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

# random_samples <- generate_random_samples(my_trace, n_samples = 1000)
# aki_trajectories <- generate_trajectories_with_uncertainty(random_samples, init.state, "aki_hosp_model_4")
# 
# aki_quantiles_df <- data.frame(
#   time = 1:365,
#   median_traj = apply(aki_trajectories, 2, median),
#   ci_lower = apply(aki_trajectories, 2, quantile, 0.025),
#   ci_upper = apply(aki_trajectories, 2, quantile, 0.975)
# )
# 
# aki_fit_points <- aki_quantiles_df |> 
#   left_join(observation_data |> select(time, week_date, aki_hosp_obs_4), by = "time") |>
#   filter(time < 364) |> 
#   ggplot(aes(x = week_date)) +
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
#   geom_point(aes(y = aki_hosp_obs_4, color = "Observed"), size = 1.5) +
#   geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   # Heatwave periods
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2019-06-28"), xmax = as.Date("2019-06-30"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2019-07-21"), xmax = as.Date("2019-07-28"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2019-08-23"), xmax = as.Date("2019-08-29"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2018-06-25"), xmax = as.Date("2018-06-27"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2018-06-30"), xmax = as.Date("2018-07-10"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2018-07-21"), xmax = as.Date("2018-07-29"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2018-08-01"), xmax = as.Date("2018-08-09"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2017-06-16"), xmax = as.Date("2017-06-23"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2017-07-05"), xmax = as.Date("2017-07-07"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2016-07-18"), xmax = as.Date("2016-07-22"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2016-08-22"), xmax = as.Date("2016-08-26"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2016-09-12"), xmax = as.Date("2016-09-17"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2015-07-01"), xmax = as.Date("2015-07-03"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2013-07-12"), xmax = as.Date("2013-07-23"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "#FF000030",
#            xmin = as.Date("2013-07-30"), xmax = as.Date("2013-08-02"),
#            ymin = -Inf, ymax = Inf) +
#   theme_minimal(base_size = 11) +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 200), 
#                      breaks = seq(0, 200, by = 50)) +
#   scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
#   scale_fill_manual(values = c("95% CrI" = "#3498db")) +
#   labs(title = "Acute kidney injury hospital admissions, 65+ years olds (HES)",
#        y = "Incidence per 100,000 person-years",
#        x = NULL) +
#   theme(
#     axis.title.y = element_text(size = 11, margin = margin(r = 10)),
#     axis.text = element_text(size = 10, color = "gray30"),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.spacing.x = unit(0.5, 'cm'),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20))
# 
# noro_1_trajectory <- generate_trajectories_with_uncertainty(random_samples, init.state, outcome = "noro_model_1")
# 
# noro_1_quantiles_df <- data.frame(
#   time = 1:365,
#   median_traj = apply(noro_1_trajectory, 2, median),
#   ci_lower = apply(noro_1_trajectory, 2, quantile, 0.025),
#   ci_upper = apply(noro_1_trajectory, 2, quantile, 0.975)
# )
# 
# noro_1_fit_points <- noro_1_quantiles_df |> 
#   left_join(observation_data |> select(time, week_date, noro_obs_1), by = "time") |>
#   filter(time < 364) |> 
#   ggplot(aes(x = week_date)) +
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
#   geom_point(aes(y = noro_obs_1, color = "Observed"), size = 1.5) +
#   geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   theme_minimal(base_size = 11) +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 100), 
#                      breaks = seq(0, 100, by = 20)) +
#   scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
#   scale_fill_manual(values = c("95% CrI" = "#3498db")) +
#   labs(title = "Norovirus laboratory surveillance, 0-4 years olds (SGSS)",
#        y = "Number of laboratory reports",
#        x = NULL) +
#   theme(
#     plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
#     plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
#     axis.title.y = element_text(size = 11, margin = margin(r = 10)),
#     axis.text = element_text(size = 10, color = "gray30"),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.spacing.x = unit(0.5, 'cm'),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
#   )
