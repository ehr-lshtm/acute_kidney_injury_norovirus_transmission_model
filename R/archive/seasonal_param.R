plot_data <- aki_trajectory %>%
  as.data.frame() %>%
  mutate(iteration = 1:n()) %>%
  # filter(iteration %in% sample(iteration, 20)) %>%
  pivot_longer(
    cols = -iteration,
    names_to = "time",
    values_to = "count"
  ) %>%
  mutate(time = as.numeric(gsub("V", "", time)))

ggplot(plot_data, aes(x = time, y = count, group = iteration, color = factor(iteration))) +
  geom_line(alpha = 0.7) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    color = "black",
    size = 1.2,
    geom = "line"
  ) +
  scale_color_manual(name = "Iteration", 
                     values = rainbow(20)) +
  theme_minimal() +
  labs(
    title = "AKI trajectories random 20 Iterations",
    x = "Time Point",
    y = "Count",
    caption = "Black line shows mean trend"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


parameters = c(theta, par)
test_winter <- exp(parameters[["aki_hospitalisation_4_winter"]])
test_summer <- exp(parameters[["aki_hospitalisation_4_summer"]])

test_winter <- 0.0183
test_summer <- 0.0053

data <- summarized_data |>
  filter(time < 364) |> 
  left_join(observation_data |> select(aki_hosp_obs_4, time), by = "time") |> 
  mutate(seasonal_param_test = ((0.5 * (1 + cos(2 * pi * (week_number - 1)/52))) * test_winter + (0.5 * (1 - cos(2 * pi * (week_number - 1)/52))) * test_summer),
         seasonal_param_test2 = ((0.5 * (1 + cos(2 * pi * (week_number - 1)/52))) * test_winter + (0.5 * (1 - cos(2 * pi * (week_number - 1)/52))) * test_summer),
#            aki_hosp_model_4_with_factor = ((((infectious_symp_4 / init.state[4, 1] * 100000) * seasonal_param_test) - median((infectious_symp_4 / init.state[4, 1] * 100000) * seasonal_param_test)
#   ) + model_spl_pred4),
#   aki_hosp_model_4_without_factor = ((((infectious_symp_4 / init.state[4, 1] * 100000) * seasonal_param_test ) - median((infectious_symp_4 / init.state[4, 1] * 100000) * seasonal_param_test)
#   ) + model_spl_pred4),
# aki_hosp_model_4_with_factor_test = ((((infectious_symp_4 / init.state[4, 1] * 100000) * seasonal_param_test2) - median((infectious_symp_4 / init.state[4, 1] * 100000) * seasonal_param_test2)
# ) + model_spl_pred4),
infec_sym_only = infectious_symp_4 / init.state[4, 1] * 100000,
infec_sym_with_seasonal_param = infectious_symp_4 / init.state[4, 1] * 100000 * seasonal_param_test)


plot(data$time, data$aki_hosp_obs_4, ylim = c(0, 150))
lines(data$time, data$aki_hosp_model_4_with_factor, col = "black")
lines(data$time, data$aki_hosp_model_4_without_factor, col = "red")
lines(data$time, data$aki_hosp_model_4_with_factor_test, col = "blue")
lines(data$time, data$infec_sym_only, col = "green")

lines(data$time, data$infec_sym_with_seasonal_param, col = "purple")
  
plot(data$time, data$seasonal_param_test, xlab = "Time", ylab = "Reporting paramater for 65+ to surveillance", col = "black")
