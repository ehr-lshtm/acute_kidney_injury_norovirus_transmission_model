### calculate overdispersion

calculate_overdispersion <- function(counts) {
  variance <- var(counts, na.rm=TRUE)
  mean_val <- mean(counts, na.rm=TRUE)
  size <- ifelse(variance > mean_val,
                 mean_val^2 / (variance - mean_val),
                 100)  # Default to large size if underdispersed
  return(size)
}

# exclude NAs for gp data - because first year is removed from the fit. calculate overdispersion for gastro_gp_1 where it is counted.
observation_data_gastro_gp_1 <- observation_data |> 
  filter(!is.na(gastro_gp_obs_1)) |>
  dplyr::select(time, gastro_gp_obs_1)

sizes <- list(
  noro_obs_1 = calculate_overdispersion(observation_data$noro_obs_1),
  noro_obs_2 = calculate_overdispersion(observation_data$noro_obs_2), 
  noro_obs_3 = calculate_overdispersion(observation_data$noro_obs_3),
  noro_obs_4 = calculate_overdispersion(observation_data$noro_obs_4),
  aki_hosp_4 = calculate_overdispersion(observation_data$aki_hosp_obs_4),
  gastro_hosp_obs_4 = calculate_overdispersion(observation_data$gastro_hosp_obs_4),
  gastro_gp_1 = calculate_overdispersion(observation_data_gastro_gp_1$gastro_gp_obs_1),
  gastro_gp_2 = calculate_overdispersion(observation_data$gastro_gp_obs_2)
)

