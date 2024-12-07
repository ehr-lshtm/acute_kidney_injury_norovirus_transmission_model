###### quasi poisson over dispersion

calculate_dispersion <- function(counts) {
  variance <- var(counts, na.rm=TRUE)
  mean_val <- mean(counts, na.rm=TRUE)
  phi <- variance/mean_val
  return(phi)
}

# exclude NAs for gp data - because first year is removed from the fit
observation_data_gastro_gp_1 <- observation_data |> 
  filter(!is.na(gastro_gp_obs_1)) |>
  dplyr::select(time, gastro_gp_obs_1)

dispersion_params <- list(
  noro_obs_1 = calculate_dispersion(observation_data$noro_obs_1),
  noro_obs_2 = calculate_dispersion(observation_data$noro_obs_2), 
  noro_obs_3 = calculate_dispersion(observation_data$noro_obs_3),
  noro_obs_4 = calculate_dispersion(observation_data$noro_obs_4),
  aki_hosp_4 = calculate_dispersion(observation_data$aki_hosp_obs_4),
  gastro_hosp_obs_4 = calculate_dispersion(observation_data$gastro_hosp_obs_4),
  gastro_gp_1 = calculate_dispersion(observation_data_gastro_gp_1$gastro_gp_obs_1),
  gastro_gp_2 = calculate_dispersion(observation_data$gastro_gp_obs_2)
)
