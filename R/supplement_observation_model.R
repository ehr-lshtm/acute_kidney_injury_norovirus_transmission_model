# step by step observation model implementation

## function for residual

# Function to calculate and plot residuals between model output and observed data
# Arguments:
#   model_vec  : numeric vector of model output
#   obs_vec    : numeric vector of observed data
#   label      : string label for the comparison (used in plot titles and output)

compare_lines <- function(model_vec, obs_vec, label = "Comparison") {
  
  # Check lengths and trim to shortest if needed
  n <- min(length(model_vec), length(obs_vec))
  if (length(model_vec) != length(obs_vec)) {
    warning(paste(label, ": vectors are different lengths, trimming to", n))
  }
  model_vec <- model_vec[1:n]
  obs_vec   <- obs_vec[1:n]
  
  # Plot model vs observed
  plot(model_vec, type = "l", col = "blue", 
       ylab = "Incidence per 100,000", xlab = "Time",
       main = paste(label, ": Model vs Data"))
  lines(obs_vec, col = "red")
  legend("topright", legend = c("Model Output", "Data"), col = c("blue", "red"), lty = 1)
  
  # Calculate residuals (model - data)
  residuals <- model_vec - obs_vec
  
  # Plot residuals
  plot(residuals, type = "l", ylab = "Residual (Model - Data)", xlab = "Time",
       main = paste(label, ": Residuals"))
  abline(h = 0, lty = 2, col = "grey")
  
  # Fit statistics (na.rm = TRUE to handle any remaining NAs)
  ssr  <- sum(residuals^2, na.rm = TRUE)
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae  <- mean(abs(residuals), na.rm = TRUE)
  
  cat("\n---", label, "---\n")
  cat("SSR: ", ssr, "\nRMSE:", rmse, "\nMAE: ", mae, "\n")
  
  # Return results invisibly for further use if needed
  invisible(list(residuals = residuals, ssr = ssr, rmse = rmse, mae = mae))
}

# --- Example usage ---

# Single comparison
compare_lines(summarized_data$infectious_symp_1, 
              observation_data$gastro_gp_obs_1, 
              label = "Symptomatic strain 1")


## gp 0-4

plot(summarized_data$infectious_symp_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output", "Data"), col = c("blue", "red"), lty = 1)

compare_lines(summarized_data$infectious_symp_1, 
              observation_data$gastro_gp_obs_1, 
              label = "Symptomatic strain 1")

plot(summarized_data$infectious_symp_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
lines(summarized_data$gastro_gp_model_spl_pred1, col = "black")
legend("topright", legend = c("Model Output", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

compare_lines(summarized_data$infectious_symp_1, 
              summarized_data$gastro_gp_model_spl_pred1, 
              label = "Symptomatic strain 1")

plot(gastro_gp_model_spl_pred1$gastro_gp_model_spl_pred1_diff, ylim = c(-20,20), ylab = "Incidence per 100,000", xlab = "time")

plot(summarized_data$infectious_symp_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(summarized_data$gastro_gp_model_1_infection_model_spline, col = "black")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

compare_lines(summarized_data$infectious_symp_1, 
              summarized_data$gastro_gp_model_1_infection_model_spline, 
              label = "Symptomatic strain 1")

plot(summarized_data$gastro_gp_model_1_infection, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output with cubic spline", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$gastro_gp_model_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output with cubic spline", "Data"), col = c("blue", "red"), lty = 1)

compare_lines(summarized_data$gastro_gp_model_1, 
              observation_data$gastro_gp_obs_1, 
              label = "Residual plot between GP consultations in the model output and observed data")

## aki 65+

plot(summarized_data$infectious_symp_4, type = "l", col = "blue", ylim = c(0,200), xlim = c(0,363), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red", , xlim = c(0,363))
legend("topright", legend = c("Model Output", "Data"), col = c("blue", "red"), lty = 1)


compare_lines(summarized_data$infectious_symp_4, 
              observation_data$aki_hosp_obs_4, 
              label = "Symptomatic & AKI 4")


plot(summarized_data$aki_hosp_model_4_infection, type = "l", col = "blue", ylim = c(0,200), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output with parameter (norovirus infections linked to an AKI hospitalisation)", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$aki_hosp_model_4_infection, type = "l", col = "blue", ylim = c(0,200), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
lines(summarized_data$model_spl_pred4, col = "black")
legend("topright", legend = c("Model Output with parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

compare_lines(observation_data$aki_hosp_obs_4, 
              summarized_data$model_spl_pred4, 
              label = "Symptomatic & AKI 4")

plot(summarized_data$aki_hosp_model_4_difference, ylim = c(-10,10), ylab = "Incidence per 100,000")

plot(summarized_data$aki_hosp_model_4, type = "l", col = "blue", ylim = c(0,200), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
lines(summarized_data$model_spl_pred4, col = "black")
legend("topright", legend = c("Changing incidence of the model output with reporting parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$aki_hosp_model_4, type = "l", col = "blue", ylim = c(0,200), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output", "Data"), col = c("blue", "red"), lty = 1)

compare_lines(observation_data2$aki_hosp_obs_4,
              summarized_data$aki_hosp_model_4, 
              label = "Residual plot between AKI hospitalisations in the model output and observed data (excluding data points attributed to heat waves)")


compare_lines(observation_data$aki_hosp_obs_4,
              summarized_data$aki_hosp_model_4, 
              label = "Residual plot between AKI hospitalisations in the model output and observed data (including data points attributed to heat waves)")






# gastro 65+

plot(summarized_data$infectious_symp_4, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$gastro_hosp_model_4_infection, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
legend("topright", legend = c("Model Output with reporting parameter", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$gastro_hosp_model_4_infection, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
lines(summarized_data$gastro_model_spl_pred4, col = "black")
legend("topright", legend = c("Model Output with reporting parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$gastro_hosp_model_4_difference, ylim = c(-10,10), ylab = "Incidence per 100,000")

plot(summarized_data$gastro_hosp_model_4, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
lines(summarized_data$gastro_model_spl_pred4, col = "black")
legend("topright", legend = c("Changing incidence of the model output with reporting parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$gastro_hosp_model_4, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output", "Data"), col = c("blue", "red"), lty = 1)

compare_lines(observation_data$gastro_hosp_obs_4,
              summarized_data$gastro_hosp_model_4, 
              label = "Residual plot between gastro hospitalisations in the model output and observed data")
