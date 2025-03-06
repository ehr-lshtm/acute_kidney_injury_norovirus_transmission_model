# step by step observation model implementation

## gp 0-4

plot(summarized_data$infectious_symp_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$infectious_symp_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
lines(summarized_data$gastro_gp_model_spl_pred1, col = "black")
legend("topright", legend = c("Model Output", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(gastro_gp_model_spl_pred1$gastro_gp_model_spl_pred1_diff, ylim = c(-20,20), ylab = "Incidence per 100,000", xlab = "time")

plot(summarized_data$infectious_symp_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(summarized_data$gastro_gp_model_1_infection_model_spline, col = "black")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$gastro_gp_model_1_infection, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output with cubic spline", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$gastro_gp_model_1, type = "l", col = "blue", ylim = c(0,500), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_gp_obs_1, col = "red")
legend("topright", legend = c("Model Output with cubic spline", "Data"), col = c("blue", "red"), lty = 1)

## aki 65+

plot(summarized_data$infectious_symp_4, type = "l", col = "blue", ylim = c(0,150), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
legend("topright", legend = c("Model Output", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$aki_hosp_model_4_infection, type = "l", col = "blue", ylim = c(0,150), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output with parameter (number of norovirus infections linked to an AKI hospitalisation)", "Data"), col = c("blue", "red"), lty = 1)

plot(summarized_data$aki_hosp_model_4_infection, type = "l", col = "blue", ylim = c(0,150), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
lines(summarized_data$model_spl_pred4, col = "black")
legend("topright", legend = c("Model Output with parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$aki_hosp_model_4_difference, ylim = c(-10,10), ylab = "Incidence per 100,000")

plot(summarized_data$aki_hosp_model_4, type = "l", col = "blue", ylim = c(0,150), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
lines(summarized_data$model_spl_pred4, col = "black")
legend("topright", legend = c("Changing incidence of the model output with reporting parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$aki_hosp_model_4, type = "l", col = "blue", ylim = c(0,150), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$aki_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output", "Data"), col = c("blue", "red"), lty = 1)

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

plot(summarized_data$gastro_hosp_model_4, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
lines(summarized_data$gastro_model_spl_pred4, col = "black")
legend("topright", legend = c("Changing incidence of the model output with reporting parameter", "Data", "Cubic spline"), col = c("blue", "red", "black"), lty = 1)

plot(summarized_data$gastro_hosp_model_4, type = "l", col = "blue", ylim = c(0,100), ylab = "Incidence per 100,000", xlab = "time")
lines(observation_data$gastro_hosp_obs_4, col = "red")
legend("topright", legend = c("Model output", "Data"), col = c("blue", "red"), lty = 1)
