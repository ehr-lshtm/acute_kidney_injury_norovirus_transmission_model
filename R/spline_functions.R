# spline functions for aki hospitalisations observations

# specify spline
spl <-bs(observation_data$time, degree=3, df=3)

# aki hosp splines

model_spl4 <- glm(aki_hosp_obs_4 ~ spl,observation_data,family=quasipoisson)
model_spl_pred4 <- predict(model_spl4,type="response")
model_spl_pred4 <- as.data.table(model_spl_pred4)
setDT(model_spl_pred4)
model_spl_pred4[, time := 0:(.N - 1)]

# model_spl4 <- glm(aki_hosp_obs_4 ~ spl,observation_data,family=quasipoisson)
# model_spl_pred4 <- predict(model_spl4,type="response")
# model_spl_pred4 <- as.data.table(model_spl_pred4)
# median_value <- median(model_spl_pred4$model_spl_pred4)
# model_spl_pred4[, `:=`(median = median_value,
#                       model_spl_pred4_diff = model_spl_pred4 - median_value)]
# model_spl_pred4[, time := 0:(.N - 1)]
# model_spl_pred4_diff <- model_spl_pred4[, .(model_spl_pred4_diff, time)]
# setDT(model_spl_pred4_diff)

# gastro hosp splines

# gastro_model_spl1 <- glm(gastro_hosp_obs_1 ~ spl,observation_data2,family=quasipoisson)
# gastro_model_spl_pred1 <- predict(gastro_model_spl1,type="response")
# gastro_model_spl_pred1 <- as.data.table(gastro_model_spl_pred1)
# median_value <- median(gastro_model_spl_pred1$gastro_model_spl_pred1)
# gastro_model_spl_pred1[, `:=`(median = median_value,
#                               gastro_model_spl_pred1_diff = gastro_model_spl_pred1 - median_value)]
# gastro_model_spl_pred1[, time := 52:(51 + .N)]
# gastro_model_spl_pred1_diff <- gastro_model_spl_pred1[, .(gastro_model_spl_pred1_diff, time)]
# 
# gastro_model_spl_pred1_diff1.1 <- data.table(time = 0:51, gastro_model_spl_pred1_diff = rep(NA, 52))
# gastro_model_spl_pred1_diff <- rbindlist(list(gastro_model_spl_pred1_diff1.1, gastro_model_spl_pred1_diff), use.names = TRUE)
# setDT(gastro_model_spl_pred1_diff)

gastro_model_spl4 <- glm(gastro_hosp_obs_4 ~ spl,observation_data,family=quasipoisson)
gastro_model_spl_pred4 <- predict(gastro_model_spl4,type="response")
gastro_model_spl_pred4 <- as.data.table(gastro_model_spl_pred4)
# median_value <- median(gastro_model_spl_pred4$gastro_model_spl_pred4)
# gastro_model_spl_pred4[, `:=`(median = median_value,
#                               gastro_model_spl_pred4_diff = gastro_model_spl_pred4 - median_value)]
gastro_model_spl_pred4[, time := 0:(.N - 1)]
# gastro_model_spl_pred4_diff <- gastro_model_spl_pred4[, .(gastro_model_spl_pred4_diff, time)]

# setDT(gastro_model_spl_pred4_diff)
setDT(gastro_model_spl_pred4)

# gastro gp splines

gastro_gp_model_spl1 <- glm(gastro_gp_obs_1 ~ spl,observation_data,family=quasipoisson)
gastro_gp_model_spl_pred1 <- predict(gastro_gp_model_spl1,type="response")
gastro_gp_model_spl_pred1 <- as.data.table(gastro_gp_model_spl_pred1)
gastro_gp_model_spl_pred1[, time := 52:(51 + .N)]
median_value <- median(gastro_gp_model_spl_pred1$gastro_gp_model_spl_pred1)
gastro_gp_model_spl_pred1[, `:=`(median = median_value,
                                 gastro_gp_model_spl_pred1_diff = gastro_gp_model_spl_pred1 - median_value)]
gastro_gp_model_spl_pred1[, time := 52:(51 + .N)]
gastro_gp_model_spl_pred1_diff <- gastro_gp_model_spl_pred1[, .(gastro_gp_model_spl_pred1_diff, time)]
gastro_gp_model_spl_pred1_diff1.1 <- data.table(time = 0:51, gastro_gp_model_spl_pred1_diff = rep(NA, 52))
gastro_gp_model_spl_pred1_diff <- rbindlist(list(gastro_gp_model_spl_pred1_diff1.1, gastro_gp_model_spl_pred1_diff), use.names = TRUE)
setDT(gastro_gp_model_spl_pred1_diff)

# gastro_gp_model_spl1 <- glm(gastro_gp_obs_1 ~ spl,observation_data,family=quasipoisson)
# gastro_gp_model_spl_pred1 <- predict(gastro_gp_model_spl1,type="response")
# gastro_gp_model_spl_pred1 <- as.data.table(gastro_gp_model_spl_pred1)
# gastro_gp_model_spl_pred1[, time := 52:(51 + .N)]
# gastro_gp_model_spl_pred1.1 <- data.table(time = 0:51, gastro_gp_model_spl_pred1 = rep(NA, 52))
# gastro_gp_model_spl_pred1 <- rbindlist(list(gastro_gp_model_spl_pred1.1, gastro_gp_model_spl_pred1), use.names = TRUE)
# setDT(gastro_gp_model_spl_pred1)

gastro_gp_model_spl2 <- glm(gastro_gp_obs_2 ~ spl,observation_data,family=quasipoisson)
gastro_gp_model_spl_pred2 <- predict(gastro_gp_model_spl2,type="response")
gastro_gp_model_spl_pred2 <- as.data.table(gastro_gp_model_spl_pred2)
median_value <- median(gastro_gp_model_spl_pred2$gastro_gp_model_spl_pred2)
gastro_gp_model_spl_pred2[, `:=`(median = median_value,
                                 gastro_gp_model_spl_pred2_diff = gastro_gp_model_spl_pred2 - median_value)]
gastro_gp_model_spl_pred2[, time := 0:(.N - 1)]
gastro_gp_model_spl_pred2_diff <- gastro_gp_model_spl_pred2[, .(gastro_gp_model_spl_pred2_diff, time)]

setDT(gastro_gp_model_spl_pred2_diff)
# setDT(gastro_gp_model_spl_pred2)
