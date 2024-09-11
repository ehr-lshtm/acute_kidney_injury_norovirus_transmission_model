# set up for using mcmc sampler

source("R/01_setup.R")

source("R/02_observation_data.R")

source("R/03_model_function.R")

source("R/04_likelihood_prior_functions.R")

source("R/fixed_parameters.R")

################################################################

# set fixed parameters for mcmcH function

par = list(
  psi = psi,
  b = b,
  d = d,
  epsilon = epsilon,
  gamma = gamma,
  n_age_groups = n_age_groups,
  rho = 0.05,
  aging = aging
  )

par[["contacts"]] <- uk_contact_rate_matrix
par[["season_change_points"]] <- c(11000, 0, 0, 0, 0, 0, 0)

init.state <- init_matrix

times <- 11000

# initial thetas

starting.value <-
  c(
    season_amp_over65 = 1.0,
    sigma = 0.78, #0.76, #0.85, #0.78, #0.95, changed
    surveillance_report_1 = 0.002,
    surveillance_report_2 = 0.0004,
    surveillance_report_3 = 0.00084, #0.0007,
    surveillance_report_4 = 0.015, #0.008, #0.015, #0.016, #0.018, #0.005, #0.05, changed
    season_amp = 3.9, #3.5, #3, #1.32, # 5.1, # 3.3,
    season_offset = 6.4, #2.7, #7.8, #7.0, #5.0, #0.2, # 0.08, changed
    aki_hospitalisation_4 = log(0.35), #log(0.24), #log(0.35), #log(0.24), #log(0.2), #log(0.1), #log(0.25), changed
    gastro_hospitalisation_4 = log(0.05), #log(0.1), #log(0.13), #log(0.042), #log(0.037), #log(0.06), changed
    gastro_gp_attend_1 = log(0.4), #log(0.35), #log(0.2), #log(0.05),
    gastro_gp_attend_2 = log(0.32), #log(0.27), #log(0.23) #log(0.2) #log(0.05)
    D_immun = 8.1, #6.2, #4.5, #7.7 #11, #15, #6.2, #7, #7.0, changed
    probT_under5 = log(0.20), #log(0.25), #log(0.21), #log(0.2), #log(0.28), #1.5, #2.15, changed
    probT_over5 = log(0.04) #log(0.028), #3.2, #2.92, changed
    )

# diagonal elements of the covariance matrix for the Gaussian proposal
                 
prop.sd <-
  c(
    season_amp_over65 = 0.02,
    sigma = 0.0015,
    surveillance_report_1 = 0.00002,
    surveillance_report_2 = 0.00001,
    surveillance_report_3 = 0.00001,
    surveillance_report_4 = 0.00005,
    season_amp = 0.06, #0.0479,#0.14, #0.054
    season_offset = 0.06, #0.6, #0.068,#0.2,
    aki_hospitalisation_4 = 0.095, #0.01 #exp(0.01),
    gastro_hospitalisation_4 = 0.095, #0.01, #0.095,
    gastro_gp_attend_1 = 0.005, #0.007, #0.002,
    gastro_gp_attend_2 = 0.005, #0.007 #0.002
    D_immun = 0.05, #0.1 #0.009, #0.07, #0.04
    probT_under5 = 0.0012, # 0.001, #0.005, #0.02, #0.009, #0.03, #0.02,#0.06,
    probT_over5 = 0.0015 #0.001, #0.005, #0.02, #0.008, #0.007, #0.00995,#0.029
  )

# lower and upper limits of each parameter
lower <- c(
  season_amp_over65 = 0,
  sigma = 0.6,
  surveillance_report_1 = 0,
  surveillance_report_2 = 0,
  surveillance_report_3 = 0,
  surveillance_report_4 = 0, #0.015,
  season_amp = 0,
  season_offset = 0,
  aki_hospitalisation_4 = -Inf,
  gastro_hospitalisation_4 = -Inf,
  gastro_gp_attend_1 = -Inf,
  gastro_gp_attend_2 = -Inf,
  D_immun = 0,
  probT_under5 = -Inf,
  probT_over5 = -Inf
)

upper <- c(
  season_amp_over65 = Inf,
  sigma = 0.9,
  surveillance_report_1 = Inf,
  surveillance_report_2 = Inf,
  surveillance_report_3 = Inf,
  surveillance_report_4 = Inf,
  season_amp = Inf,
  season_offset = Inf,
  aki_hospitalisation_4 = Inf,
  gastro_hospitalisation_4 = Inf,
  gastro_gp_attend_1 = Inf,
  gastro_gp_attend_2 = Inf,
  D_immun = 12,
  probT_under5 = Inf,
  probT_over5 = Inf
)

# additional parameters for the adaptive MCMC, see ?mcmcMh for more details
adaptSizeStart <- 500
adaptSizeCooling <- 0.99
adaptShapeStart <- 500

# number of iterations for the MCMC
iter <- 500000

set.seed(123)

mcmc_trace <-
  mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 1.5,
    verbose = FALSE
  )

# store trace values

my_trace <- mcmc(mcmc_trace$trace)
xyplot(x = my_trace)

my_trace_df <- data.frame(my_trace)
setDT(my_trace_df)

my_trace_df[, season_amp := season_amp / 100]
my_trace_df[, season_offset := season_offset / 100]
my_trace_df[, probT_under5 := exp(probT_under5)]
my_trace_df[, probT_over5 := exp(probT_over5)]
my_trace_df[, aki_hospitalisation_4 := exp(aki_hospitalisation_4)]
my_trace_df[, gastro_hospitalisation_4 := exp(gastro_hospitalisation_4)]
my_trace_df[, gastro_gp_attend_1 := exp(gastro_gp_attend_1)]
my_trace_df[, gastro_gp_attend_2 := exp(gastro_gp_attend_2)]

tail(my_trace_df)

params_trace <- mcmc(my_trace_df[,1:15])
log_density_trace <- mcmc(my_trace_df[,16])

rmarkdown::render("R/06_mcmc_outputs.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("mcmc_outputs", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

# parallel mcmc

multiple_chains <- furrr::future_imap(
  seq_len(4),
  ~ mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 1.5,
    verbose = FALSE
  ),
  .options = furrr::furrr_options(seed = TRUE)
)

trace1 <- mcmc(multiple_chains[[1]]$trace)
trace2 <- mcmc(multiple_chains[[2]]$trace)
trace3 <- mcmc(multiple_chains[[3]]$trace)
trace4 <- mcmc(multiple_chains[[4]]$trace)

trace1_params <- mcmc(trace1[,1:15])
trace1_log_density <- mcmc(trace1[,16])

trace2_params <- mcmc(trace2[,1:15])
trace2_log_density <- mcmc(trace1[,16])

trace3_params <- mcmc(trace3[,1:15])
trace3_log_density <- mcmc(trace3[,16])

trace4_params <- mcmc(trace4[,1:15])
trace4_log_density <- mcmc(trace4[,16])

multi_trace_params <- mcmc.list(list(trace1_params, trace2_params, trace3_params, trace4_params))
multi_trace_log_density <- mcmc.list(list(trace1_log_density, trace2_log_density, trace3_log_density, trace4_log_density))

trace1 |>
  as.data.frame() |>
  write_tsv("Z:/GPRD_GOLD/Hikaru/mcmc/trace_1_26072024.txt")

trace2 |>
  as.data.frame() |>
  write_tsv("Z:/GPRD_GOLD/Hikaru/mcmc/trace_2_26072024.txt")

trace3 |>
  as.data.frame() |>
  write_tsv("Z:/GPRD_GOLD/Hikaru/mcmc/trace_3_26072024.txt")

trace4 |>
  as.data.frame() |>
  write_tsv("Z:/GPRD_GOLD/Hikaru/mcmc/trace_4_26072024.txt")

########################################
########################################
########################################
##### Re-running traces already saved
########################################
########################################
########################################

my_trace <- fread("data/trace_data/my_trace_19072024.txt")
trace1 <- fread("data/trace_data/trace_1_26072024.txt")
trace2 <- fread("data/trace_data/mcmc/trace_2_26072024.txt")
trace3 <- fread("data/trace_data/mcmc/trace_3_26072024.txt")
trace4 <- fread("data/trace_data/mcmc/trace_4_26072024.txt")

my_trace <- mcmc(my_trace)
trace1 <- mcmc(trace1)
trace2 <- mcmc(trace2)
trace3 <- mcmc(trace3)
trace4 <- mcmc(trace4)

my_trace_df <- data.frame(my_trace)
setDT(my_trace_df)
my_trace_df[, season_amp := season_amp / 100]
my_trace_df[, season_offset := season_offset / 100]
my_trace_df[, probT_under5 := exp(probT_under5)]
my_trace_df[, probT_over5 := exp(probT_over5)]
my_trace_df[, aki_hospitalisation_4 := exp(aki_hospitalisation_4)]
my_trace_df[, gastro_hospitalisation_4 := exp(gastro_hospitalisation_4)]
my_trace_df[, gastro_gp_attend_1 := exp(gastro_gp_attend_1)]
my_trace_df[, gastro_gp_attend_2 := exp(gastro_gp_attend_2)]
tail(my_trace_df)
params_trace <- mcmc(my_trace_df[,1:15])
log_density_trace <- mcmc(my_trace_df[,16])

trace1_params <- mcmc(trace1[,1:15])
trace1_log_density <- mcmc(trace1[,16])
trace2_params <- mcmc(trace2[,1:15])
trace2_log_density <- mcmc(trace1[,16])
trace3_params <- mcmc(trace3[,1:15])
trace3_log_density <- mcmc(trace3[,16])
trace4_params <- mcmc(trace4[,1:15])
trace4_log_density <- mcmc(trace4[,16])

multi_trace_params <- mcmc.list(list(trace1_params, trace2_params, trace3_params, trace4_params))
multi_trace_log_density <- mcmc.list(list(trace1_log_density, trace2_log_density, trace3_log_density, trace4_log_density))

rmarkdown::render("R/06_mcmc_outputs.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("mcmc_outputs", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))
