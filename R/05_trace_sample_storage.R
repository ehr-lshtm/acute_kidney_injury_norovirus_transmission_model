# set up for using mcmc sampler

source("R/01_setup.R")

source("R/02_observation_data.R")

source("R/03_model_function.R")

source("R/04_likelihood_prior_functions.R")
source("R/04_likelihood_prior_functions_negbin.R")
source("R/04_likelihood_prior_functions_quasipoisson.R")

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
  aging = aging,
  season_amp_over65 = 1
  )

par[["contacts"]] <- uk_contact_rate_matrix
par[["season_change_points"]] <- c(11000, 0, 0, 0, 0, 0, 0)

init.state <- init_matrix

times <- 11000

knots_number = 3

# initial thetas

# starting.value <-
#   c(
#     season_amp_over65 = 1.0,
#     sigma = 0.78, 
#     surveillance_report_1 = 0.002,
#     surveillance_report_2 = 0.0004,
#     surveillance_report_3 = 0.00084, 
#     season_amp = 2.5, 
#     season_offset = 15, 
#     aki_hospitalisation_4 = log(0.4), 
#     gastro_hospitalisation_4 = log(0.05), 
#     gastro_gp_attend_1 = log(0.4),
#     gastro_gp_attend_2 = log(0.35), 
#     D_immun = 8.0,
#     probT_under5 = log(0.22), 
#     probT_over5 = log(0.04)
#     )

starting.value <-
  c(
    sigma = 0.78, 
    surveillance_report_1_summer = 0.002,
    surveillance_report_1_winter = 0.002,
    surveillance_report_2_summer = 0.0004,
    surveillance_report_2_winter = 0.0004,
    surveillance_report_3_summer = 0.00084, 
    surveillance_report_3_winter = 0.00084, 
    surveillance_report_4_summer = 0.0035, 
    surveillance_report_4_winter = 0.026, 
    season_amp = 2.5, 
    season_offset = 15, 
    aki_hospitalisation_4 = log(0.05), 
    gastro_hospitalisation_4 = log(0.05), 
    gastro_gp_attend_1 = log(0.4),
    gastro_gp_attend_2 = log(0.35), 
    D_immun = 8.0,
    probT_under5 = log(0.22), 
    probT_over5 = log(0.04)
  )

# diagonal elements of the covariance matrix for the Gaussian proposal
                 
prop.sd <-
  c(
    # season_amp_over65 = 0.01, 
    sigma = 0.0015,
    surveillance_report_1_summer = 0.00002,
    surveillance_report_1_winter = 0.00002,
    surveillance_report_2_summer = 0.00001,
    surveillance_report_2_winter = 0.00001,
    surveillance_report_3_summer = 0.00001,
    surveillance_report_3_winter = 0.00001,
    surveillance_report_4_summer = 0.00002,
    surveillance_report_4_winter = 0.00002,
    season_amp = 0.02,
    season_offset = 0.1, 
    aki_hospitalisation_4 = 0.095, 
    gastro_hospitalisation_4 = 0.095, 
    gastro_gp_attend_1 = 0.005, 
    gastro_gp_attend_2 = 0.002, 
    D_immun = 0.15, 
    probT_under5 = 0.012, 
    probT_over5 = 0.015
  )

# lower and upper limits of each parameter
lower <- c(
  # season_amp_over65 = 0,
  sigma = 0.6,
  surveillance_report_1_summer = 0,
  surveillance_report_1_winter = 0,
  surveillance_report_2_summer = 0,
  surveillance_report_2_winter = 0,
  surveillance_report_3_summer = 0,
  surveillance_report_3_winter = 0,
  surveillance_report_4_summer = 0,
  surveillance_report_4_winter = 0,
  season_amp = 0,
  season_offset = 0,
  aki_hospitalisation_4 = log(0.0001), # bound so that transformed value is no lower than 0
  gastro_hospitalisation_4 = log(0.0001),  # bound so that transformed value is no lower than 0
  gastro_gp_attend_1 = log(0.0001),  # bound so that transformed value is no lower than 0
  gastro_gp_attend_2 = log(0.0001),  # bound so that transformed value is no lower than 0
  D_immun = 0.5,
  probT_under5 = log(0.0001),  # bound so that transformed value is no lower than 0
  probT_over5 = log(0.0001) # bound so that transformed value is no lower than 0
)

upper <- c(
  # season_amp_over65 = 10,
  sigma = 0.9,
  surveillance_report_1_summer = 0.01,
  surveillance_report_1_winter = 0.02,
  surveillance_report_2_summer = 0.01,
  surveillance_report_2_winter = 0.02,
  surveillance_report_3_summer = 0.01,
  surveillance_report_3_winter = 0.02,
  surveillance_report_4_summer = 0.02,
  surveillance_report_4_winter = 0.06,
  season_amp = 10,
  season_offset = 50,
  aki_hospitalisation_4 = log(0.75), # bound so that transformed value is no higher than 0.5
  gastro_hospitalisation_4 = log(0.5), # bound so that transformed value is no higher than 0.4
  gastro_gp_attend_1 = log(0.5), # bound so that transformed value is no higher than 0.5
  gastro_gp_attend_2 = log(0.5), # bound so that transformed value is no higher than 0.5
  D_immun = 14,
  probT_under5 = log(0.44), # bound so that transformed value is no higher than 0.44
  probT_over5 = log(0.114) # bound so that transformed value is no higher than 0.114
)

# additional parameters for the adaptive MCMC, see ?mcmcMh for more details
adaptSizeStart <- 500
adaptSizeCooling <- 0.99
adaptShapeStart <- 500

# number of iterations for the MCMC
iter <- 5000

# set.seed(1234)

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
    maxScalingSd = 3,
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
tail(my_trace)

params_trace <- mcmc(my_trace_df[,1:15])
log_density_trace <- mcmc(my_trace_df[,16])

rmarkdown::render("R/06_mcmc_outputs.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("mcmc_outputs", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

# Save
saveRDS(my_trace, "my_trace_wiht_differential_reporting.rds")

# Load
load_my_trace <- readRDS("my_trace.rds")

# load_my_trace <- fread("Z:/GPRD_GOLD/Hikaru/mcmc/my_trace_19072024.txt")

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
