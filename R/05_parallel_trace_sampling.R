# Set up parallel processing
library(future)
library(progressr)

start_time <- Sys.time()

start_time

plan(multisession, workers = 4)

start_time <- Sys.time()

with_progress({
  p <- progressor(steps = 4)
  multiple_chains <- furrr::future_imap(
    seq_len(4),
    ~ {
      # Source all required files
      source("R/01_setup.R")
      source("R/02_observation_data.R")  # This probably creates observation_data
      source("R/03_model_function.R")
      source("R/04_likelihood_prior_functions.R")
      source("R/04_likelihood_prior_functions_quasipoisson.R")
      source("R/fixed_parameters.R")
      
      # Set up all parameters
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
      
      # Function to generate random starting values
      generate_random_starts <- function() {
        c(
          sigma = runif(1, 0.75, 0.8),
          surveillance_report_1 = runif(1, 0.001, 0.003),
          surveillance_report_3 = runif(1, 0.001, 0.0015),
          surveillance_report_4_summer = runif(1, 0.003, 0.004),
          surveillance_report_4_winter = runif(1, 0.02, 0.03),
          season_amp = runif(1, 2.0, 3.0),
          season_offset = runif(1, 10, 20),
          aki_hospitalisation_4_winter = runif(1, log(0.15), log(0.25)),
          gastro_hospitalisation_4_winter = runif(1, log(0.08), log(0.12)),
          gastro_gp_attend_1 = runif(1, log(0.38), log(0.42)),
          D_immun = runif(1, 7, 9),
          probT_under5 = runif(1, log(0.19), log(0.23)),
          probT_over5 = runif(1, log(0.035), log(0.042))
        )
      }
      
      # Generate starting values
      set.seed(1234 + .x)  # Different seed for each chain
      starting_values <- replicate(4, generate_random_starts(), simplify = FALSE)
      
      # Set up other parameters
      prop.sd <- c(
        sigma = 0.0015,
        surveillance_report_1 = 0.00003,
        surveillance_report_3 = 0.00003,
        surveillance_report_4_summer = 0.00003,
        surveillance_report_4_winter = 0.00003,
        season_amp = 0.02,
        season_offset = 0.1, 
        aki_hospitalisation_4_winter = 0.095,
        gastro_hospitalisation_4_winter = 0.095,
        gastro_gp_attend_1 = 0.005, 
        D_immun = 0.15, 
        probT_under5 = 0.012,
        probT_over5 = 0.015
      )
      
      lower <- c(
        sigma = 0.6,
        surveillance_report_1 = 0,
        surveillance_report_3 = 0,
        surveillance_report_4_summer = 0,
        surveillance_report_4_winter = 0,
        season_amp = 0,
        season_offset = 0,
        aki_hospitalisation_4_winter = -Inf,
        gastro_hospitalisation_4_winter = -Inf,
        gastro_gp_attend_1 = -Inf,
        D_immun = 0.5,
        probT_under5 = log(0.0001), 
        probT_over5 = log(0.0001)
      )
      
      upper <- c(
        sigma = 0.9,
        surveillance_report_1 = 0.06,
        surveillance_report_3 = 0.06,
        surveillance_report_4_summer = 0.1,
        surveillance_report_4_winter = 0.1,
        season_amp = 10,
        season_offset = 50,
        aki_hospitalisation_4_winter = log(0.75), 
        gastro_hospitalisation_4_winter = log(0.5),
        gastro_gp_attend_1 = log(0.5),
        D_immun = 12,
        probT_under5 = log(0.5),
        probT_over5 = log(0.12)
      )
      
      adaptSizeStart <- 500
      adaptSizeCooling <- 0.99
      adaptShapeStart <- 500
      iter <- 500000
      
      # Run MCMC
      result <- mcmcMh(
        target = parameters_posteriror_function,
        initTheta = starting_values[[.x]],
        limits = list(lower = lower, upper = upper),
        proposalSd = prop.sd,
        nIterations = iter,
        adaptSizeStart = adaptSizeStart,
        adaptShapeStart = adaptShapeStart,
        adaptSizeCooling = adaptSizeCooling,
        maxScalingSd = 3,
        verbose = FALSE
      )
      p()
      result
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
})

end_time <- Sys.time()
print(end_time - start_time)

plan(sequential)

#####################################

# Save the trace data
trace1 <- mcmc(multiple_chains[[1]]$trace)
trace2 <- mcmc(multiple_chains[[2]]$trace)
trace3 <- mcmc(multiple_chains[[3]]$trace)
trace4 <- mcmc(multiple_chains[[4]]$trace)

trace1 |>
  as.data.frame() |>
  write_tsv("results/trace_data/trace_1_26122024.txt")

trace2 |>
  as.data.frame() |>
  write_tsv("results/trace_data/trace_2_26122024.txt")

trace3 |>
  as.data.frame() |>
  write_tsv("results/trace_data/trace_3_26122024.txt")

trace4 |>
  as.data.frame() |>
  write_tsv("results/trace_data/trace_4_26122024.txt")

# trace5 |>
#  as.data.frame() |>
#  write_tsv("results/trace_data/trace_5_19122024.txt")

######
# store trace values
######

trace1 <- fread(("results/trace_data/trace_1_26122024.txt"))
trace2 <- fread(("results/trace_data/trace_2_26122024.txt"))
trace3 <- fread(("results/trace_data/trace_3_26122024.txt"))
trace4 <- fread(("results/trace_data/trace_4_26122024.txt"))

trace1_params <- mcmc(trace1[,1:13])
trace1_log_density <- mcmc(trace1[,14])

trace2_params <- mcmc(trace2[,1:13])
trace2_log_density <- mcmc(trace1[,14])

trace3_params <- mcmc(trace3[,1:13])
trace3_log_density <- mcmc(trace3[,14])

trace4_params <- mcmc(trace4[,1:13])
trace4_log_density <- mcmc(trace4[,14])

multi_trace_params <- mcmc.list(list(trace1_params, trace2_params, trace3_params, trace4_params))
multi_trace_log_density <- mcmc.list(list(trace1_log_density, trace2_log_density, trace3_log_density, trace4_log_density))

my_trace <- trace3
my_trace <- mcmc(my_trace)

xyplot(x = my_trace)

my_trace_df <- data.frame(my_trace)
setDT(my_trace_df)

my_trace_df[, season_amp := season_amp / 100]
my_trace_df[, season_offset := season_offset / 100]
my_trace_df[, probT_under5 := exp(probT_under5)]
my_trace_df[, probT_over5 := exp(probT_over5)]
my_trace_df[, aki_hospitalisation_4_winter := exp(aki_hospitalisation_4_winter)]
my_trace_df[, gastro_hospitalisation_4_winter := exp(gastro_hospitalisation_4_winter)]
my_trace_df[, gastro_gp_attend_1 := exp(gastro_gp_attend_1)]

tail(my_trace_df)
tail(my_trace)

params_trace <- mcmc(my_trace_df[,1:13])
log_density_trace <- mcmc(my_trace_df[,14])

rmarkdown::render("R/06_mcmc_outputs.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("mcmc_outputs", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))
