
source("R/01_setup.R")

source("R/02_observation_data.R")

source("R/03_model_function.R")

source("R/04_likelihood_prior_functions.R")
# source("R/04_likelihood_prior_functions_negbin.R")
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

starting.value <-
  c(
    sigma = 0.78, 
    surveillance_report_1 = 0.002,
    surveillance_report_3 = 0.001,
    surveillance_report_4_summer = 0.0035, 
    surveillance_report_4_winter = 0.026, 
    season_amp = 2.5, 
    season_offset = 15, 
    aki_hospitalisation_4_winter = log(0.2), #log(0.05),
    gastro_hospitalisation_4_winter = log(0.1), #log(0.05),
    gastro_gp_attend_1 = log(0.4),
    D_immun = 8,
    probT_under5 = log(0.22),
    probT_over5 = log(0.04)
  )

# diagonal elements of the covariance matrix for the Gaussian proposal

prop.sd <-
  c(
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

# lower and upper limits of each parameter
lower <- c(
  sigma = 0.6,
  surveillance_report_1 = 0,
  surveillance_report_3 = 0,
  surveillance_report_4_summer = 0,
  surveillance_report_4_winter = 0,
  season_amp = 0,
  season_offset = 0,
  aki_hospitalisation_4_winter = -Inf, # bound so that transformed value is no lower than 0
  gastro_hospitalisation_4_winter = -Inf,  # bound so that transformed value is no lower than 0
  gastro_gp_attend_1 = -Inf,  # bound so that transformed value is no lower than 0
  D_immun = 0.5,
  probT_under5 = log(0.0001),  # bound so that transformed value is no lower than 0
  probT_over5 = log(0.0001) # bound so that transformed value is no lower than 0
)

upper <- c(
  sigma = 0.9,
  surveillance_report_1 = 0.06,
  surveillance_report_3 = 0.06,
  surveillance_report_4_summer = 0.1,
  surveillance_report_4_winter = 0.1,
  season_amp = 10,
  season_offset = 50,
  aki_hospitalisation_4_winter = log(0.75), # bound so that transformed value is no higher than 0.5
  gastro_hospitalisation_4_winter = log(0.5), # bound so that transformed value is no higher than 0.4
  gastro_gp_attend_1 = log(0.5), # bound so that transformed value is no higher than 0.5
  D_immun = 12,
  probT_under5 = log(0.5), # bound so that transformed value is no higher than 0.44
  probT_over5 = log(0.12) # bound so that transformed value is no higher than 0.114
)

# additional parameters for the adaptive MCMC, see ?mcmcMh for more details
adaptSizeStart <- 500
adaptSizeCooling <- 0.99
adaptShapeStart <- 500

# sensitivity analysis

iter = 100000

knots_number = 3

set.seed(123)

burn_value <- 50000

thin_factor <- 10

source("R/04_likelihood_prior_functions.R")
source("R/04_likelihood_prior_functions_quasipoisson.R")

# limit AKI defintiion to primary and secondary codes within 0-2 days

parameters_posteriror_function <- function(theta) {
  
  return(log_posterior_sum_quasi_pois(parameters = c(theta, par),
                                     init.state = init.state,
                                     times = times,
                                     data = observation_data_sensitivity_primary_secondary_aki,
                                     age_data = age_incidence))
}

starting.value_primary_aki <-
  c(
    sigma = 0.78, 
    surveillance_report_1 = 0.002,
    surveillance_report_3 = 0.001,
    surveillance_report_4_summer = 0.0035, 
    surveillance_report_4_winter = 0.026, 
    season_amp = 2.5, 
    season_offset = 15, 
    aki_hospitalisation_4_winter = log(0.1), #log(0.05),
    gastro_hospitalisation_4_winter = log(0.1), #log(0.05),
    gastro_gp_attend_1 = log(0.4),
    D_immun = 8,
    probT_under5 = log(0.22),
    probT_over5 = log(0.04)
  )

lower <- c(
  sigma = 0.6,
  surveillance_report_1 = 0,
  surveillance_report_3 = 0,
  surveillance_report_4_summer = 0,
  surveillance_report_4_winter = 0,
  season_amp = 0,
  season_offset = 0,
  aki_hospitalisation_4_winter = log(0.001), # bound so that transformed value is no lower than 0
  gastro_hospitalisation_4_winter = -Inf,  # bound so that transformed value is no lower than 0
  gastro_gp_attend_1 = -Inf,  # bound so that transformed value is no lower than 0
  D_immun = 0.5,
  probT_under5 = log(0.0001),  # bound so that transformed value is no lower than 0
  probT_over5 = log(0.0001) # bound so that transformed value is no lower than 0
)

mcmc_trace_sen_aki_primary_secondary <-
  mcmcMh(
    target = parameters_posteriror_function,
    initTheta = starting.value_primary_aki,
    limits = list(lower = lower, upper = upper),
    proposalSd = prop.sd,
    nIterations = iter,
    adaptSizeStart = adaptSizeStart,
    adaptShapeStart = adaptShapeStart,
    adaptSizeCooling = adaptSizeCooling,
    maxScalingSd = 3,
    verbose = FALSE
  )

my_trace_sen_aki_primary_secondary <- mcmc(mcmc_trace_sen_aki_primary_secondary$trace)
xyplot(x = my_trace_sen_aki_primary_secondary)
tail(my_trace_sen_aki_primary_secondary)

saveRDS(my_trace_sen_aki_primary_secondary, "results/trace_data/my_trace_sen_aki_primary_secondary.rds")

my_trace <- my_trace_sen_aki_primary_secondary

# extended when AKI is definied (greater)

parameters_posteriror_function <- function(theta) {
  
  return(log_posterior_sum_quasi_pois(parameters = c(theta, par),
                                     init.state = init.state,
                                     times = times,
                                     data = observation_data_sensitivity_any_time,
                                     age_data = age_incidence))
}

mcmc_trace_sen_aki_any <-
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

my_trace_sen_aki_any <- mcmc(mcmc_trace_sen_aki_any$trace)
xyplot(x = my_trace_sen_aki_any)

saveRDS(mcmc_trace_sen_aki_any, "results/trace_data/my_trace_sen_aki_any.rds")

my_trace <- my_trace_sen_aki_any

# reduce B-spline to 3 knots

source("R/04_likelihood_prior_functions.R")
source("R/04_likelihood_prior_functions_quasipoisson.R")

knots_number = 1

mcmc_trace_knots1 <-
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

my_trace_knots1 <- mcmc(mcmc_trace_knots1$trace)
xyplot(x = my_trace_knots1)

saveRDS(mcmc_trace_knots1, "results/trace_data/mcmc_trace_knots1.rds")

my_trace <- my_trace_knots1


####

knots_number = 2

mcmc_trace_knots2 <-
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

my_trace_knots2 <- mcmc(mcmc_trace_knots2$trace)
xyplot(x = my_trace_knots2)

saveRDS(mcmc_trace_knots2, "results/trace_data/mcmc_trace_knots2.rds")

####

# Load sensitivity analysis traces

my_trace_sen_aki_primary_secondary <- readRDS("results/trace_data/my_trace_sen_aki_primary_secondary.rds")
my_trace_sen_aki_any <- readRDS("results/trace_data/my_trace_sen_aki_any.rds")
my_trace_knots1 <- readRDS("results/trace_data/mcmc_trace_knots1.rds")
my_trace_knots2 <- readRDS("results/trace_data/mcmc_trace_knots2.rds")

# burn and thin traces

my_trace_sen_aki_any <- mcmc(my_trace_sen_aki_any$trace)
my_trace_knots1 <- mcmc(my_trace_knots1$trace)
my_trace_knots2 <- mcmc(my_trace_knots2$trace)

my_trace_sen_aki_primary_secondary <- burnAndThin(my_trace_sen_aki_primary_secondary, burn = burn_value, thin = thin_factor)
my_trace_sen_aki_any <- burnAndThin(my_trace_sen_aki_any, burn = burn_value, thin = thin_factor)
my_trace_knots1 <- burnAndThin(my_trace_knots1, burn = burn_value, thin = thin_factor)
my_trace_knots2 <- burnAndThin(my_trace_knots2, burn = burn_value, thin = thin_factor)

# Function to create posterior tables

create_posterior_tables <- function(trace_list, parameter_labels = NULL, digits = 2) {

  # Process each trace and create a posterior table
  tables_list <- list()
  
  for (trace_name in names(trace_list)) {
    # Convert to mcmc object
    if (!inherits(trace_list[[trace_name]], "mcmc")) {
      if (is.list(trace_list[[trace_name]]) && "trace" %in% names(trace_list[[trace_name]])) {
        trace <- coda::mcmc(trace_list[[trace_name]]$trace)
      } else {
        trace <- coda::mcmc(trace_list[[trace_name]])
      }
    } else {
      trace <- trace_list[[trace_name]]
    }
    
    # Get summary statistics
    summary_stats <- summary(trace)
    median_mcmc <- summary_stats$quantiles[, 3]
    q2.5 <- summary_stats$quantiles[, 1]
    q97.5 <- summary_stats$quantiles[, 5]
    
    # Create posterior table
    posterior_table <- data.frame(
      theta = median_mcmc,
      q2.5 = q2.5,
      q97.5 = q97.5
    )
    
    # Add parameter names if provided for this trace
    if (!is.null(parameter_labels) && trace_name %in% names(parameter_labels)) {
      rownames(posterior_table) <- parameter_labels[[trace_name]]
    }
    
    # Convert to tibble and add Parameter column
    posterior_table <- as_tibble(posterior_table) %>%
      mutate(Parameter = rownames(posterior_table)) %>%
      # Add trace name to identify the source
      mutate(Trace = trace_name)
    
    tables_list[[trace_name]] <- posterior_table
  }
  
  combined_table <- bind_rows(tables_list)
  
  # apply transformation
  transformed_table <- combined_table %>%
    mutate(across(c(theta, q2.5, q97.5), ~ 
                    case_when(
                      Parameter == "season_amp" ~ ./100,
                      Parameter == "season_offset" ~ ./100,
                      Parameter %in% c("probT_under5", "probT_over5", 
                                       "aki_hospitalisation_4_winter", 
                                       "gastro_hospitalisation_4_winter", 
                                       "gastro_gp_attend_1") ~ exp(.),
                      TRUE ~ .
                    ))) %>%
    select(Trace, Parameter, theta, q2.5, q97.5)
  
  presentation_table <- transformed_table %>%
    mutate(
      theta = signif(theta, digits = digits),
      q2.5 = signif(q2.5, digits = digits),
      q97.5 = signif(q97.5, digits = digits),
      cri = paste0(q2.5, "-", q97.5),
      "Median (95% CrI)" = paste0(theta, " ", paste0("(", cri, ")"))
    ) %>%
    select(Trace, Parameter, `Median (95% CrI)`)
  
  pivoted_table <- presentation_table %>%
    pivot_wider(
      id_cols = Parameter,
      names_from = Trace,
      values_from = `Median (95% CrI)`
    )
  
  return(list(
    data_table = transformed_table,
    presentation_table = presentation_table,
    pivoted_table = pivoted_table
  ))
}

trace_list <- list(
  primary_secondary = my_trace_sen_aki_primary_secondary,
  any = my_trace_sen_aki_any,
  knots1 = my_trace_knots1,
  knots2 = my_trace_knots2
)

parameter_labels <- list(
  primary_secondary = c(
    "sigma",
    "surveillance_report_1",
    "surveillance_report_3",
    "surveillance_report_4_summer",
    "surveillance_report_4_winter",
    "season_amp",
    "season_offset",
    "aki_hospitalisation_4_winter",
    "gastro_hospitalisation_4_winter",
    "gastro_gp_attend_1",
    "D_immun",
    "probT_under5",
    "probT_over5",
    "logDensity"
  )
)

result <- create_posterior_tables(trace_list, parameter_labels)
data_table <- result$data_table
presentation_table <- result$presentation_table
pivoted_table <- result$pivoted_table

# Order by specific parameter sequence
parameter_order <- c(
  "sigma",
  "D_immun",
  "probT_under5",
  "probT_over5",
  "season_amp",
  "season_offset",
  "aki_hospitalisation_4_winter",
  "gastro_hospitalisation_4_winter",
  "surveillance_report_1",
  "surveillance_report_3",
  "surveillance_report_4_winter",
  "surveillance_report_4_summer",
  "gastro_gp_attend_1"
)

# Apply ordering to the pivoted table
sensitivity_analysis_parameter_table <- pivoted_table %>%
  mutate(Parameter = factor(Parameter, levels = parameter_order)) %>%
  arrange(Parameter) |> 
  filter(Parameter != "logDensity") |> 
  mutate(Parameter = case_when(
    Parameter == "surveillance_report_1" ~ "Underreporting to surveillance 0-4",
    Parameter == "surveillance_report_3" ~ "Underreporting to surveillance 15-64",
    Parameter == "surveillance_report_4_winter" ~ "Underreporting to surveillance 65+ in the winter",
    Parameter == "surveillance_report_4_summer" ~ "Underreporting to surveillance 65+ in the summer",
    Parameter == "season_amp" ~ "Seasonal amplitude term",
    Parameter == "season_offset" ~ "Seasonal offset term",
    Parameter == "probT_under5" ~ "Probability of infection between under 5s",
    Parameter == "probT_over5" ~ "Probability of infection to over 5s",
    Parameter == "aki_hospitalisation_4_winter" ~ "Norovirus associated AKI hospitalisation in 65+ in the winter",
    Parameter == "gastro_hospitalisation_4_winter" ~ "Norovirus associated hospitalisation in 65+ in the winter",
    Parameter == "gastro_gp_attend_1" ~ "GP attendance for all cause gastroenteritis in under 5s",
    Parameter == "D_immun" ~ "Duration of immunity",
    Parameter == "sigma" ~ "Proportion symptomatic"
  )) |> 
  rename(
    "AKI definition - AKI code in primary and secondary position only" = primary_secondary,
    "AKI definition - AKI code any positon and any time during admission" = any,
    "AKI definition 1 knot" = knots1,
    "AKI definition 2 knots" = knots2
  ) |>
  flextable::flextable()  |>    
  padding(padding = 1.5, part = "all") |>  
  fontsize(size = 10, part = "all")  |> 
  width(j = 1, width = 3.7) |>  
  width(j = 2, width = 3.0) |>  
  width(j = 3, width = 3.0) |> 
  width(j = 4, width = 3.0) |> 
  width(j = 5, width = 3.0) |> 
  height_all(height = 0.4) |> 
  hrule(rule = "exact") |>
  theme_zebra() |> 
  hline_bottom(part = "body") |> 
  hline_top(part = "header") 


save_as_image(sensitivity_analysis_parameter_table, path = "figures/sensitivity_analysis_parameter_table.png", bg = "white")

xyplot( x = my_trace_sen_aki_primary_secondary)
xyplot( x = my_trace_sen_aki_any)
xyplot( x = my_trace_knots1)
xyplot( x = my_trace_knots2)

