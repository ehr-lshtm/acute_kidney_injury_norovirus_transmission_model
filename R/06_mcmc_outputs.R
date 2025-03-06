#' ---
#' title: MCMC outputs
#' date: "`r format(Sys.time(), '%d %B %Y %H:%M')`"
#' author: Hikaru Bolt
#' output:
#'   html_document:
#'     df_print: paged
#'     highlight: kate
#'     theme: spacelab
#'     toc: yes
#'     toc_float: yes
#'     fig_height: 14
#'     fig_width: 16
#'     mathjax: null
#' ---
#'
#'
# To run this
# rmarkdown::render("R/06_mcmc_outputs.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("mcmc_outputs", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE)


#+ data

#' ## Preliminary run

#starting_values[[2]]

#prop.sd

#' ## Covariance matrix

# mcmc_trace$covmatEmpirical

#' ## Trace plots

xyplot( x = params_trace)

#' #### logDensity

xyplot( x = log_density_trace)

#' ## acceptance rate

1 - rejectionRate(my_trace)

#' ## effective sample size

plotEssBurn(my_trace)

#' ## trace without burn in

burn_value <- 100000

traceBurn <- burnAndThin(my_trace, burn = burn_value)

traceBurn_df <- burnAndThin(my_trace_df, burn = burn_value)
traceBurn_params <- burnAndThin(params_trace, burn = burn_value)
traceBurn_log_density <- mcmc(traceBurn[,14])

effectiveSize(traceBurn)

xyplot( x = traceBurn)

plotEssBurn(traceBurn_params)

#' #### logDensity

xyplot( x = traceBurn_log_density)

#' ## acceptance rate

1 - rejectionRate(traceBurn)

#' ## autocorrelation plot 

acfplot(x = traceBurn, lag.max = 60)

#' ## thinned trace

thin_factor <- 100

traceBurnThin <- burnAndThin(my_trace, burn = burn_value, thin = thin_factor)
traceBurnThin_df <- burnAndThin(my_trace_df, burn = burn_value, thin = thin_factor)
traceBurnThin_params <- burnAndThin(params_trace, burn = burn_value, thin = thin_factor)
traceBurnThin_log_density <- mcmc(traceBurnThin[,14])

effectiveSize(traceBurnThin)

xyplot( x = traceBurnThin)

#' #### logDensity

xyplot( x = traceBurnThin_log_density)

acfplot(x = traceBurnThin, lag.max = 60)

#' ## comparing thinned and unthinned trace

plotPosteriorDensity(list(unthinned = traceBurn_df, thinned = traceBurnThin_df))

# traceBurn_df_aki <- traceBurn_df |> 
#   select(aki_hospitalisation_4) |> 
#   rename('Norovirus linked AKI hospitalisation' = aki_hospitalisation_4)
# 
# traceBurnThin_df_aki <- traceBurnThin_df |> 
#   select(aki_hospitalisation_4) |> 
#   rename('Norovirus linked AKI hospitalisation' = aki_hospitalisation_4)

source("R/create_prior_distributions.R")

plotPosteriorDensity(trace = traceBurnThin_df, prior = prior_df)

#' ## correlation

levelplot(traceBurnThin, col.regions = heat.colors(100), scales=list(x=list(rot=90)))

# mcmc_pairs(
#   traceBurnThin_df,
#   pars = c(
#     "sigma",
#     "season_amp",
#     "season_offset",
#     "D_immun",
#     "probT_under5",
#     "probT_over5",
#     "surveillance_report_1",
#     "surveillance_report_2",
#     "surveillance_report_3",
#     "surveillance_report_4_winter",
#     "surveillance_report_4_summer",
#     "aki_hospitalisation_4",
#     "gastro_hospitalisation_4",
#     "gastro_gp_attend_1",
#     "gastro_gp_attend_2"
#   ),
#   diag_fun = "dens",
#   off_diag_fun = "hex",
#   off_diag_args = list(size = 1, alpha = 0.5)
# )

# simulate model - assess visual fit

summary_stats<-summary(traceBurnThin)
# summary_stats<-summary(my_trace)

mean_mcmc<-summary_stats$statistics[,1]
median_mcmc<-summary_stats$quantiles[,3]
q2.5<-summary_stats$quantiles[,1]
q97.5<-summary_stats$quantiles[,5]
theta <- median_mcmc

posterior_table <- data.frame(theta = theta,
                              q2.5 = q2.5,
                              q97.5 = q97.5)
rownames(posterior_table) <- c(
  # "season_amp_over65",
  "sigma",
  "surveillance_report_1",
  # "surveillance_report_1_summer",
  # "surveillance_report_1_winter",
  # "surveillance_report_2",
  # "surveillance_report_2_summer",
  # "surveillance_report_2_winter",
  "surveillance_report_3",
  # "surveillance_report_3_summer",
  # "surveillance_report_3_winter",
  "surveillance_report_4_summer",
  "surveillance_report_4_winter",
  "season_amp",
  "season_offset",
  # "aki_hospitalisation_4_summer",
  "aki_hospitalisation_4_winter",
  # "gastro_hospitalisation_4_summer",
  "gastro_hospitalisation_4_winter",
  "gastro_gp_attend_1",
  # "gastro_gp_attend_2",
  "D_immun",
  "probT_under5",
  "probT_over5",
  "logDensity"
)

posterior_table <- as_tibble(posterior_table)  |> 
  mutate(Parameter = rownames(posterior_table))

posterior_table <- posterior_table %>%
  mutate(across(-Parameter, ~ 
                  case_when(
                    Parameter == "season_amp" ~ ./100,
                    Parameter == "season_offset" ~ ./100,
                    Parameter == "probT_under5" ~ exp(.),
                    Parameter == "probT_over5" ~ exp(.),
                    Parameter == "aki_hospitalisation_4_winter" ~ exp(.),
                    # Parameter == "aki_hospitalisation_4_summer" ~ exp(.),
                    Parameter == "gastro_hospitalisation_4_winter" ~ exp(.),
                    # Parameter == "gastro_hospitalisation_4_summer" ~ exp(.),
                    Parameter == "gastro_gp_attend_1" ~ exp(.),
                    # Parameter == "gastro_gp_attend_2" ~ exp(.),
                    TRUE ~ .
                  ))) |> 
  select(Parameter, theta, q2.5, q97.5)

parameter_order <- c(
  "sigma",
  "D_immun",
  "probT_under5",
  "probT_over5",
  "season_amp",
  # "season_amp_over65",
  "season_offset",
  "aki_hospitalisation_4_winter",
  # "aki_hospitalisation_4_summer",
  "gastro_hospitalisation_4_winter",
  # "gastro_hospitalisation_4_summer",
  "surveillance_report_1",
  # "surveillance_report_1_winter",
  # "surveillance_report_1_summer",
  # "surveillance_report_2",
  # "surveillance_report_2_winter",
  # "surveillance_report_2_summer",
  "surveillance_report_3",
  # "surveillance_report_3_winter",
  # "surveillance_report_3_summer",
  "surveillance_report_4_winter",
  "surveillance_report_4_summer",
  "gastro_gp_attend_1"
  # "gastro_gp_attend_2"
)

posterior_table_image <- posterior_table |> 
  mutate(theta = signif(theta, digits = 3),
         q2.5 = signif(q2.5, digits = 3),
         q97.5 = signif(q97.5, digits = 3),
         cri = paste0(q2.5, "-", q97.5),
         "Median (95% CrI)" = paste0(theta, " ", paste0("(", cri, ")"))
         ) |>
  filter(!row_number() %in% c(14)) |> 
  select(Parameter, `Median (95% CrI)` ) |>
  arrange(factor(Parameter, levels = parameter_order)) |> 
  mutate(Explanation = case_when(
    Parameter == "surveillance_report_1" ~ "Proportion of symptomatic norovirus in 0-4 year olds reported to surveillance",
    # Parameter == "surveillance_report_1_winter" ~ "Proportion of symptomatic norovirus in 0-4 year olds reported to surveillance in the winter",
    # Parameter == "surveillance_report_1_summer" ~ "Proportion of symptomatic norovirus in 0-4 year olds reported to surveillance in the summer",
    # Parameter == "surveillance_report_2" ~ "Proportion of symptomatic norovirus in 5-14 year olds reported to surveillance",
    # Parameter == "surveillance_report_2_winter" ~ "Proportion of symptomatic norovirus in 5-14 year olds reported to surveillance in the winter",
    # Parameter == "surveillance_report_2_summer" ~ "Proportion of symptomatic norovirus in 5-14 year olds reported to surveillance in the summer",
    Parameter == "surveillance_report_3" ~ "Proportion of symptomatic norovirus in 15-64 year olds reported to surveillance",
    # Parameter == "surveillance_report_3_winter" ~ "Proportion of symptomatic norovirus in 15-64 year olds reported to surveillance in the winter",
    # Parameter == "surveillance_report_3_summer" ~ "Proportion of symptomatic norovirus in 15-64 year olds reported to surveillance in the summer",
    Parameter == "surveillance_report_4_winter" ~ "Proportion of symptomatic norovirus in over 65s reported to surveillance in the winter",
    Parameter == "surveillance_report_4_summer" ~ "Proportion of symptomatic norovirus in over 65s reported to surveillance in the summer",
    Parameter == "season_amp" ~ "A term forcing the amplitude of the periodicity in the contact rate",
    Parameter == "season_offset" ~ "A term forcing the timing of the periodicity in the contact rate",
    # Parameter == "season_amp_over65" ~ "Scaling seasonal amplitude for over 65 seasonality to improve fit",
    Parameter == "probT_under5" ~ "Probability of transmission in under 5s transmitting to under 5s",
    Parameter == "probT_over5" ~ "Probability of transmission transmitting to over 5s",
    Parameter == "aki_hospitalisation_4_winter" ~ "Proportion of symptomatic norovirus infections linked to an AKI hospitalisation in over 65s in the winter",
    # Parameter == "aki_hospitalisation_4_summer" ~ "Proportion of symptomatic norovirus infections linked to an AKI hospitalisation in over 65s in the summer",
    Parameter == "gastro_hospitalisation_4_winter" ~ "Proportion of symptomatic norovirus infections linked to a gastroenteritis hospitalisation in over 65s in the winter",
    # Parameter == "gastro_hospitalisation_4_summer" ~ "Proportion of symptomatic norovirus infections linked to a gastroenteritis hospitalisation in over 65s in the summer",
    Parameter == "gastro_gp_attend_1" ~ "Reporting parameter linking symptomatic norovirus infection in under 5s to all cause gastroenteritis diagnosed in primary care",
    # Parameter == "gastro_gp_attend_2" ~ "Reporting parameter linking symptomatic norovirus infection in 5-14s to all cause gastroenteritis diagnosed in primary care",
    Parameter == "D_immun" ~ "Number of years individual is immune",
    Parameter == "sigma" ~ "Proportion of individuals symptomatic"
  ), Parameter = case_when(
    Parameter == "surveillance_report_1" ~ "Underreporting to surveillance 0-4",
    # Parameter == "surveillance_report_1_winter" ~ "Underreporting to surveillance 0-4 in the winter",
    # Parameter == "surveillance_report_1_summer" ~ "Underreporting to surveillance 0-4 in the summer",
    # Parameter == "surveillance_report_2" ~ "Underreporting to surveillance 5-14",
    # Parameter == "surveillance_report_2_winter" ~ "Underreporting to surveillance 5-14 in the winter",
    # Parameter == "surveillance_report_2_summer" ~ "Underreporting to surveillance 5-14 in the summer",
    Parameter == "surveillance_report_3" ~ "Underreporting to surveillance 15-64",
    # Parameter == "surveillance_report_3_winter" ~ "Underreporting to surveillance 15-64 in the winter",
    # Parameter == "surveillance_report_3_summer" ~ "Underreporting to surveillance 15-64 in the summer",
    Parameter == "surveillance_report_4_winter" ~ "Underreporting to surveillance 65+ in the winter",
    Parameter == "surveillance_report_4_summer" ~ "Underreporting to surveillance 65+ in the summer",
    Parameter == "season_amp" ~ "Seasonal amplitude term",
    Parameter == "season_offset" ~ "Seasonal offset term",
    # Parameter == "season_amp_over65" ~ "Scaling seasonal amplitude for over 65",
    Parameter == "probT_under5" ~ "Probability of infection between under 5s",
    Parameter == "probT_over5" ~ "Probability of infection to over 5s",
    Parameter == "aki_hospitalisation_4_winter" ~ "Norovirus associated AKI hospitalisation in 65+ in the winter",
    # Parameter == "aki_hospitalisation_4_summer" ~ "Norovirus associated AKI hospitalisation in 65+ in the summer",
    Parameter == "gastro_hospitalisation_4_winter" ~ "Norovirus associated hospitalisation in 65+ in the winter",
    # Parameter == "gastro_hospitalisation_4_summer" ~ "Norovirus associated hospitalisation in 65+ in the summer",
    Parameter == "gastro_gp_attend_1" ~ "GP attendance for all cause gastroenteritis in under 5s",
    # Parameter == "gastro_gp_attend_2" ~ "GP attendance for all cause gastroenteritis in 5-14s",
    Parameter == "D_immun" ~ "Duration of immunity",
    Parameter == "sigma" ~ "Proportion symptomatic"
  )) |> 
  rename("Parameter" = Parameter) |> 
  flextable::flextable()  |>     # convert to pretty image
  padding(padding = 1.5, part = "all") |>  
  fontsize(size = 10, part = "all")  |> 
  width(j = 1, width = 3.7) |>   # Adjust the width of the second column
  width(j = 2, width = 2.5) |>  # Adjust the width of the second column
  width(j = 3, width = 4.1) |> # Adjust the width of the second column
  height_all(height = 0.4) |> 
  hrule(rule = "exact") |>
  theme_zebra() |> 
  hline_bottom(part = "body") |> 
  hline_top(part = "header") 
  
posterior_table_image

save_as_image(posterior_table_image, path = "figures/posterior_table.png", bg = "white")

#summary_stats
theta <- as.list(theta)
#par

times <- 11000
traj_median <- simulate(parameters = c(theta, par), init.state = init_matrix, times)
age_incidence_median <- simulate(parameters = c(theta, par), init.state = init_matrix, times, age.incidence = TRUE)
# 
# theta$aki_hospitalisation_4_winter <- log(0.25)
# theta$aki_hospitalisation_4_summer<- log(0.000001)
# 
# theta$probT_under5 <- log(0.24)
# theta$probT_over5 <- log(0.042)
# theta$D_immun <- 9
# 
# traj_median <- simulate(parameters = c(theta, par), init.state = init_matrix, times)
# age_incidence_median <- simulate(parameters = c(theta, par), init.state = init_matrix, times, age.incidence = TRUE)
# 
# traj_median |>
#   left_join(observation_data |> select(time, week_date, aki_hosp_obs_4), by = "time") |>
#   filter(time < 364) |>
#   ggplot(aes(x = week_date)) +
#   geom_point(aes(y = aki_hosp_obs_4, color = "Points"), size = 2, show.legend = FALSE) +
#   geom_line(aes(y = aki_hosp_model_4, linetype = "Model"), color = "red", show.legend = FALSE) +
#   theme_classic()
# 
# traj_median |>
#   left_join(observation_data |> select(time, week_date, noro_obs_3), by = "time") |>
#   filter(time < 364) |>
#   ggplot(aes(x = week_date)) +
#   geom_point(aes(y = noro_model_3, color = "Points"), size = 2, show.legend = FALSE) +
#   geom_line(aes(y = noro_obs_3, linetype = "Model"), color = "red", show.legend = FALSE) +
#   theme_classic()
# 
# age_incidence_median |>
#   left_join(age_incidence) |>
#   pivot_longer(cols = c(model_incidence, harris_incidence), names_to = "study", values_to = "incidence") |>
#   ggplot() +
#   geom_bar(
#     aes(x = age, y = incidence, fill = study),
#     stat = "identity",
#     position = "dodge"
#   )

total_noro_infections_65 <- traj_median |>
  summarize(total_infectious_symp_4 = sum(infectious_symp_4_count))

noro_linked_aki <- traj_median |>
  left_join(observation_data |> select(time, week), by = "time") |>
  mutate(seasonal_param = (0.5 * (1 + cos(2 * pi * (week - 1)/52))) * exp(theta$aki_hospitalisation_4_winter) +
           (0.5 * (1 - cos(2 * pi * (week - 1)/52))) * 0,
         infectious_symp_4_count_aki = infectious_symp_4_count*seasonal_param) |> 
  summarize(aki_linked = sum(infectious_symp_4_count_aki)) |>
  select(aki_linked)

noro_linked_aki_min <- traj_median |>
  left_join(observation_data |> select(time, week), by = "time") |>
  mutate(seasonal_param = (0.5 * (1 + cos(2 * pi * (week - 1)/52))) * exp(summary_stats$quantiles[8,1]) +
           (0.5 * (1 - cos(2 * pi * (week - 1)/52))) * 0,
         infectious_symp_4_count_aki = infectious_symp_4_count*seasonal_param) |> 
  summarize(aki_linked_min = sum(infectious_symp_4_count_aki)) |>
  select(aki_linked_min)

noro_linked_aki_max <- traj_median |>
  left_join(observation_data |> select(time, week), by = "time") |>
  mutate(seasonal_param = (0.5 * (1 + cos(2 * pi * (week - 1)/52))) * exp(summary_stats$quantiles[8,5]) +
           (0.5 * (1 - cos(2 * pi * (week - 1)/52))) * 0,
         infectious_symp_4_count_aki = infectious_symp_4_count*seasonal_param) |> 
  summarize(aki_linked_max = sum(infectious_symp_4_count_aki)) |>
  select(aki_linked_max)

aki_total <- observation_data |>
  mutate(aki_total = (aki_hosp_obs_4*init_matrix[4,1])/100000) |>
  summarize(aki_total = sum(aki_total))

total_noro_infections_65
noro_linked_aki
noro_linked_aki_min
noro_linked_aki_max

noro_linked_aki/aki_total
noro_linked_aki_min/aki_total
noro_linked_aki_max/aki_total

# generate latin hypercube samples of posterior 
source("R/lhs_posterior.R")
source("R/random_sample.R")
# lhs_samples <- generate_lhs_samples(traceBurnThin, n_samples = 2000)
random_samples <- generate_random_samples(traceBurnThin, n_samples = 1000)
lhs_samples <- random_samples

###

infectious_symp_4_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "infectious_symp_4_count")

infectious_symp_4_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(infectious_symp_4_trajectory, 2, median),
  ci_lower = apply(infectious_symp_4_trajectory, 2, quantile, 0.025),
  ci_upper = apply(infectious_symp_4_trajectory, 2, quantile, 0.975)) |> 
  summarize(
    percentile_2.5_noro_total = sum(ci_lower),
    percentile_97.5_noro_total = sum(ci_upper),
    percentile_50_noro_total = sum(median_traj)
  )

### age group incidence

process_age_group_incidence <- function(lhs_samples, init.state, age_group) {
  # Generate incidence data for the age group
  age_group_incidence <- generate_age_group_incidence_with_uncertainty(
    lhs_samples, 
    init.state, 
    age_group
  )
  
  # Create data frame with summary statistics
  data.frame(
    time = 1:365,
    median_traj = apply(age_group_incidence, 2, median),
    ci_lower = apply(age_group_incidence, 2, quantile, 0.025),
    ci_upper = apply(age_group_incidence, 2, quantile, 0.975)
  ) |> 
    summarize(
      percentile_2.5_noro_total = sum(ci_lower)/365,
      percentile_97.5_noro_total = sum(ci_upper)/365,
      percentile_50_noro_total = sum(median_traj)/365
    )
    
}

age_group_1_incidence <- process_age_group_incidence(lhs_samples, init.state, 1)
age_group_2_incidence <- process_age_group_incidence(lhs_samples, init.state, 2)
age_group_3_incidence <- process_age_group_incidence(lhs_samples, init.state, 3)
age_group_4_incidence <- process_age_group_incidence(lhs_samples, init.state, 4)

age_group_model_incidence <- age_group_1_incidence |> mutate(age_group = "0-4") |> 
  bind_rows(age_group_2_incidence |> mutate(age_group = "5-14")) |>
  bind_rows(age_group_3_incidence |> mutate(age_group = "15-64")) |>
  bind_rows(age_group_4_incidence |> mutate(age_group = "65+")) |> 
  mutate(study = "model_incidence")
  
age_group_harris_incidence <- age_incidence |> 
  mutate(study = "harris_incidence",
         percentile_2.5_noro_total = NA,
         percentile_97.5_noro_total = NA) |>
  rename(age_group = age, percentile_50_noro_total = harris_incidence)
  
incidence_fit <- age_group_model_incidence |>
  rbind(age_group_harris_incidence) |>
  rename(age = age_group,
         percentile_2.5 = percentile_2.5_noro_total ,
         percentile_97.5 = percentile_97.5_noro_total) |>
  mutate(
    age = factor(age, levels = c("0-4", "5-14", "15-64", "65+")),
    study = factor(study, levels = c("model_incidence", "harris_incidence")),
    percentile_2.5 = ifelse(
      is.na(percentile_2.5),
      case_when(
        age == "0-4" & study == "harris_incidence" ~ 147.1,
        age == "5-14" & study == "harris_incidence" ~ 40.1,
        age == "15-64" & study == "harris_incidence" ~ 34.7,
        age == "65+" & study == "harris_incidence" ~ 20.4,
        TRUE ~ percentile_2.5
      ),
      percentile_2.5
    ),
    percentile_97.5 = case_when(
      age == "0-4" & study == "harris_incidence" ~ 266.5,
      age == "5-14" & study == "harris_incidence" ~ 101.4,
      age == "15-64" & study == "harris_incidence" ~ 58.2,
      age == "65+" & study == "harris_incidence" ~ 45.3,
      TRUE ~ percentile_97.5
    ),
    study = case_when(
      study == "model_incidence" ~ "Model (95% CrI)",
      study == "harris_incidence" ~ "Harris et al. (95% CI)"
    ),
    study = factor(study, levels = c("Model (95% CrI)", "Harris et al. (95% CI)"))
  )  |>
  ggplot() +
  geom_bar(
    aes(x = age, y = percentile_50_noro_total, fill = study),
    stat = "identity",
    position = "dodge"
  ) +
  geom_errorbar(
    aes(
      x = age,
      y = percentile_50_noro_total,
      ymin = percentile_2.5,
      ymax = percentile_97.5,
      group = study
    ),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  theme_classic() +
  xlab("Age group") +
  ylab("Incidence per 1000 p-yrs") +
  guides(fill = guide_legend(title = NULL)) +
  ylim(c(0, 400)) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  ) +
  scale_color_manual(values = cbbPalette)

incidence_fit

## noro fit to observation data (model age 65+; observation data all ages)

noro_1_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_1")

noro_1_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(noro_1_trajectory, 2, median),
  ci_lower = apply(noro_1_trajectory, 2, quantile, 0.025),
  ci_upper = apply(noro_1_trajectory, 2, quantile, 0.975)
)

noro_1_fit_points <- noro_1_quantiles_df |> 
  left_join(observation_data |> select(time, week_date, noro_obs_1), by = "time") |>
  filter(time < 364) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
  geom_point(aes(y = noro_obs_1, color = "Observed"), size = 1.5) +
  geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
           ymin = -Inf, ymax = Inf) +
  theme_minimal(base_size = 11) +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 100), 
                     breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
  scale_fill_manual(values = c("95% CrI" = "#3498db")) +
  labs(title = "Norovirus laboratory surveillance, 0-4 years olds (SGSS)",
       y = "Number of laboratory reports",
       x = NULL) +
  theme(
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# noro_2_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_2")
# 
# noro_2_quantiles_df <- data.frame(
#   time = 1:365,
#   median_traj = apply(noro_2_trajectory, 2, median),
#   ci_lower = apply(noro_2_trajectory, 2, quantile, 0.025),
#   ci_upper = apply(noro_2_trajectory, 2, quantile, 0.975)
# )
# 
# noro_2_fit_points <- noro_2_quantiles_df |> 
#   left_join(observation_data |> select(time, week_date, noro_obs_2), by = "time") |>
#   filter(time < 364) |> 
#   ggplot(aes(x = week_date)) +
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
#   geom_point(aes(y = noro_obs_2, color = "Observed"), size = 1.5) +
#   geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   theme_minimal(base_size = 11) +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 40), 
#                      breaks = seq(0, 40, by = 10)) +
#   scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
#   scale_fill_manual(values = c("95% CrI" = "#3498db")) +
#   labs(title = "Norovirus laboratory surveillance, 5-14 years olds (SGSS)",
#        y = "Number of laboratory reports",
#        x = NULL) +
#   theme(
#     plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
#     plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
#     axis.title.y = element_text(size = 11, margin = margin(r = 10)),
#     axis.text = element_text(size = 10, color = "gray30"),
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.spacing.x = unit(0.5, 'cm'),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
#   )

noro_3_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_3")

noro_3_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(noro_3_trajectory, 2, median),
  ci_lower = apply(noro_3_trajectory, 2, quantile, 0.025),
  ci_upper = apply(noro_3_trajectory, 2, quantile, 0.975)
)

noro_3_fit_points <- noro_3_quantiles_df |> 
  left_join(observation_data |> select(time, week_date, noro_obs_3), by = "time") |>
  filter(time < 364) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
  geom_point(aes(y = noro_obs_3, color = "Observed"), size = 1.5) +
  geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
           ymin = -Inf, ymax = Inf) +
  theme_minimal(base_size = 11) +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 200), 
                     breaks = seq(0, 200, by = 50)) +
  scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
  scale_fill_manual(values = c("95% CrI" = "#3498db")) +
  labs(title = "Norovirus laboratory surveillance, 15-64 years olds (SGSS)",
       y = "Number of laboratory reports",
       x = NULL) +
  theme(
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )
 
noro_4_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "noro_model_4")

noro_4_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(noro_4_trajectory, 2, median),
  ci_lower = apply(noro_4_trajectory, 2, quantile, 0.025),
  ci_upper = apply(noro_4_trajectory, 2, quantile, 0.975)
)

noro_4_fit_points <- noro_4_quantiles_df |> 
  left_join(observation_data |> select(time, week_date, noro_obs_4), by = "time") |>
  filter(time < 364) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
  geom_point(aes(y = noro_obs_4, color = "Observed"), size = 1.5) +
  geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
           ymin = -Inf, ymax = Inf) +
  theme_minimal(base_size = 11) +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 500), 
                     breaks = seq(0, 500, by = 100)) +
  scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
  scale_fill_manual(values = c("95% CrI" = "#3498db")) +
  labs(title = "Norovirus laboratory surveillance, 65+ year olds (SGSS)",
       y = "Number of laboratory reports",
       x = NULL) +
  theme(
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 15)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

## aki hosp fit to observation data (age 65+)

aki_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "aki_hosp_model_4")

aki_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(aki_trajectory, 2, median),
  ci_lower = apply(aki_trajectory, 2, quantile, 0.025),
  ci_upper = apply(aki_trajectory, 2, quantile, 0.975)
)

aki_fit_points <- aki_quantiles_df |> 
  left_join(observation_data |> select(time, week_date, aki_hosp_obs_4), by = "time") |>
  filter(time < 364) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
  geom_point(aes(y = aki_hosp_obs_4, color = "Observed"), size = 1.5) +
  geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
           ymin = -Inf, ymax = Inf) +
  # Heatwave periods
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2019-06-28"), xmax = as.Date("2019-06-30"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2019-07-21"), xmax = as.Date("2019-07-28"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2019-08-23"), xmax = as.Date("2019-08-29"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2018-06-25"), xmax = as.Date("2018-06-27"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2018-06-30"), xmax = as.Date("2018-07-10"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2018-07-21"), xmax = as.Date("2018-07-29"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2018-08-01"), xmax = as.Date("2018-08-09"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2017-06-16"), xmax = as.Date("2017-06-23"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2017-07-05"), xmax = as.Date("2017-07-07"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2016-07-18"), xmax = as.Date("2016-07-22"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2016-08-22"), xmax = as.Date("2016-08-26"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2016-09-12"), xmax = as.Date("2016-09-17"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2015-07-01"), xmax = as.Date("2015-07-03"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2013-07-12"), xmax = as.Date("2013-07-23"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "#FF000030",
           xmin = as.Date("2013-07-30"), xmax = as.Date("2013-08-02"),
           ymin = -Inf, ymax = Inf) +
  theme_minimal(base_size = 11) +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 200), 
                     breaks = seq(0, 200, by = 50)) +
  scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
  scale_fill_manual(values = c("95% CrI" = "#3498db")) +
  labs(title = "Acute kidney injury hospital admissions, 65+ years olds (HES)",
       y = "Incidence per 100,000 person-years",
       x = NULL) +
  theme(
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

## gastro hosp fit to observation data (age 65+)

gastro_hosp_4_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "gastro_hosp_model_4")

gastro_hosp_4_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(gastro_hosp_4_trajectory, 2, median),
  ci_lower = apply(gastro_hosp_4_trajectory, 2, quantile, 0.025),
  ci_upper = apply(gastro_hosp_4_trajectory, 2, quantile, 0.975)
)

gastro_fit_points <- gastro_hosp_4_quantiles_df |> 
  left_join(observation_data |> select(time, week_date, gastro_hosp_obs_4), by = "time") |>
  filter(time < 364) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
  geom_point(aes(y = gastro_hosp_obs_4, color = "Observed"), size = 1.5) +
  geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
           ymin = -Inf, ymax = Inf) +
  theme_minimal(base_size = 11) +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 100), 
                     breaks = seq(0, 100, by = 25)) +
  scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
  scale_fill_manual(values = c("95% CrI" = "#3498db")) +
  labs(title = "Gastroenteritis hospital admissions, 65+ years olds (HES)",
       y = "Incidence per 100,000 person-years",
       x = NULL) +
  theme(
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

## gastro hosp fit to observation data (age 0-4)

gastro_gp_1_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "gastro_gp_model_1")

gastro_gp_1_quantiles_df <- data.frame(
  time = 1:365,
  median_traj = apply(gastro_gp_1_trajectory, 2, median, na.rm = TRUE),
  ci_lower = apply(gastro_gp_1_trajectory, 2, quantile, 0.025, na.rm = TRUE),
  ci_upper = apply(gastro_gp_1_trajectory, 2, quantile, 0.975, na.rm = TRUE)
)

gastro_gp_1_fit_points <- gastro_gp_1_quantiles_df |> 
  left_join(observation_data |> select(time, week_date, gastro_gp_obs_1), by = "time") |>
  filter(time < 364) |>
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
  geom_point(aes(y = gastro_gp_obs_1, color = "Observed"), size = 1.5) +
  geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "gray95", alpha = 0.5,
           xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
           ymin = -Inf, ymax = Inf) +
  theme_minimal(base_size = 11) +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 400), 
                     breaks = seq(0, 400, by = 50)) +
  scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
  scale_fill_manual(values = c("95% CrI" = "#3498db")) +
  labs(title = "Gastroenteritis primary care attendance, 0-4 year olds (CPRD)",
       y = "Incidence per 100,000 person-years",
       x = NULL) +
  theme(axis.title.y = element_text(size = 11, margin = margin(r = 10)),
        axis.text = element_text(size = 10, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

## gastro hosp fit to observation data (age 5-14)

# gastro_gp_2_trajectory <- generate_trajectories_with_uncertainty(lhs_samples, init.state, outcome = "gastro_gp_model_2")

# gastro_gp_2_quantiles_df <- data.frame(
#   time = 1:365,
#   median_traj = apply(gastro_gp_2_trajectory, 2, median, na.rm = TRUE),
#   ci_lower = apply(gastro_gp_2_trajectory, 2, quantile, 0.025, na.rm = TRUE),
#   ci_upper = apply(gastro_gp_2_trajectory, 2, quantile, 0.975, na.rm = TRUE)
# )
# 
# gastro_gp_2_fit_points <- gastro_gp_2_quantiles_df |> 
#   left_join(observation_data |> select(time, week_date, gastro_gp_obs_2), by = "time") |>
#   filter(time < 364) |>
#   ggplot(aes(x = week_date)) +
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CrI"), alpha = 0.5) +
#   geom_point(aes(y = gastro_gp_obs_2, color = "Observed"), size = 1.5) +
#   geom_line(aes(y = median_traj, color = "Model fit"), linewidth = 1) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "gray95", alpha = 0.5,
#            xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
#            ymin = -Inf, ymax = Inf) +
#   theme_minimal(base_size = 11) +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 200), 
#                      breaks = seq(0, 200, by = 50)) +
#   scale_color_manual(values = c("Observed" = "#2c3e50", "Model fit" = "#e74c3c")) +
#   scale_fill_manual(values = c("95% CrI" = "#3498db")) +
#   labs(title = "Gastroenteritis primary care attendance, 5-14 year olds (CPRD)",
#        y = "Incidence per 100,000 person-years",
#        x = NULL) +
#   theme(axis.title.y = element_text(size = 11, margin = margin(r = 10)),
#         axis.text = element_text(size = 10, color = "gray30"),
#         legend.position = "bottom",
#         legend.title = element_blank(),
#         legend.spacing.x = unit(0.5, 'cm'),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
#   )
       
source("R/random_trajectories_plot.R")

multi_panel_noro_surveillance_plot <- ggarrange(
  noro_1_fit_points,
  # noro_2_fit_points,
  noro_3_fit_points,
  noro_4_fit_points,
  ncol = 2,
  nrow = 2,
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE
)

multi_panel_noro_surveillance_plot

ggarrange(noro_1, noro_3, noro_4, ncol = 2, nrow = 2, common.legend = TRUE)

ggsave("figures/multi_panel_noro_surveillance_plot.png", width = 14, height = 12, dpi = 600, bg = "white")
# ggsave("figures/multi_panel_figure_1.pdf", width = 12, height = 14)

multi_panel_healthcare_plot <- ggarrange(
  gastro_gp_1_fit_points,
  # gastro_gp_2_fit_points,
  gastro_fit_points,
  aki_fit_points,
  ncol = 2,
  nrow = 2,
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE,
  legend = "bottom"
)

multi_panel_healthcare_plot

ggarrange(gastro_gp_1, gastro_hosp_4, aki, ncol = 2, nrow = 2, common.legend = TRUE)

ggsave("figures/multi_panel_healthcare_plot.png", width = 14, height = 12, dpi = 600, bg = "white")

# ggsave("figures/multi_panel_figure_2.png", width = 12, height = 14)
# ggsave("figures/multi_panel_figure_2.pdf", width = 12, height = 14)

# ggarrange(noro_1_fit_points, noro_2_fit_points, noro_3_fit_points, noro_4_fit_points, gastro_gp1_fit_points, gastro_gp2_fit_points, gastro_fit_points, aki_fit_points, ncol = 2, nrow = 4, labels = c("A", "B", "C", "D"))

# ggsave("figures/multi_panel_figure.png", width = 12, height = 14)

source("R/07_reference_costing.R")

cost_table <- cost_per_year_signif |> 
  rbind(total_cost_signif) |>
  mutate(
    mean_cost_per_activity = as.double(mean_cost_per_activity),
    across(infectious_symp_4_total:aki_max, scales::label_comma()),
    across(total_cost_year:max_cost_2021, scales::label_currency(prefix = "£", decimal.mark = ".")),
    aki_cri = paste0(aki_min, "-", aki_max),
         aki_range = paste0(aki_total, " ", paste0("(", aki_cri, ")")),
    cost_cri = paste0(min_cost_year , "-", max_cost_year),
         cost_range = paste0(total_cost_year , " ", paste0("(", cost_cri, ")")),
    cost_2021_cri = paste0(min_cost_2021 , "-", max_cost_2021),
         cost_range_2021 = paste0(cost_2021 , " ", paste0("(", cost_2021_cri, ")")),
         ) |>
  select(year, infectious_symp_4_total, aki_range, mean_cost_per_activity, cost_range_2021) |> 
  rename("Year" = year,
         "Number of norovirus infections (thousands)" = infectious_symp_4_total,
         "Number of AKI hospitalisations linked (thousands)" = aki_range,
         "Mean activity weighted reference cost (£)" = mean_cost_per_activity,
         # "Total cost (million)" = cost_range,
         "Annual total cost inflation adjusted (millions)" = cost_range_2021) |> 
  flextable::flextable() |>     # convert to pretty image
  autofit() |> 
  padding(padding = 1.5, part = "all")  |>  
  theme_zebra() |> # Adjust the width of the second column
  align(align = "right") |>
  align(part = "header", align = "left") |>  # Left justify column titles
  align(j = 1, align = "left") |>  # Left justify rows in the first column
  fontsize(size = 10, part = "all")  |>  
  # fit_to_width(8.0) |> 
  width(j = 1, width = 1.0) |>   # Adjust the width of the second column
  width(j = 2, width = 1.8) |>   # Adjust the width of the second column
  width(j = 3, width = 1.8) |>  # Adjust the width of the second column
  width(j = 4, width = 1.8) |>
  width(j = 5, width = 1.8) |>
  height_all(height = 0.3) |>
  hrule(rule = "exact") |>
  hline_bottom(part = "body") |> 
  hline_top(part = "header")

cost_table

save_as_image(cost_table, path = "figures/cost_table.png")

#' multiple chains

xyplot(multi_trace_params)

xyplot(trace1_params)
xyplot(trace2_params)
xyplot(trace3_params)
xyplot(trace4_params)

#' #### logDensity

xyplot(multi_trace_log_density)

traceBurnThin_params_multi <- burnAndThin(multi_trace_params, burn = burn_value, thin = thin_factor)

burnAndThin_log <- function(trace, burn = 0, thin = 0) {
  convertToMCMC <- FALSE
  if (inherits(trace, "mcmc")) {
    convertToMCMC <- TRUE
    trace <- as.data.frame(trace)
  }
  else if (inherits(trace, "mcmc.list")) {
    convertToMCMC <- TRUE
    trace <- as.list(trace)
  }
  
  if (is.data.frame(trace) || is.matrix(trace)) {
    if (burn > 0) {
      trace <- trace[-(1:burn), ]
    }
    trace <- trace[seq(1, nrow(trace), thin + 1), ]
    if (convertToMCMC) {
      trace <- mcmc(trace)
    }
  }
  else {
    trace <- lapply(trace, function(x) {
      if (burn > 0) {
        x <- x[-(1:burn)]  # Removed the comma
      }
      x <- x[seq(1, length(x), thin + 1)]  # Changed nrow(x) to length(x)
      if (convertToMCMC) {
        x <- mcmc(x)
      }
      return(x)
    })
    if (convertToMCMC) {
      trace <- mcmc.list(trace)
    }
  }
  return(trace)
}

traceBurnThin_log_density_multi <- burnAndThin_log(multi_trace_log_density, burn = burn_value, thin = thin_factor)

#' ## multiple chains without burn in

xyplot(traceBurnThin_params_multi)

xyplot(traceBurnThin_log_density_multi)
