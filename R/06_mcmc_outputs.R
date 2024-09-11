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

starting.value

prop.sd

#' ## Covariance matrix

# mcmc_trace$covmatEmpirical

#' ## Trace plots

xyplot( x = params_trace)

#' #### logDensity

xyplot( x = log_density_trace)

#' ## acceptance rate

1 - rejectionRate(my_trace)

#' ## effective sample size

# plotEssBurn(my_trace)

#' ## trace without burn in

burn_value <- 50000

traceBurn <- burnAndThin(my_trace, burn = burn_value)

traceBurn_df <- burnAndThin(my_trace_df, burn = burn_value)
traceBurn_params <- burnAndThin(params_trace, burn = burn_value)
traceBurn_log_density <- mcmc(traceBurn[,16])

effectiveSize(traceBurn)

xyplot( x = traceBurn_params)

plotEssBurn(traceBurn_params)

#' #### logDensity

xyplot( x = traceBurn_log_density)

#' ## acceptance rate

1 - rejectionRate(traceBurn)

#' ## autocorrelation plot 

acfplot(x = traceBurn, lag.max = 60)

#' ## thinned trace

thin_factor <- 50

traceBurnThin <- burnAndThin(my_trace, burn = burn_value, thin = thin_factor)
traceBurnThin_df <- burnAndThin(my_trace_df, burn = burn_value, thin = thin_factor)
traceBurnThin_params <- burnAndThin(params_trace, burn = burn_value, thin = thin_factor)
traceBurnThin_log_density <- mcmc(traceBurnThin[,16])

effectiveSize(traceBurnThin)

xyplot( x = traceBurnThin_params)

#' #### logDensity

xyplot( x = traceBurnThin_log_density)

acfplot(x = traceBurnThin, lag.max = 60)

#' ## comparing thinned and unthinned trace

plotPosteriorDensity(list(unthinned = traceBurn_df, thinned = traceBurnThin_df))

traceBurn_df_aki <- traceBurn_df |> 
  select(aki_hospitalisation_4) |> 
  rename('Norovirus linked AKI hospitalisation' = aki_hospitalisation_4)

traceBurnThin_df_aki <- traceBurnThin_df |> 
  select(aki_hospitalisation_4) |> 
  rename('Norovirus linked AKI hospitalisation' = aki_hospitalisation_4)
  
#' ## correlation

levelplot(traceBurnThin, col.regions = heat.colors(100), scales=list(x=list(rot=90)))

mcmc_pairs(
  traceBurnThin_df,
  pars = c(
    "sigma",
    "season_amp",
    "season_offset",
    "season_amp_over65",
    "D_immun",
    "probT_under5",
    "probT_over5",
    "surveillance_report_1",
    "surveillance_report_2",
    "surveillance_report_3",
    "surveillance_report_4",
    "aki_hospitalisation_4",
    "gastro_hospitalisation_4",
    "gastro_gp_attend_1",
    "gastro_gp_attend_2"
  ),
  diag_fun = "dens",
  off_diag_fun = "hex",
  off_diag_args = list(size = 1, alpha = 0.5)
)

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
  "season_amp_over65",
  "sigma",
  "surveillance_report_1",
  "surveillance_report_2",
  "surveillance_report_3",
  "surveillance_report_4",
  "season_amp",
  "season_offset",
  "aki_hospitalisation_4",
  "gastro_hospitalisation_4",
  "gastro_gp_attend_1",
  "gastro_gp_attend_2",
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
                    Parameter == "aki_hospitalisation_4" ~ exp(.),
                    Parameter == "gastro_hospitalisation_4" ~ exp(.),
                    Parameter == "gastro_gp_attend_1" ~ exp(.),
                    Parameter == "gastro_gp_attend_2" ~ exp(.),
                    TRUE ~ .
                  ))) |> 
  select(Parameter, theta, q2.5, q97.5)

parameter_order <- c("sigma", "D_immun", "probT_under5", "probT_over5",  "season_amp", "season_amp_over65", "season_offset", 
                     "aki_hospitalisation_4", "gastro_hospitalisation_4", "surveillance_report_1", "surveillance_report_2", 
                     "surveillance_report_3", "surveillance_report_4", "gastro_gp_attend_1", "gastro_gp_attend_2")

posterior_table_image <- posterior_table |> 
  mutate(theta = signif(theta, digits = 3),
         q2.5 = signif(q2.5, digits = 3),
         q97.5 = signif(q97.5, digits = 3),
         cri = paste0(q2.5, "-", q97.5),
         "Median (95% CrI)" = paste0(theta, " ", paste0("(", cri, ")"))
         ) |>
  filter(!row_number() %in% c(16)) |> 
  select(Parameter, `Median (95% CrI)` ) |>
  arrange(factor(Parameter, levels = parameter_order)) |> 
  mutate(Explanation = case_when(
    Parameter == "surveillance_report_1" ~ "Proportion of symptomatic norovirus in 0-4 year olds reported to surveillance",
    Parameter == "surveillance_report_2" ~ "Proportion of symptomatic norovirus in 5-14 year olds reported to surveillance",
    Parameter == "surveillance_report_3" ~ "Proportion of symptomatic norovirus in 15-64 year olds reported to surveillance",
    Parameter == "surveillance_report_4" ~ "Proportion of symptomatic norovirus in over 65s reported to surveillance",
    Parameter == "season_amp" ~ "A term forcing the amplitude of the periodicity in the contact rate",
    Parameter == "season_offset" ~ "A term forcing the timing of the periodicity in the contact rate",
    Parameter == "season_amp_over65" ~ "Scaling seasonal amplitude for over 65 seasonality to improve fit",
    Parameter == "probT_under5" ~ "Probability of transmission in under 5s transmitting to under 5s",
    Parameter == "probT_over5" ~ "Probability of transmission transmitting to over 5s",
    Parameter == "aki_hospitalisation_4" ~ "Proportion of symptomatic norovirus infections linked to an AKI hospitalisation in over 65s",
    Parameter == "gastro_hospitalisation_4" ~ "Proportion of symptomatic norovirus infections linked to a gastroenteritis hospitalisation in over 65s",
    Parameter == "gastro_gp_attend_1" ~ "Reporting parameter linking symptomatic norovirus infection in under 5s to all cause gastroenteritis diagnosed in primary care",
    Parameter == "gastro_gp_attend_2" ~ "Reporting parameter linking symptomatic norovirus infection in 5-14s to all cause gastroenteritis diagnosed in primary care",
    Parameter == "D_immun" ~ "Number of years individual is immune",
    Parameter == "sigma" ~ "Proportion of individuals symptomatic"
  ), Parameter = case_when(
    Parameter == "surveillance_report_1" ~ "Underreporting to surveillance 0-4",
    Parameter == "surveillance_report_2" ~ "Underreporting to surveillance 5-14",
    Parameter == "surveillance_report_3" ~ "Underreporting to surveillance 15-64",
    Parameter == "surveillance_report_4" ~ "Underreporting to surveillance 65+",
    Parameter == "season_amp" ~ "Seasonal amplitude term",
    Parameter == "season_offset" ~ "Seasonal offset term",
    Parameter == "season_amp_over65" ~ "Scaling seasonal amplitude for over 65",
    Parameter == "probT_under5" ~ "Probability of infection between under 5s",
    Parameter == "probT_over5" ~ "Probability of infection to over 5s",
    Parameter == "aki_hospitalisation_4" ~ "Norovirus associated AKI hospitalisation in 65+",
    Parameter == "gastro_hospitalisation_4" ~ "Norovirus associated hospitalisation in 65+",
    Parameter == "gastro_gp_attend_1" ~ "GP attendance for all cause gastroenteritis in under 5s",
    Parameter == "gastro_gp_attend_2" ~ "GP attendance for all cause gastroenteritis in 5-14s",
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

save_as_image(posterior_table_image, path = "figures/posterior_table.png")

summary_stats
theta <- as.list(theta)
par

times <- 11000
traj_median <- simulate(parameters = c(theta, par), init.state = init_matrix, times)
age_incidence_median <- simulate(parameters = c(theta, par), init.state = init_matrix, times, age.incidence = TRUE)

total_noro_infections_65 <- traj_median |> 
  summarize(total_infectious_symp_4 = sum(infectious_symp_4_count))

noro_linked_aki <- traj_median |> 
  summarize(total_infectious_symp_4 = sum(infectious_symp_4_count)) |> 
  mutate(aki_linked = total_infectious_symp_4*exp(theta$aki_hospitalisation_4)) |> 
  select(aki_linked)

noro_linked_aki_min <- traj_median |> 
  summarize(total_infectious_symp_4 = sum(infectious_symp_4_count)) |> 
  mutate(aki_linked_min = total_infectious_symp_4*exp(summary_stats$quantiles[9,1])) |> 
  select(aki_linked_min)

noro_linked_aki_max <- traj_median |> 
  summarize(total_infectious_symp_4 = sum(infectious_symp_4_count)) |> 
  mutate(aki_linked_max = total_infectious_symp_4*exp(summary_stats$quantiles[9,5])) |> 
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

n_sample_trace <- nrow(traceBurnThin)
source("R/replicates.R")
traj_repli <- replicates(mcmc_trace = traceBurnThin, n_samples = n_sample_trace)
# traj_repli <- replicates(mcmc_trace = my_trace, n_samples = 1000)
traj_repli_age_group <- replicates_age_group(mcmc_trace = traceBurnThin, n_samples = n_sample_trace)

traj_summary <- traj_repli |> 
  group_by(time) |> 
  mutate(
    percentile_2.5_noro_1 = quantile(noro_model_1, probs = 0.025, na.rm = TRUE),
         percentile_2.5_noro_2 = quantile(noro_model_2, probs = 0.025, na.rm = TRUE),
         percentile_2.5_noro_3 = quantile(noro_model_3, probs = 0.025, na.rm = TRUE),
         percentile_2.5_noro_4 = quantile(noro_model_4, probs = 0.025, na.rm = TRUE),
         percentile_2.5_aki = quantile(aki_hosp_model_4, probs = 0.025, na.rm = TRUE),
         percentile_2.5_gastro = quantile(gastro_hosp_model_4, probs = 0.025, na.rm = TRUE),
         percentile_2.5_gastro_gp1 = quantile(gastro_gp_model_1 , probs = 0.025, na.rm = TRUE),
         percentile_2.5_gastro_gp2 = quantile(gastro_gp_model_2 , probs = 0.025, na.rm = TRUE),
         percentile_97.5_noro_1 = quantile(noro_model_1, probs = 0.975, na.rm = TRUE),
         percentile_97.5_noro_2 = quantile(noro_model_2, probs = 0.975, na.rm = TRUE),
         percentile_97.5_noro_3 = quantile(noro_model_3, probs = 0.975, na.rm = TRUE),
         percentile_97.5_noro_4 = quantile(noro_model_4, probs = 0.975, na.rm = TRUE),
         percentile_97.5_aki = quantile(aki_hosp_model_4, probs = 0.975, na.rm = TRUE),
         percentile_97.5_gastro = quantile(gastro_hosp_model_4, probs = 0.975, na.rm = TRUE),
         percentile_97.5_gastro_gp1 = quantile(gastro_gp_model_1 , probs = 0.975, na.rm = TRUE),
         percentile_97.5_gastro_gp2 = quantile(gastro_gp_model_2 , probs = 0.975, na.rm = TRUE),
         percentile_50_noro_1 = quantile(noro_model_1, probs = 0.50, na.rm = TRUE),
         percentile_50_noro_2 = quantile(noro_model_2, probs = 0.50, na.rm = TRUE),
         percentile_50_noro_3 = quantile(noro_model_3, probs = 0.50, na.rm = TRUE),
         percentile_50_noro_4 = quantile(noro_model_4, probs = 0.50, na.rm = TRUE),
         percentile_50_aki = quantile(aki_hosp_model_4, probs = 0.50, na.rm = TRUE),
         percentile_50_gastro = quantile(aki_hosp_model_4, probs = 0.50, na.rm = TRUE),
         percentile_50_gastro_gp1 = quantile(gastro_gp_model_1 , probs = 0.50, na.rm = TRUE),
         percentile_50_gastro_gp2 = quantile(gastro_gp_model_2 , probs = 0.50, na.rm = TRUE)
  ) |>
  ungroup() |> 
  filter(replicate == 1) |> 
  select(
    time,
    percentile_2.5_noro_1,
    percentile_2.5_noro_2,
    percentile_2.5_noro_3,
    percentile_2.5_noro_4,
    percentile_2.5_aki,
    percentile_2.5_gastro,
    percentile_2.5_gastro_gp1,
    percentile_2.5_gastro_gp2,
    percentile_97.5_noro_1,
    percentile_97.5_noro_2,
    percentile_97.5_noro_3,
    percentile_97.5_noro_4,
    percentile_97.5_aki,
    percentile_97.5_gastro,
    percentile_97.5_gastro_gp1,
    percentile_97.5_gastro_gp2,
    percentile_50_noro_1,
    percentile_50_noro_2,
    percentile_50_noro_3,
    percentile_50_noro_4,
    percentile_50_aki,
    percentile_50_gastro,
    percentile_50_gastro_gp1,
    percentile_50_gastro_gp2
  )

traj_summary_noro_count_4 <- traj_repli |> 
  group_by(time) |> 
  mutate(
    percentile_2.5_noro_4 = quantile(infectious_symp_4_count, probs = 0.025, na.rm = TRUE),
    percentile_97.5_noro_4 = quantile(infectious_symp_4_count, probs = 0.975, na.rm = TRUE),
    percentile_50_noro_4 = quantile(infectious_symp_4_count, probs = 0.50, na.rm = TRUE),
  ) |>
  ungroup() |> 
  filter(replicate == 1) |> 
  select(
    time,
    percentile_2.5_noro_4,
    percentile_97.5_noro_4,
    percentile_50_noro_4
  ) |> 
  summarize(
    percentile_2.5_noro_total = sum(percentile_2.5_noro_4),
    percentile_97.5_noro_total = sum(percentile_97.5_noro_4),
    percentile_50_noro_total = sum(percentile_50_noro_4)
  )

traj_summary_noro_count_4

traj_summary_age_group <- traj_repli_age_group |> 
  group_by(age) |> 
  mutate(percentile_2.5 = quantile(model_incidence, probs = 0.025, na.rm = TRUE),
         percentile_97.5 = quantile(model_incidence, probs = 0.975, na.rm = TRUE),
         percentile_50 = quantile(model_incidence, probs = 0.50, na.rm = TRUE),
  ) |>
  ungroup() |> 
  filter(replicate == 1) |>
  mutate(study = "model_incidence") |> 
  select(age, study, percentile_2.5, percentile_97.5, percentile_50)

### age group incidence

age_incidence_error <- age_incidence_median |> 
  left_join(age_incidence) |> 
  pivot_longer(cols = !age, names_to = "study", values_to = "value") |>
  left_join(traj_summary_age_group, by = c("age", "study")) |>
  mutate(age = factor(age, levels = c("0-4", "5-14", "15-64", "65+")),
         study = factor(study, levels = c("model_incidence", "harris_incidence")),
         percentile_2.5 = ifelse(is.na(percentile_2.5), case_when(
           age == "0-4" & study == "harris_incidence" ~ 147.1,
           age == "5-14" & study == "harris_incidence" ~ 40.1,
           age == "15-64" & study == "harris_incidence" ~ 34.7,
           age == "65+" & study == "harris_incidence" ~ 20.4,
           TRUE ~ percentile_2.5), percentile_2.5),
         percentile_97.5 = case_when(
           age == "0-4" & study == "harris_incidence" ~ 266.5,
           age == "5-14" & study == "harris_incidence" ~ 101.4,
           age == "15-64" & study == "harris_incidence" ~ 58.2,
           age == "65+" & study == "harris_incidence" ~ 45.3,
           TRUE ~ percentile_97.5)
  )

incidence_fit <- age_incidence_median |> 
  left_join(age_incidence) |>
  pivot_longer(cols = !age, names_to = "study", values_to = "value") |>
  mutate(age = factor(age, levels = c("0-4", "5-14", "15-64", "65+")),
         study = factor(study, levels = c("model_incidence", "harris_incidence"))) |>
  left_join(age_incidence_error |> select(age, study, percentile_2.5, percentile_97.5)) |>
  mutate(age = factor(age, levels = c("0-4", "5-14", "15-64", "65+")),
         study = case_when(
           study == "model_incidence" ~ "Model (95% CrI)",
           study == "harris_incidence" ~ "Harris et al. (95% CI)"
         ),
         study = factor(study, levels = c("Model (95% CrI)", "Harris et al. (95% CI)"))) |> 
  ggplot() +
  geom_bar(aes(x = age, y = value, fill = study), stat = "identity", position = "dodge") +
  geom_errorbar(aes(x = age, y = value, ymin = percentile_2.5, ymax = percentile_97.5, group = study),
                position = position_dodge(width = 0.9), width = 0.25) +
  theme_classic() +
  xlab("Age group") +
  ylab("Incidence per 1000 p-yrs") +
  guides(fill = guide_legend(title = NULL)) + 
  ylim(c(0,300)) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.text = element_text(size = 10)) +
  scale_color_manual(values = cbbPalette)



## noro fit to observation data (model age 65+; observation data all ages)

noro_1_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_noro_1, percentile_97.5_noro_1), by ="time") |>
  left_join(observation_data, by = "time") |>
  select(week_date, noro_model_1, percentile_2.5_noro_1, percentile_97.5_noro_1,  noro_obs_1) |>
  ggplot(aes(x = week_date)) +
  geom_point(aes(y = noro_obs_1, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = noro_model_1, linetype = "Model"), color = 'red', show.legend = FALSE) +
  geom_ribbon(aes(ymin = percentile_2.5_noro_1, ymax = percentile_97.5_noro_1, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Number of laboratory reports") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, title = "Norovirus laboratory surveillance, 0-4 years olds (SGSS)")

noro_2_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_noro_2, percentile_97.5_noro_2), by ="time") |>
  left_join(observation_data, by = "time") |>
  select(week_date, noro_model_2, percentile_2.5_noro_2, percentile_97.5_noro_2,  noro_obs_2) |>
  ggplot(aes(x = week_date)) +
  geom_point(aes(y = noro_obs_2, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = noro_model_2, linetype = "Model"), color = 'red', show.legend = FALSE) +
  geom_ribbon(aes(ymin = percentile_2.5_noro_2, ymax = percentile_97.5_noro_2, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 20), breaks = seq(0, 20, by = 5)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Number of laboratory reports") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, title = "Norovirus laboratory surveillance, 5-14 years olds (SGSS)")

noro_3_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_noro_3, percentile_97.5_noro_3), by ="time") |>
  left_join(observation_data, by = "time") |>
  select(week_date, noro_model_3, percentile_2.5_noro_3, percentile_97.5_noro_3,  noro_obs_3) |>
  ggplot(aes(x = week_date)) +
  geom_point(aes(y = noro_obs_3, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = noro_model_3, linetype = "Model"), color = 'red', show.legend = FALSE) +
  geom_ribbon(aes(ymin = percentile_2.5_noro_3, ymax = percentile_97.5_noro_3, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Number of laboratory reports") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, title = "Norovirus laboratory surveillance, 15-64 years olds (SGSS)")

noro_4_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_noro_4, percentile_97.5_noro_4), by ="time") |> 
  left_join(observation_data, by = "time") |>
  select(week_date, noro_model_4, percentile_2.5_noro_4, percentile_97.5_noro_4,  noro_obs_4) |> 
  ggplot(aes(x = week_date)) +
  geom_point(aes(y = noro_obs_4, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = noro_model_4, linetype = "Model"), color = 'red', show.legend = FALSE) +
  geom_ribbon(aes(ymin = percentile_2.5_noro_4, ymax = percentile_97.5_noro_4, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 300), breaks = seq(0, 300, by = 100)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Number of laboratory reports") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, title = "Norovirus laboratory surveillance, 65+ years olds (SGSS)")

## aki hosp fit to observation data (age 65+)

aki_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_aki, percentile_97.5_aki), by ="time") |> 
  left_join(observation_data, by = "time") |>
  select(week_date, aki_hosp_model_4, percentile_2.5_aki, percentile_97.5_aki,  aki_hosp_obs_4) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = percentile_2.5_aki, ymax = percentile_97.5_aki, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(y = aki_hosp_obs_4, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = aki_hosp_model_4, linetype = "Model"), color = "red", show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 200), breaks = seq(0, 200, by = 50)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-06-28", "%Y-%m-%d"), xmax = as.Date("2019-06-30",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-07-21", "%Y-%m-%d"), xmax = as.Date("2019-07-28",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2019-08-23", "%Y-%m-%d"), xmax = as.Date("2019-08-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-25", "%Y-%m-%d"), xmax = as.Date("2018-06-27",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-06-30", "%Y-%m-%d"), xmax = as.Date("2018-07-10",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-07-21", "%Y-%m-%d"), xmax = as.Date("2018-07-29",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2018-08-01", "%Y-%m-%d"), xmax = as.Date("2018-08-09",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-06-16", "%Y-%m-%d"), xmax = as.Date("2017-06-23",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2017-07-05", "%Y-%m-%d"), xmax = as.Date("2017-07-07",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-07-18", "%Y-%m-%d"), xmax = as.Date("2016-07-22",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-08-22", "%Y-%m-%d"), xmax = as.Date("2016-08-26",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2016-09-12", "%Y-%m-%d"), xmax = as.Date("2016-09-17",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2015-07-01", "%Y-%m-%d"), xmax = as.Date("2015-07-03",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2013-07-12", "%Y-%m-%d"), xmax = as.Date("2013-07-23",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.3,
           xmin = as.Date("2013-07-30", "%Y-%m-%d"), xmax = as.Date("2013-08-02",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Incidence per 100,000 p-yrs") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL ,title = "Acute kidney injury hospital admissions, 65+ years olds (HES)")

## gastro hosp fit to observation data (age 65+)

gastro_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_gastro, percentile_97.5_gastro), by ="time") |> 
  left_join(observation_data, by = "time") |>
  select(week_date, gastro_hosp_model_4, percentile_2.5_gastro, percentile_97.5_gastro,  gastro_hosp_obs_4) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = percentile_2.5_gastro, ymax = percentile_97.5_gastro, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(y = gastro_hosp_obs_4, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = gastro_hosp_model_4, linetype = "Model"), color = "red", show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Incidence per 100,000 p-yrs") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, title = "Gastroenteritis hospital admissions, 65+ years olds (HES)")

## gastro hosp fit to observation data (age 0-4)

gastro_gp1_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_gastro_gp1, percentile_97.5_gastro_gp1), by ="time") |> 
  left_join(observation_data, by = "time") |>
  select(week_date, gastro_gp_model_1, percentile_2.5_gastro_gp1, percentile_97.5_gastro_gp1,  gastro_gp_obs_1) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = percentile_2.5_gastro_gp1, ymax = percentile_97.5_gastro_gp1, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(y = gastro_gp_obs_1, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = gastro_gp_model_1, linetype = "Model"), color = "red", show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Incidence per 100,000 p-yrs") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, title = "Gastroenteritis primary care attendance, 0-4 year olds (CPRD)")

## gastro hosp fit to observation data (age 5-14)

gastro_gp2_fit_points <- traj_median |>
  left_join(traj_summary |> select(time, percentile_2.5_gastro_gp2, percentile_97.5_gastro_gp2), by ="time") |> 
  left_join(observation_data, by = "time") |>
  select(week_date, gastro_gp_model_2, percentile_2.5_gastro_gp2, percentile_97.5_gastro_gp2,  gastro_gp_obs_2) |> 
  ggplot(aes(x = week_date)) +
  geom_ribbon(aes(ymin = percentile_2.5_gastro_gp2, ymax = percentile_97.5_gastro_gp2, fill = "Ribbon"), alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(y = gastro_gp_obs_2, color = "Points"), size = 2, show.legend = FALSE) +
  geom_line(aes(y = gastro_gp_model_2, linetype = "Model"), color = "red", show.legend = FALSE) +
  theme_classic() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 75), breaks = seq(0, 75, by = 25)) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.2,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  ylab("Incidence per 100,000 p-yrs") +
  scale_color_manual(name = NULL, values = c("Points" = "black"), labels = "Data") +
  scale_fill_manual(name = NULL, values = "blue", labels = "95% CrI") + # No need for a legend title for the ribbon
  theme(title = element_text(size = 10),
    axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right",
        legend.spacing.y = unit(-0.09, 'cm')) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 2),
         fill = guide_legend(order = 3)) +
  labs(linetype = NULL, 
       title = "Gastroenteritis primary care attendance, 5-14 year olds (CPRD)")

ggarrange(noro_1_fit_points, noro_2_fit_points, noro_3_fit_points, noro_4_fit_points, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

ggsave("figures/multi_panel_figure_1.png", width = 12, height = 14)
ggsave("figures/multi_panel_figure_1.pdf", width = 12, height = 14)

ggarrange(gastro_gp1_fit_points, gastro_gp2_fit_points, gastro_fit_points, aki_fit_points, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

ggsave("figures/multi_panel_figure_2.png", width = 12, height = 14)
ggsave("figures/multi_panel_figure_2.pdf", width = 12, height = 14)

ggarrange(noro_1_fit_points, noro_2_fit_points, noro_3_fit_points, noro_4_fit_points, gastro_gp1_fit_points, gastro_gp2_fit_points, gastro_fit_points, aki_fit_points, ncol = 2, nrow = 4, labels = c("A", "B", "C", "D"))

ggsave("figures/multi_panel_figure.png", width = 12, height = 14)


incidence_fit

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

#' #### logDensity

xyplot(multi_trace_log_density)

traceBurnThin_params_multi <- burnAndThin(multi_trace_params, burn = burn_value, thin = thin_factor)

#' ## multiple chains without burn in

xyplot(traceBurnThin_params_multi)

