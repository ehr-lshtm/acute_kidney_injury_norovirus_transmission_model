### reference costs


reference_costs <- fread("data/aki_reference_costs.csv") 

mean_yearly_cost <- reference_costs |>
  mutate(observation_year = as.double(case_when(
    year == "2012-2013" ~ "2012",
    year == "2013-2014" ~ "2013",
    year == "2014-2015" ~ "2014",
    year == "2015-2016" ~ "2015",
    year == "2016-2017" ~ "2016",
    year == "2017-2018" ~ "2017",
    year == "2018-2019" ~ "2018",
    year == "2019-2020" ~ "2019",
  ))) |>
  group_by(observation_year) |>
  # group_by(year) |> 
  summarise(total_cost = sum(total_cost),
            total_activity = sum(activity),
            mean_cost_per_activity = total_cost/total_activity) |> 
  ungroup() |> 
  filter(observation_year!= 2012)

cost_per_year <- traj_median |> 
  left_join(observation_data |> select(time, week, week_date), by = "time") |>
  mutate(year = year(week_date),
         seasonal_param = (0.5 * (1 + cos(2 * pi * (week - 1)/52))) * exp(theta$aki_hospitalisation_4_winter) +
           (0.5 * (1 - cos(2 * pi * (week - 1)/52))) * 0,
         seasonal_param_min = (0.5 * (1 + cos(2 * pi * (week - 1)/52))) * exp(summary_stats$quantiles[8,1]) +
           (0.5 * (1 - cos(2 * pi * (week - 1)/52))) * 0,
         seasonal_param_max = (0.5 * (1 + cos(2 * pi * (week - 1)/52))) * exp(summary_stats$quantiles[8,5]) +
           (0.5 * (1 - cos(2 * pi * (week - 1)/52))) * 0,
         aki_linked = infectious_symp_4_count*seasonal_param,
         aki_linked_min = infectious_symp_4_count*seasonal_param_min,
         aki_linked_max = infectious_symp_4_count*seasonal_param_max) |> 
  group_by(year) |>
  summarise(infectious_symp_4_total = round(sum(infectious_symp_4_count), digits = 0),
            aki_total = round(sum(aki_linked), digits = 0),
            aki_min = round(sum(aki_linked_min), digits = 0),
            aki_max = round(sum(aki_linked_max), digits = 0)) |> 
  ungroup() |> 
  left_join(mean_yearly_cost |> select(observation_year, mean_cost_per_activity), by = c("year" = "observation_year")) |>
  mutate(total_cost_year = round((aki_total* mean_cost_per_activity) / 1000000, digits = 0),
         min_cost_year = round((aki_min* mean_cost_per_activity) / 1000000, digits = 0),
         max_cost_year = round((aki_max* mean_cost_per_activity)  / 1000000, digits = 0),
         mean_cost_per_activity = round(mean_cost_per_activity, digits = 0),
         cost_2021 = case_when(
           year == "2013" ~ round(total_cost_year * (1.1493), digits = 0),
           year == "2014" ~ round(total_cost_year * (1.1323), digits = 0),
           year == "2015" ~ round(total_cost_year * (1.1213), digits = 0),
           year == "2016" ~ round(total_cost_year * (1.1123), digits = 0),
           year == "2017" ~ round(total_cost_year * (1.1088), digits = 0),
           year == "2018" ~ round(total_cost_year * (1.0876), digits = 0),
           year == "2019" ~ round(total_cost_year * (1.076), digits = 0)
         ),
         min_cost_2021 = case_when(
           year == "2013" ~ round(min_cost_year * (1.1493), digits = 0),
           year == "2014" ~ round(min_cost_year * (1.1323), digits = 0),
           year == "2015" ~ round(min_cost_year * (1.1213), digits = 0),
           year == "2016" ~ round(min_cost_year * (1.1123), digits = 0),
           year == "2017" ~ round(min_cost_year * (1.1088), digits = 0),
           year == "2018" ~ round(min_cost_year * (1.0876), digits = 0),
           year == "2019" ~ round(min_cost_year * (1.076), digits = 0)
         ),
         max_cost_2021 = case_when(
           year == "2013" ~ round(max_cost_year * (1.1493), digits = 0),
           year == "2014" ~ round(max_cost_year * (1.1323), digits = 0),
           year == "2015" ~ round(max_cost_year * (1.1213), digits = 0),
           year == "2016" ~ round(max_cost_year * (1.1123), digits = 0),
           year == "2017" ~ round(max_cost_year * (1.1088), digits = 0),
           year == "2018" ~ round(max_cost_year * (1.0876), digits = 0),
           year == "2019" ~ round(max_cost_year * (1.076), digits = 0)
         ))

total_cost <- cost_per_year |> 
  summarise(year = "Total",
            infectious_symp_4_total = round(sum(infectious_symp_4_total)),
            aki_total = round(sum(aki_total), digits = 0),
            aki_min = round(sum(aki_min), digits = 0),
            aki_max = round(sum(aki_max), digits = 0),
            mean_cost_per_activity = "",
            total_cost_year = sum(total_cost_year),
            min_cost_year  = sum(min_cost_year ),
            max_cost_year = sum(max_cost_year),
            cost_2021 = sum(cost_2021),
            min_cost_2021  = sum(min_cost_2021 ),
            max_cost_2021 = sum(max_cost_2021)
  )

cost_per_year_signif <- cost_per_year |>
  mutate(infectious_symp_4_total = infectious_symp_4_total / 1000,
         aki_total = aki_total / 1000,
         aki_min = round(aki_min / 1000, digits = 0),
         aki_max = aki_max / 1000,
         across(-1, ~ signif(.x, 3)))

total_cost_signif <- total_cost |> 
  mutate(infectious_symp_4_total = infectious_symp_4_total / 1000,
         aki_total = aki_total / 1000,
         aki_min = aki_min / 1000,
         aki_max = aki_max / 1000,
         across(-c(1, 6), ~ round(.x, digits = 0)))

