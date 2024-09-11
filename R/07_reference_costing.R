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
  left_join(observation_data, by = "time") |>
  select(week_date, infectious_symp_4_count) |> 
  mutate(year = year(week_date)) |>
  # mutate(year = case_when(
  #   week_date > as.Date("2013-06-30") & week_date < as.Date("2014-07-01") ~ "2013-2014",
  #   week_date > as.Date("2014-06-30") & week_date < as.Date("2015-07-01") ~ "2014-2015",
  #   week_date > as.Date("2015-06-30") & week_date < as.Date("2016-07-01") ~ "2015-2016",
  #   week_date > as.Date("2016-06-30") & week_date < as.Date("2017-07-01") ~ "2016-2017",
  #   week_date > as.Date("2017-06-30") & week_date < as.Date("2018-07-01") ~ "2017-2018",
  #   week_date > as.Date("2018-06-30") & week_date < as.Date("2019-07-01") ~ "2018-2019")
  #   ) |> 
  group_by(year) |> 
  summarise(infectious_symp_4_total = round(sum(infectious_symp_4_count), digits = 0)) |> 
  ungroup() |> 
  filter(!is.na(year)) |> 
  left_join(mean_yearly_cost, by = c("year" = "observation_year")) |>
  # left_join(mean_yearly_cost, by = "year") |> 
  mutate(aki_total = round(infectious_symp_4_total * exp(theta$aki_hospitalisation_4), digits = 0),
         aki_min = round(infectious_symp_4_total * exp(summary_stats$quantiles[9,1]), digits = 0),     
         aki_max = round(infectious_symp_4_total * exp(summary_stats$quantiles[9,5]), digits = 0),
         total_cost_year = round((aki_total* mean_cost_per_activity) / 1000000), digits = 0,
         min_cost_year = round((aki_min* mean_cost_per_activity) / 1000000), digits = 0,
         max_cost_year = round((aki_max* mean_cost_per_activity)  / 1000000), digits = 0,
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
         )
         # cost_2021 = case_when(
         #   year == "2013-2014" ~ round(total_cost_year * (1.1323), digits = 0),
         #   year == "2014-2015" ~ round(total_cost_year * (1.1213), digits = 0),
         #   year == "2015-2016" ~ round(total_cost_year * (1.1123), digits = 0),
         #   year == "2016-2017" ~ round(total_cost_year * (1.1088), digits = 0),
         #   year == "2017-2018" ~ round(total_cost_year * (1.0876), digits = 0),
         #   year == "2018-2019" ~ round(total_cost_year * (1.076), digits = 0)
         # ),
         # min_cost_2021 = case_when(
         #   year == "2013-2014" ~ round(min_cost_year * (1.1323), digits = 0),
         #   year == "2014-2015" ~ round(min_cost_year * (1.1213), digits = 0),
         #   year == "2015-2016" ~ round(min_cost_year * (1.1123), digits = 0),
         #   year == "2016-2017" ~ round(min_cost_year * (1.1088), digits = 0),
         #   year == "2017-2018" ~ round(min_cost_year * (1.0876), digits = 0),
         #   year == "2018-2019" ~ round(min_cost_year * (1.076), digits = 0)
         # ),
         # max_cost_2021 = case_when(
         #   year == "2013-2014" ~ round(max_cost_year * (1.1323), digits = 0),
         #   year == "2014-2015" ~ round(max_cost_year * (1.1213), digits = 0),
         #   year == "2015-2016" ~ round(max_cost_year * (1.1123), digits = 0),
         #   year == "2016-2017" ~ round(max_cost_year * (1.1088), digits = 0),
         #   year == "2017-2018" ~ round(max_cost_year * (1.0876), digits = 0),
         #   year == "2018-2019" ~ round(max_cost_year * (1.076), digits = 0)
         # )
         ) |> 
  select(year, infectious_symp_4_total, aki_total, aki_min, aki_max, mean_cost_per_activity, total_cost_year, min_cost_year, max_cost_year, cost_2021, min_cost_2021, max_cost_2021)


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

