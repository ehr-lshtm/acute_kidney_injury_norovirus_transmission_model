# observation data

source("paths/00_filepath.R")

###################
## denominator data
###################

cprd_annual_denominator <- read_parquet(file.path(data_files_path, "cprd_annual_denominator.parquet")) |> 
  as.data.frame() |> 
  rename('2013' = denom_2013,
         '2014' = denom_2014,
         '2015' = denom_2015,
         '2016' = denom_2016,
         '2017' = denom_2017,
         '2018' = denom_2018,
         '2019' = denom_2019,
         '2020' = denom_2020,
         '2021' = denom_2021
  ) |>
  pivot_longer(cols = !age_group, names_to = "year", values_to = "denom_n") |>
  filter(!is.na(age_group)) |>
  mutate(year = as.double(year))

hes_annual_denominator <- read_parquet(file.path(data_files_path, "hes_annual_denominator.parquet")) |> 
  as.data.frame() |> 
  rename('2013' = denom_2013,
         '2014' = denom_2014,
         '2015' = denom_2015,
         '2016' = denom_2016,
         '2017' = denom_2017,
         '2018' = denom_2018,
         '2019' = denom_2019,
         '2020' = denom_2020,
         '2021' = denom_2021
         ) |>
  pivot_longer(cols = !age_group, names_to = "year", values_to = "denom_n") |>
  filter(!is.na(age_group)) |>
  mutate(year = as.double(year))

###################  
## IID2 incidence
###################

age_incidence <- fread("data/age_incidence_fitting.csv") |> 
  filter(study == "Harris et al.") |> 
  rename(harris_incidence = incidence) |> 
  select(age, harris_incidence) |> 
  mutate(harris_incidence = round(harris_incidence))

###################
## noro surveillance time series
###################

noro_data <- fread(file.path(noro_data_path, "Norovirus_weekly_counts_England__2013-2019.txt")) |> 
  mutate(time = seq(from = 0, to = 364, by = 1)) |> 
  rename(noro_obs_1 = `0-4`,
         noro_obs_2 = `5-14`,
         noro_obs_3 = `15-64`,
         noro_obs_4 = `65+`
         ) |> 
  select(time, noro_obs_1, noro_obs_2, noro_obs_3, noro_obs_4)

###################
## aki hospitalisation data for visualisation
###################

aki_hosp_spell <- read_parquet(file.path(data_files_path, "a_hes_aki_spell_community.parquet")) |>
# aki_hosp_spell <- fread(file.path(data_files_path, "a_hes_aki_spell.txt")) |>
    mutate(age_group = case_when(
    age_at_admission < 5 ~ "aki_hosp_obs_1",
    age_at_admission > 4 & age_at_admission < 15 ~ "aki_hosp_obs_2",
    age_at_admission > 14 & age_at_admission < 65 ~ "aki_hosp_obs_3",
    age_at_admission > 64 ~ "aki_hosp_obs_4"
  )) |> 
  group_by(age_group) |> 
  count(week_date = floor_date(admidate, "week", week_start = 7)) |>
  ungroup() |>
  filter(week_date > "2012-12-30" & week_date <= "2019-12-29") |> 
  mutate(year = year(week_date)) |> 
  # convert to incidence using hes denominator
  left_join(hes_annual_denominator |> 
              mutate(age_group = case_when(
                age_group == "0-4" ~ "aki_hosp_obs_1",
                age_group == "5-14" ~ "aki_hosp_obs_2",
                age_group == "15-64" ~ "aki_hosp_obs_3",
                age_group == "65+" ~ "aki_hosp_obs_4",
              )), by = c("age_group", "year")) |> 
  mutate(aki_hosp_incidence = round((n/denom_n)*100000, digits = 2)) |> 
  select(week_date, age_group, aki_hosp_incidence) |> 
  pivot_wider(names_from = age_group, values_from = aki_hosp_incidence) |> 
  arrange(week_date) |> 
  mutate(time = seq(from = 0, to = 364, by = 1))

###################
## gastro hospitalisation data for visualisation
###################

gastro_hosp_spell <- read_parquet(file.path(data_files_path, "a_hes_gastro_spell.parquet")) |>
  filter(ICD_group != "K52") %>%
  mutate(
    age_group = case_when(
      age_at_admission < 5 ~ "gastro_hosp_obs_1",
      age_at_admission > 4 & age_at_admission < 15 ~ "gastro_hosp_obs_2",
      age_at_admission > 14 & age_at_admission < 65 ~ "gastro_hosp_obs_3",
      age_at_admission > 64 ~ "gastro_hosp_obs_4"
    )) |>
  group_by(age_group) |>
  count(week_date = floor_date(admidate, "week", week_start = 7)) |>
  ungroup() |>
  filter(week_date > "2012-12-30" & week_date <= "2019-12-29") |>
  mutate(n = case_when(
    age_group == "gastro_hosp_obs_1" & week_date < "2014-01-05" ~ NA_real_,
    TRUE ~ n
  ), year = year(week_date)
  ) |> 
  # convert to incidence using hes denominator
    left_join(hes_annual_denominator |> 
              mutate(age_group = case_when(
                age_group == "0-4" ~ "gastro_hosp_obs_1",
                age_group == "5-14" ~ "gastro_hosp_obs_2",
                age_group == "15-64" ~ "gastro_hosp_obs_3",
                age_group == "65+" ~ "gastro_hosp_obs_4",
              )), by = c("age_group", "year")) |> 
  mutate(gastro_hosp_incidence = round((n/denom_n)*100000, digits = 2)) |> 
  select(week_date, age_group, gastro_hosp_incidence) |> 
  pivot_wider(names_from = age_group, values_from = gastro_hosp_incidence) |> 
  arrange(week_date) |> 
  mutate(time = seq(from = 0, to = 364, by = 1)) |>
  select(-week_date) 

###################
## gastro gp attendance for visualising fit
###################

gastro_gp_attendance <- read_parquet(file.path(data_files_path, "gastro_aurum_events.parquet")) |> 
  filter(acceptable == 1,
         code_type == "diagnosis") |> 
  distinct(patid, pracid, obsdate, age_group) |> 
  group_by(age_group) |>
  count(week_date = floor_date(obsdate, "week", week_start = 7)) |>
  ungroup() |>
  filter(week_date > "2012-12-30" & week_date <= "2019-12-29",
         !is.na(age_group)) |>
  mutate(age_group = case_when(
    age_group == "0-4" ~ "gastro_gp_obs_1",
    age_group == "5-14" ~ "gastro_gp_obs_2",
    age_group == "15-64" ~ "gastro_gp_obs_3",
    age_group == "65+" ~ "gastro_gp_obs_4",
  ), 
  n = case_when(
    age_group == "gastro_gp_obs_1" & week_date < "2014-01-05" ~ NA_real_,
    TRUE ~ n
  ),
  year = year(week_date)
  ) |>
  # convert to incidence using cprd denominator
  left_join(cprd_annual_denominator |> 
              mutate(age_group = case_when(
                age_group == "0-4" ~ "gastro_gp_obs_1",
                age_group == "5-14" ~ "gastro_gp_obs_2",
                age_group == "15-64" ~ "gastro_gp_obs_3",
                age_group == "65+" ~ "gastro_gp_obs_4",
              )), by = c("age_group", "year")) |> 
  mutate(gastro_gp_incidence = round((n/denom_n)*100000, digits = 2)) |> 
  select(week_date, age_group, gastro_gp_incidence) |> 
  pivot_wider(names_from = age_group, values_from = gastro_gp_incidence) |> 
  arrange(week_date) |> 
  mutate(time = seq(from = 0, to = 364, by = 1)) |>
  select(-week_date)

###################
## combine data
###################

observation_data <- noro_data |> 
  left_join(aki_hosp_spell, by = "time") |> 
  left_join(gastro_hosp_spell, by = "time") |> 
  left_join(gastro_gp_attendance, by = "time") |> 
  mutate(week = week(week_date))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

###################
## aki hospitalisation data for likelihood calculation
###################

aki_hosp_spell2 <- read_parquet(file.path(data_files_path, "a_hes_aki_spell_community.parquet")) |> 
  mutate(age_group = case_when(
    age_at_admission < 5 ~ "aki_hosp_obs_1",
    age_at_admission > 4 & age_at_admission < 15 ~ "aki_hosp_obs_2",
    age_at_admission > 14 & age_at_admission < 65 ~ "aki_hosp_obs_3",
    age_at_admission > 64 ~ "aki_hosp_obs_4"
  )) |> 
  group_by(age_group) |> 
  count(week_date = floor_date(admidate, "week", week_start = 7)) |>
  ungroup() |>
  filter(week_date > "2012-12-30" & week_date <= "2019-12-29") |>
  # This mutate function identifies all periods of summer heat waves leading to increases in aki hospitalisations
  # these do not contribute to the likelihood
  mutate(n = case_when(
  age_group == "aki_hosp_obs_4" & week_date == "2013-07-07" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2013-07-14" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2013-07-21" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2013-07-28" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2015-06-28" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2016-07-17" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2016-08-21" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2016-09-11" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2017-06-11" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2017-06-18" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2017-07-02" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-06-24" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-07-01" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-07-08" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-07-22" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-07-29" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-08-05" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2019-06-23" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-07-21" ~ NA_real_,
  age_group == "aki_hosp_obs_4" & week_date == "2018-08-18" ~ NA_real_,
  TRUE ~ n
), year = year(week_date)
) |>
  # convert to incidence using hes denominator
  left_join(hes_annual_denominator |> 
              mutate(age_group = case_when(
                age_group == "0-4" ~ "aki_hosp_obs_1",
                age_group == "5-14" ~ "aki_hosp_obs_2",
                age_group == "15-64" ~ "aki_hosp_obs_3",
                age_group == "65+" ~ "aki_hosp_obs_4",
              )), by = c("age_group", "year")) |>
  # round to no decimal places for poisson likelihood calculation
  mutate(aki_hosp_incidence = round((n/denom_n)*100000)) |> 
  select(week_date, age_group, aki_hosp_incidence) |> 
  pivot_wider(names_from = age_group, values_from = aki_hosp_incidence) |> 
  arrange(week_date) |> 
  mutate(time = seq(from = 0, to = 364, by = 1))

###################
## gastro hospitalisation data for likelihood calculation
###################

gastro_hosp_spell2 <- read_parquet(file.path(data_files_path, "a_hes_gastro_spell.parquet")) |> 
  filter(ICD_group != "K52") %>% 
  mutate(age_group = case_when(
    age_at_admission < 5 ~ "gastro_hosp_obs_1",
    age_at_admission > 4 & age_at_admission < 15 ~ "gastro_hosp_obs_2",
    age_at_admission > 14 & age_at_admission < 65 ~ "gastro_hosp_obs_3",
    age_at_admission > 64 ~ "gastro_hosp_obs_4"
  )) |> 
  group_by(age_group) |> 
  count(week_date = floor_date(admidate, "week", week_start = 7)) |>
  ungroup() |> 
  filter(week_date > "2012-12-30" & week_date <= "2019-12-29") |>
  mutate(n = case_when(
    age_group == "gastro_hosp_obs_1" & week_date < "2014-01-05" ~ NA_real_,
    TRUE ~ n
  ), year = year(week_date)
  ) |> 
  # convert to incidence using hes denominator
  left_join(hes_annual_denominator |> 
              mutate(age_group = case_when(
                age_group == "0-4" ~ "gastro_hosp_obs_1",
                age_group == "5-14" ~ "gastro_hosp_obs_2",
                age_group == "15-64" ~ "gastro_hosp_obs_3",
                age_group == "65+" ~ "gastro_hosp_obs_4",
              )), by = c("age_group", "year")) |> 
  # round to no decimal places for poisson likelihood calculation
  mutate(gastro_hosp_incidence = round((n/denom_n)*100000)) |> 
  select(week_date, age_group, gastro_hosp_incidence) |> 
  pivot_wider(names_from = age_group, values_from = gastro_hosp_incidence) |> 
  arrange(week_date) |> 
  mutate(time = seq(from = 0, to = 364, by = 1)) |>
  select(-week_date) 

###################
## gastro gp attendance for likelihood calculation - excludes week 52
###################

gastro_gp_attendance2 <- read_parquet(file.path(data_files_path, "gastro_aurum_events.parquet")) |>
  filter(acceptable == 1,
         code_type == "diagnosis") |> 
  distinct(patid, pracid, obsdate, age_group) |> 
  group_by(age_group) |>
  count(week_date = floor_date(obsdate, "week", week_start = 7)) |>
  ungroup() |>
  filter(week_date > "2012-12-30" & week_date <= "2019-12-29",
         !is.na(age_group)) |>
  mutate(age_group = case_when(
    age_group == "0-4" ~ "gastro_gp_obs_1",
    age_group == "5-14" ~ "gastro_gp_obs_2",
    age_group == "15-64" ~ "gastro_gp_obs_3",
    age_group == "65+" ~ "gastro_gp_obs_4",
  ),
  n = case_when(
    age_group == "gastro_gp_obs_1" & week_date < "2014-01-05" ~ NA_real_,
    week_date == "2013-12-22" ~ NA_real_,
    week_date == "2014-12-21" ~ NA_real_,
    week_date == "2015-12-27" ~ NA_real_,
    week_date == "2016-12-25" ~ NA_real_,
    week_date == "2017-12-24" ~ NA_real_,
    week_date == "2018-12-23" ~ NA_real_,
    TRUE ~ n
  ),
  year = year(week_date)
  ) |> 
  # convert to incidence using cprd denominator
  left_join(cprd_annual_denominator |> 
              mutate(age_group = case_when(
                age_group == "0-4" ~ "gastro_gp_obs_1",
                age_group == "5-14" ~ "gastro_gp_obs_2",
                age_group == "15-64" ~ "gastro_gp_obs_3",
                age_group == "65+" ~ "gastro_gp_obs_4",
              )), by = c("age_group", "year")) |>
  # round to no decimal places for poisson likelihood calculation
  mutate(gastro_gp_incidence = round((n/denom_n)*100000)) |> 
  select(week_date, age_group, gastro_gp_incidence) |> 
  pivot_wider(names_from = age_group, values_from = gastro_gp_incidence) |> 
  mutate(time = seq(from = 0, to = 364, by = 1)) |>
  arrange(week_date) |> 
  select(-week_date)

###################
## combine data for likelihood calculation
###################

observation_data2 <- noro_data |> 
  left_join(aki_hosp_spell2, by = "time") |> 
  left_join(gastro_hosp_spell2, by = "time") |> 
  left_join(gastro_gp_attendance2, by = "time")

## gastro noro hosp data (sandmann et al.)

# hosp_weekly_primary <- fread("data/hospital_primary.csv") |>  
#   mutate(date = dmy(x),
#          week_date = floor_date(date, "week", week_start = 7)) |> 
#   select(week_date, y) %>% 
#   rename(data_count = y) %>%
#   group_by(week_date) %>% 
#   mutate(id = row_number()) %>%
#   ungroup() %>% 
#   filter(id == 1) %>% 
#   select(-id)
# 
# hosp_weekly_secondary <- fread("data/hospital_secondary.csv") %>% 
#   mutate(date = dmy(x),
#          week_date = floor_date(date, "week", week_start = 7)) %>%
#   select(week_date, y) %>% 
#   rename(data_count = y) %>% 
#   group_by(week_date) %>% 
#   mutate(id = row_number()) %>%
#   ungroup() %>% 
#   filter(id == 1) %>% 
#   select(-id)
# 
