# preparation of patient list for CPRD in order to receive HES linkage data

#############
### AURUM ###
#############

# ai gastro define - Aurum

aki_gastro_define_1 <- fread(file.path(rawdata_cprd_define_path, "aki_gastro_Define_Inc1_Observation_001.txt")) |> 
  select(patid, pracid, obsdate, medcodeid)

aki_gastro_define_1 |> 
  write_parquet(file.path(rawdata_cprd_define_path, "aki_gastro_define_1.parquet"))

aki_gastro_define_2 <- fread(file.path(rawdata_cprd_define_path, "aki_gastro_Define_Inc1_Observation_002.txt")) |> 
  select(patid, pracid, obsdate, medcodeid)

aki_gastro_define_2 |> 
  write_parquet(file.path(rawdata_cprd_define_path, "aki_gastro_define_2.parquet"))

aki_gastro_define_3 <- fread(file.path(rawdata_cprd_define_path, "aki_gastro_Define_Inc1_Observation_003.txt")) |> 
  select(patid, pracid, obsdate, medcodeid)

aki_gastro_define_3 |> 
  write_parquet(file.path(rawdata_cprd_define_path, "aki_gastro_define_3.parquet"))

# AURUM denominator file

all_pat_denominator_aurum <- fread(file.path(all_pat_denominator_path, "202309_CPRDAurum_AcceptablePats.txt")) |> 
  select(patid, gender, yob, regenddate, lcd, acceptable) |>  
  mutate(patid = as.character(patid),
         regenddate = dmy(regenddate),
         regendyear = year(regenddate))


# # linkage eligibility
# 
# linkage_eligibility_aurum <- fread(file.path(linkage_path, "Aurum_enhanced_eligibility_January_2022.txt")) |>
#   filter(hes_apc_e == 1) |> 
#   select(patid, hes_apc_e) |>  
#   mutate(patid = as.character(patid))



aki_gastro_define_1 <- read_parquet(file.path(rawdata_cprd_define_path, "aki_gastro_define_1.parquet"))
aki_gastro_define_2 <- read_parquet(file.path(rawdata_cprd_define_path, "aki_gastro_define_2.parquet"))
aki_gastro_define_3 <- read_parquet(file.path(rawdata_cprd_define_path, "aki_gastro_define_3.parquet"))

gastro_aurum_medcodes <-
  fread("data/gastro_aurum_code_list.txt")  |>
  mutate(
    code_type = case_when(
      substr(OriginalReadCode, 1, 1) == "A" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "H" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "4" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "9" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "6" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "^" ~ "diagnosis",
      OriginalReadCode == "EMISNQNO180" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "R" ~ "symptom",
      substr(OriginalReadCode, 1, 1) == "Q" ~ "symptom",
      substr(OriginalReadCode, 1, 1) == "1" ~ "symptom",
      OriginalReadCode == "EMISCDI58" ~ "symptom",
      OriginalReadCode == "EGTON6" ~ "diagnosis",
      OriginalReadCode == "J43-1" ~ "diagnosis",
      OriginalReadCode == "J43-2" ~ "diagnosis",
      OriginalReadCode == "J6A" ~ "diagnosis",
      OriginalReadCode == "J4zz-1" ~ "symptom",
      OriginalReadCode == "J162z" ~ "symptom",
      OriginalReadCode == "J1620" ~ "symptom",
      OriginalReadCode == "J162" ~ "symptom"
      
      
      )
  )

aki_aurum_medcodes <- fread("data/aki_aurum_code_list.txt") |> 
  mutate(
    code_type = case_when(
      substr(OriginalReadCode, 1, 1) == "^" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "K" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "7" ~ "dialysis",
      substr(OriginalReadCode, 1, 1) == "Z" ~ "dialysis",
      substr(OriginalReadCode, 1, 1) == "4" ~ "warning",
      substr(OriginalReadCode, 1, 1) == "S" ~ "diagnosis",
      substr(OriginalReadCode, 1, 1) == "L" ~ "diagnosis"
    )
  )

# gastro events

gastro_define_1 <- aki_gastro_define_1 |> 
  inner_join(gastro_aurum_medcodes, by = c("medcodeid" = "MedCodeId"))
gastro_define_2 <- aki_gastro_define_2 |> 
  inner_join(gastro_aurum_medcodes, by = c("medcodeid" = "MedCodeId"))
gastro_define_3 <- aki_gastro_define_3 |> 
  inner_join(gastro_aurum_medcodes, by = c("medcodeid" = "MedCodeId"))

gastro_define <- gastro_define_1 |> 
  bind_rows(gastro_define_2) |>  
  bind_rows(gastro_define_3) |> 
  mutate(patid = as.character(patid)) |> 
  left_join(all_pat_denominator_aurum, by = "patid")

remove(gastro_define_1)
remove(gastro_define_2)
remove(gastro_define_3)

gastro_aurum_events <- gastro_define |>
  distinct() |>
  mutate(obsdate = dmy(obsdate),
         obsyear = year(obsdate),
         week_number = week(obsdate),
         epiweek_number = epiweek(obsdate),
         isoweek_number = isoweek(obsdate),
         year = year(obsdate),
         week_date = floor_date(obsdate, "week", week_start = 7),
         week_floor_number = epiweek(week_date),
         age_at_attendance = obsyear - yob,
         age_group = case_when(
           age_at_attendance < 5 ~ "0-4",
           age_at_attendance > 4 & age_at_attendance < 15 ~ "5-14",
           age_at_attendance > 14 & age_at_attendance < 65 ~ "15-64",
           age_at_attendance > 64 ~ "65+"),
         age_group = factor(age_group, level = c("0-4", "5-14","15-64", "65+"))
  ) |> 
  filter(obsdate > "2012-12-29" & obsdate < "2020-01-05") 

gastro_aurum_events |> 
  write_parquet(file.path(data_files_path, "gastro_aurum_events.parquet"))

##############  
# aki events
#############

aki_define_1 <- aki_gastro_define_1 |> 
  inner_join(aki_aurum_medcodes, by = c("medcodeid" = "MedCodeId"))
aki_define_2 <- aki_gastro_define_2 |> 
  inner_join(aki_aurum_medcodes, by = c("medcodeid" = "MedCodeId"))
aki_define_3 <- aki_gastro_define_3 |> 
  inner_join(aki_aurum_medcodes, by = c("medcodeid" = "MedCodeId"))

aki_define <- aki_define_1 |> 
  bind_rows(aki_define_2) |>  
  bind_rows(aki_define_3) |> 
  mutate(patid = as.character(patid)) |> 
  left_join(all_pat_denominator_aurum, by = "patid")

remove(aki_define_1)
remove(aki_define_2)
remove(aki_define_3)

aki_aurum_events <- aki_define |>
  distinct() |> 
  mutate(obsdate = dmy(obsdate),
         obsyear = year(obsdate),
         week_number = week(obsdate),
         epiweek_number = epiweek(obsdate),
         isoweek_number = isoweek(obsdate),
         year = year(obsdate),
         week_date = floor_date(obsdate, "week", week_start = 7),
         week_floor_number = epiweek(week_date),
         age_at_attendance = obsyear - yob,
         age_group = case_when(
           age_at_attendance < 5 ~ "0-4",
           age_at_attendance > 4 & age_at_attendance < 15 ~ "5-14",
           age_at_attendance > 14 & age_at_attendance < 65 ~ "15-64",
           age_at_attendance > 64 ~ "65+"),
         age_group = factor(age_group, level = c("0-4", "5-14","15-64", "65+"))
  ) |> 
  filter(obsdate > "2012-12-29" & obsdate < "2020-01-05")

aki_aurum_events |> 
  write_parquet(file.path(data_files_path, "aki_aurum_events.parquet"))

##############
