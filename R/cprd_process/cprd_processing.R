source("paths/00_filepath.R")
source("R/01_setup.R")

### cprd processing for HES primary gastro aurum

icd10_code_list <- fread("codelists/icd10_code_list.txt")

a_hes_primary_diag_hosp <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_primary_diag_hosp_23_003034_DM.parquet"))

all_pat_denominator_aurum <- fread(file.path(all_pat_denominator_path, "2024_03_CPRDAurum_AllPats.txt")) |>  
  select(patid, gender, yob) |>  
  mutate(patid = as.character(patid))

# HES primary gastro aurum

a_hes_primary_gastro_spell <- a_hes_primary_diag_hosp %>%
  left_join(icd10_code_list, by = c("ICD_PRIMARY" = "icd")) %>%
  filter(group == "Gastrointestinal infectious and non-infectious illnesses") %>%
  distinct(patid, spno, ICD_PRIMARY, description, admidate) |>
  left_join(all_pat_denominator_aurum, by = "patid") %>%
  mutate(
    ICD3 = substr(ICD_PRIMARY, 1, 3),
    ICD_group = case_when(
      ICD3 == "A00" ~ "A00-A08",
      ICD3 == "A01" ~ "A00-A08",
      ICD3 == "A02" ~ "A00-A08",
      ICD3 == "A03" ~ "A00-A08",
      ICD3 == "A04" ~ "A00-A08",
      ICD3 == "A05" ~ "A00-A08",
      ICD3 == "A06" ~ "A00-A08",
      ICD3 == "A07" ~ "A00-A08",
      ICD3 == "A08" ~ "A00-A08",
      ICD3 == "A09" ~ "A09",
      ICD3 == "K52" ~ "K52",
      ICD3 == "R11" ~ "R11",
    ),
    admidate = dmy(admidate),
    year = year(admidate),
    age_at_admission = year - yob,
    age_group_4cat = case_when(
      age_at_admission < 5 ~ "0-4",
      age_at_admission > 5 & age_at_admission < 15 ~ "5-14",
      age_at_admission > 14 & age_at_admission < 65 ~ "15-64",
      age_at_admission > 64 ~ "65+"
    ),
    gender = case_when(gender == "M" ~ "Male",
                       gender == "F" ~ "Female")
  ) %>% 
  filter(year > 2008, year < 2020) %>% 
  write_parquet(file.path(data_files_path, "a_hes_primary_gastro_spell.parquet"))

remove(a_hes_primary_diag_hosp)

## cprd processing for HES secondary gastro aurum 

a_hes_hosp <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_hospital_23_003034_DM.parquet")) %>% 
  select(patid, spno, admidate)

a_hes_diagnosis_epi <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_diagnosis_epi_23_003034_DM.parquet")) %>% 
  select(patid, spno, ICD, d_order) %>% 
  mutate(patid = as.character(patid))

a_hes_secondary_gastro_spell <- a_hes_diagnosis_epi %>%
  left_join(icd10_code_list, by = c("ICD" = "icd")) %>%
  filter(group == "Gastrointestinal infectious and non-infectious illnesses",
         d_order > 1,
         ICD != "R11") %>%                                                     # Removed R11 (vomiting and diarrhoea)
  left_join(a_hes_hosp, by = c("patid", "spno")) %>%
  distinct(patid, spno, ICD, description, admidate) |>
  left_join(all_pat_denominator_aurum, by = "patid") %>%
  mutate(
    ICD3 = substr(ICD, 1, 3),
    ICD_group = case_when(
      ICD3 == "A00" ~ "A00-A08",
      ICD3 == "A01" ~ "A00-A08",
      ICD3 == "A02" ~ "A00-A08",
      ICD3 == "A03" ~ "A00-A08",
      ICD3 == "A04" ~ "A00-A08",
      ICD3 == "A05" ~ "A00-A08",
      ICD3 == "A06" ~ "A00-A08",
      ICD3 == "A07" ~ "A00-A08",
      ICD3 == "A08" ~ "A00-A08",
      ICD3 == "A09" ~ "A09",
      ICD3 == "K52" ~ "K52",
      ICD3 == "R11" ~ "R11",
    ),
    admidate = dmy(admidate),
    year = year(admidate),
    age_at_admission = year - yob,
    age_group_4cat = case_when(
      age_at_admission < 5 ~ "0-4",
      age_at_admission > 5 & age_at_admission < 15 ~ "5-14",
      age_at_admission > 14 & age_at_admission < 65 ~ "15-64",
      age_at_admission > 64 ~ "65+"
    ),
    gender = case_when(gender == "M" ~ "Male",
                       gender == "F" ~ "Female")
  ) %>% 
  filter(year > 2008, year < 2020) %>% 
  write_parquet(file.path(data_files_path, "a_hes_secondary_gastro_spell.parquet"))

## cprd processing for HES gastro aurum (primary and secondary) 

a_hes_hosp <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_hospital_23_003034_DM.parquet")) %>% 
  select(patid, spno, admidate)

a_hes_diagnosis_epi <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_diagnosis_epi_23_003034_DM.parquet")) %>% 
  select(patid, spno, ICD, d_order, epistart) %>% 
  mutate(patid = as.character(patid))

a_hes_gastro_spell <- a_hes_diagnosis_epi |> 
  left_join(icd10_code_list, by = c("ICD" = "icd")) |> 
  filter(group == "Gastrointestinal infectious and non-infectious illnesses",
         ICD != "R11")  |>                                                      # Removed R11 (vomiting and diarrhoea)
  left_join(a_hes_hosp, by = c("patid", "spno"))  |>  
  distinct(patid, spno, ICD, description, admidate) |>
  left_join(all_pat_denominator_aurum, by = "patid")  |> 
  mutate(
    ICD3 = substr(ICD, 1, 3),
    ICD_group = case_when(
      ICD3 == "A00" ~ "A00-A08",
      ICD3 == "A01" ~ "A00-A08",
      ICD3 == "A02" ~ "A00-A08",
      ICD3 == "A03" ~ "A00-A08",
      ICD3 == "A04" ~ "A00-A08",
      ICD3 == "A05" ~ "A00-A08",
      ICD3 == "A06" ~ "A00-A08",
      ICD3 == "A07" ~ "A00-A08",
      ICD3 == "A08" ~ "A00-A08",
      ICD3 == "A09" ~ "A09",
      ICD3 == "K52" ~ "K52",
      ICD3 == "R11" ~ "R11",
    ),
    admidate = dmy(admidate),
    year = year(admidate),
    age_at_admission = year - yob,
    gender = case_when(gender == "M" ~ "Male",
                       gender == "F" ~ "Female")
  ) %>% 
  filter(year > 2012, 
         year < 2020,
         !is.na(age_at_admission)) %>% 
  write_parquet(file.path(data_files_path, "a_hes_gastro_spell.parquet"))

####### AKI aurum

## cprd processing for HES secondary gastro aurum 

a_hes_hosp <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_hospital_23_003034_DM.parquet")) %>% 
  select(patid, spno, admidate)

a_hes_diagnosis_epi <- read_parquet(file.path(rawdata_hes_files_path, "parquethes_diagnosis_epi_23_003034_DM.parquet")) %>% 
  select(patid, spno, epikey, epistart, ICD, d_order) %>% 
  mutate(patid = as.character(patid))

a_hes_aki_spell <- a_hes_diagnosis_epi %>%
  left_join(icd10_code_list, by = c("ICD" = "icd")) |> 
  filter(group == "Acute Kidney Injury",
         ICD!= "N19") |> 
  left_join(a_hes_hosp, by = c("patid", "spno")) |>  
  left_join(all_pat_denominator_aurum, by = "patid")  |> 
  mutate(
    ICD3 = substr(ICD, 1, 3),
    epistart = dmy(epistart),
    admidate = dmy(admidate),
    year = year(admidate),
    aki_day = epistart - admidate,
    d_order_2 = case_when(d_order == 1 ~ "Primary",
                          d_order == 2 ~ "Secondary",
                          d_order > 2 ~ "Tertiary",
                          ),
    d_order_2 = factor(d_order_2, levels = c("Primary", "Secondary", "Tertiary")),
    age_at_admission = year - yob,
    gender = case_when(gender == "1" ~ "Male",
                    gender == "2" ~ "Female"),
    aki_day_3 = case_when(aki_day < 4 ~ "0-3",
                          aki_day > 3 & aki_day < 8 ~ "4-7",
                          aki_day > 7 ~ ">7"),
    aki_day_3 = factor(aki_day_3, levels = c("0-3", "4-7", ">7")),
  ) |> 
  group_by(patid, spno) |> 
  arrange(epistart) |> 
  mutate(id = row_number()) |>          # selecting first episode of AKI in the admission
  ungroup() |> 
  filter(year > 2012,
         year < 2020)

a_hes_aki_spell |> 
  filter(id == 1,
         !is.na(aki_day_3),
         !is.na(age_at_admission)) |>
  write_parquet(file.path(data_files_path, "a_hes_aki_spell.parquet"))

a_hes_aki_spell |>
  filter(id == 1,
         aki_day_3 == "0-3",
         !is.na(age_at_admission)) |>
  write_parquet(file.path(data_files_path, "a_hes_aki_spell_community.parquet"))
