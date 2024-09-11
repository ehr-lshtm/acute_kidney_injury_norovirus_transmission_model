# calculating cprd denominator

source("paths/00_filepath.R")
source("R/01_setup.R")

# denominator files 

all_pat_denominator_aurum <- fread(file.path(all_pat_denominator_path, "2024_03_CPRDAurum_AllPats.txt")) |> 
  as.data.table()

linkage_eligible <- fread(file.path(linkage_path, "Aurum_enhanced_eligibility_January_2022.txt")) |> 
  select(patid, pracid, hes_apc_e) |> 
  as.data.table()

# Join the datasets by 'patid' and 'pracid'
merged_data <- linkage_eligible[all_pat_denominator_aurum, on = .(patid, pracid), nomatch = NA]

# Convert 'regenddate' variable to Date type
merged_data[, regenddate := as.Date(regenddate, format = "%d/%m/%Y")]

# Filter rows where 'regenddate' is greater than "2012-12-31"
filtered_data <- merged_data[regenddate > as.Date("2012-12-31") | is.na(regenddate)]

# View the filtered data
print(filtered_data)

#####################
##CPRD DENOMINATOR 
#####################

# created 2016 variable if regstartis before 2013 and regend is after end of 2016

cprd_denominator <- filtered_data |>
  as.data.frame() |>
  filter(acceptable == 1) |> 
  select(-mob, -emis_ddate, -patienttypeid, -region) |> 
  mutate(
    regenddate = ymd(regenddate),
    regstartdate = dmy(regstartdate),
    age_2013 = case_when(
      (2013 - yob) < 0 ~ NA,
      (2013 - yob) < 5 & (2013 - yob) >= 0 ~ "0-4",
      (2013 - yob) >= 5 & (2013 - yob) < 15 ~ "5-14",
      (2013 - yob) >= 15 & (2013 - yob) < 65 ~ "15-64",
      (2013 - yob) >= 65 ~ "65+"),
    age_2014 = case_when(
      (2014 - yob) < 0 ~ NA,
      (2014 - yob) < 5 & (2014 - yob) >= 0 ~ "0-4",
      (2014 - yob) >= 5 & (2014 - yob) < 15 ~ "5-14",
      (2014 - yob) >= 15 & (2014 - yob) < 65 ~ "15-64",
      (2014 - yob) >= 65 ~ "65+"),
    age_2015 = case_when(
      (2015 - yob) < 0 ~ NA,
      (2015 - yob) < 5 & (2015 - yob) >= 0 ~ "0-4",
      (2015 - yob) >= 5 & (2015 - yob) < 15 ~ "5-14",
      (2015 - yob) >= 15 & (2015 - yob) < 65 ~ "15-64",
      (2015 - yob) >= 65 ~ "65+"),
    age_2016 = case_when(
      (2016 - yob) < 0 ~ NA,
      (2016 - yob) < 5 & (2016 - yob) >= 0 ~ "0-4",
      (2016 - yob) >= 5 & (2016 - yob) < 15 ~ "5-14",
      (2016 - yob) >= 15 & (2016 - yob) < 65 ~ "15-64",
      (2016 - yob) >= 65 ~ "65+"),
    age_2017 = case_when(
      (2017 - yob) < 0 ~ NA,
      (2017 - yob) < 5 & (2017 - yob) >= 0 ~ "0-4",
      (2017 - yob) >= 5 & (2017 - yob) < 15 ~ "5-14",
      (2017 - yob) >= 15 & (2017 - yob) < 65 ~ "15-64",
      (2017 - yob) >= 65 ~ "65+"),
    age_2018 = case_when(
      (2018 - yob) < 0 ~ NA,
      (2018 - yob) < 5 & (2018 - yob) >= 0 ~ "0-4",
      (2018 - yob) >= 5 & (2018 - yob) < 15 ~ "5-14",
      (2018 - yob) >= 15 & (2017 - yob) < 65 ~ "15-64",
      (2018 - yob) >= 65 ~ "65+"),
    age_2019 = case_when(
      (2019 - yob) < 0 ~ NA,
      (2019 - yob) < 5 & (2019 - yob) >= 0 ~ "0-4",
      (2019 - yob) >= 5 & (2019 - yob) < 15 ~ "5-14",
      (2019 - yob) >= 15 & (2019 - yob) < 65 ~ "15-64",
      (2019 - yob) >= 65 ~ "65+"),
    age_2020 = case_when(
      (2020 - yob) < 0 ~ NA,
      (2020 - yob) < 5 & (2020 - yob) >= 0 ~ "0-4",
      (2020 - yob) >= 5 & (2020 - yob) < 15 ~ "5-14",
      (2020 - yob) >= 15 & (2020 - yob) < 65 ~ "15-64",
      (2020 - yob) >= 65 ~ "65+"),
    age_2021 = case_when(
      (2021 - yob) < 0 ~ NA,
      (2021 - yob) < 5 & (2021 - yob) >= 0 ~ "0-4",
      (2021 - yob) >= 5 & (2021 - yob) < 15 ~ "5-14",
      (2021 - yob) >= 15 & (2021 - yob) < 65 ~ "15-64",
      (2021 - yob) >= 65 ~ "65+"),
    
    # to count in the denominator for epi week 27 in each year, they must be registered before the end of the week year
    # and the registration end date after the first day of the week or is NA because registration has not ended
    
    denom_2013 = case_when(regstartdate < as.Date("2013-07-07") & (regenddate > as.Date("2013-06-29") | is.na(regenddate)) ~ "denom_2013"),
    denom_2014 = case_when(regstartdate < as.Date("2014-07-06") & (regenddate > as.Date("2014-06-28") | is.na(regenddate)) ~ "denom_2014"),
    denom_2015 = case_when(regstartdate < as.Date("2015-07-09") & (regenddate > as.Date("2015-07-01") | is.na(regenddate)) ~ "denom_2015"),
    denom_2016 = case_when(regstartdate < as.Date("2016-07-10") & (regenddate > as.Date("2016-07-02") | is.na(regenddate)) ~ "denom_2016"),
    denom_2017 = case_when(regstartdate < as.Date("2017-07-09") & (regenddate > as.Date("2017-07-01") | is.na(regenddate)) ~ "denom_2017"),
    denom_2018 = case_when(regstartdate < as.Date("2018-07-08") & (regenddate > as.Date("2018-06-30") | is.na(regenddate)) ~ "denom_2018"),
    denom_2019 = case_when(regstartdate < as.Date("2019-07-07") & (regenddate > as.Date("2019-06-29") | is.na(regenddate)) ~ "denom_2019"),
    denom_2020 = case_when(regstartdate < as.Date("2020-07-05") & (regenddate > as.Date("2020-06-27") | is.na(regenddate)) ~ "denom_2020"),
    denom_2021 = case_when(regstartdate < as.Date("2021-07-11") & (regenddate > as.Date("2021-07-03") | is.na(regenddate)) ~ "denom_2021")
  )

# Create an empty list to store the results
denom_list <- list()

# Loop through years 2013 to 2019
for (i in 2013:2021) {
  # Calculate age_group and denom for the current year
  age_group <- paste0("age_", i)
  denom_value <- paste0("denom_", i)
  
  # Create a data frame for the current year
  current_year_data <- cprd_denominator %>%
    group_by(!!sym(age_group)) %>%
    summarise(!!sym(denom_value) := sum(!is.na(!!sym(denom_value)))) %>%
    ungroup() %>%
    rename(age_group = !!sym(age_group))
  
  # Store the current year data in the list
  denom_list[[i - 2012]] <- current_year_data
}

# View the list of results
print(denom_list)

# If you want to combine the results into a single data frame, you can use bind_rows
cprd_annual_denominator <- denom_list[[1]] |>
  as.data.frame() |> 
  left_join(denom_list[[2]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[3]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[4]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[5]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[6]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[7]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[8]] |> as.data.frame(), by = "age_group") |> 
  left_join(denom_list[[9]] |> as.data.frame(), by = "age_group") 

# View the final result
print(cprd_annual_denominator)

cprd_annual_denominator |>
  write_parquet(file.path(data_files_path, "cprd_annual_denominator.parquet"))

###################
##HES denominator
###################

hes_cprd_denominator <- filtered_data |>
  as.data.frame() |>
  filter(acceptable == 1,
         hes_apc_e == 1) |> 
  select(-mob, -emis_ddate, -patienttypeid, -region) |> 
  mutate(
    regenddate = ymd(regenddate),
    regstartdate = dmy(regstartdate),
    age_2013 = case_when(
      (2013 - yob) < 0 ~ NA,
      (2013 - yob) < 5 & (2013 - yob) >= 0 ~ "0-4",
      (2013 - yob) >= 5 & (2013 - yob) < 15 ~ "5-14",
      (2013 - yob) >= 15 & (2013 - yob) < 65 ~ "15-64",
      (2013 - yob) >= 65 ~ "65+"),
    age_2014 = case_when(
      (2014 - yob) < 0 ~ NA,
      (2014 - yob) < 5 & (2014 - yob) >= 0 ~ "0-4",
      (2014 - yob) >= 5 & (2014 - yob) < 15 ~ "5-14",
      (2014 - yob) >= 15 & (2014 - yob) < 65 ~ "15-64",
      (2014 - yob) >= 65 ~ "65+"),
    age_2015 = case_when(
      (2015 - yob) < 0 ~ NA,
      (2015 - yob) < 5 & (2015 - yob) >= 0 ~ "0-4",
      (2015 - yob) >= 5 & (2015 - yob) < 15 ~ "5-14",
      (2015 - yob) >= 15 & (2015 - yob) < 65 ~ "15-64",
      (2015 - yob) >= 65 ~ "65+"),
    age_2016 = case_when(
      (2016 - yob) < 0 ~ NA,
      (2016 - yob) < 5 & (2016 - yob) >= 0 ~ "0-4",
      (2016 - yob) >= 5 & (2016 - yob) < 15 ~ "5-14",
      (2016 - yob) >= 15 & (2016 - yob) < 65 ~ "15-64",
      (2016 - yob) >= 65 ~ "65+"),
    age_2017 = case_when(
      (2017 - yob) < 0 ~ NA,
      (2017 - yob) < 5 & (2017 - yob) >= 0 ~ "0-4",
      (2017 - yob) >= 5 & (2017 - yob) < 15 ~ "5-14",
      (2017 - yob) >= 15 & (2017 - yob) < 65 ~ "15-64",
      (2017 - yob) >= 65 ~ "65+"),
    age_2018 = case_when(
      (2018 - yob) < 0 ~ NA,
      (2018 - yob) < 5 & (2018 - yob) >= 0 ~ "0-4",
      (2018 - yob) >= 5 & (2018 - yob) < 15 ~ "5-14",
      (2018 - yob) >= 15 & (2017 - yob) < 65 ~ "15-64",
      (2018 - yob) >= 65 ~ "65+"),
    age_2019 = case_when(
      (2019 - yob) < 0 ~ NA,
      (2019 - yob) < 5 & (2019 - yob) >= 0 ~ "0-4",
      (2019 - yob) >= 5 & (2019 - yob) < 15 ~ "5-14",
      (2019 - yob) >= 15 & (2019 - yob) < 65 ~ "15-64",
      (2019 - yob) >= 65 ~ "65+"),
    age_2020 = case_when(
      (2020 - yob) < 0 ~ NA,
      (2020 - yob) < 5 & (2020 - yob) >= 0 ~ "0-4",
      (2020 - yob) >= 5 & (2020 - yob) < 15 ~ "5-14",
      (2020 - yob) >= 15 & (2020 - yob) < 65 ~ "15-64",
      (2020 - yob) >= 65 ~ "65+"),
    age_2021 = case_when(
      (2021 - yob) < 0 ~ NA,
      (2021 - yob) < 5 & (2021 - yob) >= 0 ~ "0-4",
      (2021 - yob) >= 5 & (2021 - yob) < 15 ~ "5-14",
      (2021 - yob) >= 15 & (2021 - yob) < 65 ~ "15-64",
      (2021 - yob) >= 65 ~ "65+"),
    
    # to count in the denominator for a given year, they must be registered before the end of the year
    # and the registration end date after the first day or is NA because registration has not ended
    
    denom_2013 = case_when(regstartdate < as.Date("2013-07-07") & (regenddate > as.Date("2013-06-29") | is.na(regenddate)) ~ "denom_2013"),
    denom_2014 = case_when(regstartdate < as.Date("2014-07-06") & (regenddate > as.Date("2014-06-28") | is.na(regenddate)) ~ "denom_2014"),
    denom_2015 = case_when(regstartdate < as.Date("2015-07-09") & (regenddate > as.Date("2015-07-01") | is.na(regenddate)) ~ "denom_2015"),
    denom_2016 = case_when(regstartdate < as.Date("2016-07-10") & (regenddate > as.Date("2016-07-02") | is.na(regenddate)) ~ "denom_2016"),
    denom_2017 = case_when(regstartdate < as.Date("2017-07-09") & (regenddate > as.Date("2017-07-01") | is.na(regenddate)) ~ "denom_2017"),
    denom_2018 = case_when(regstartdate < as.Date("2018-07-08") & (regenddate > as.Date("2018-06-30") | is.na(regenddate)) ~ "denom_2018"),
    denom_2019 = case_when(regstartdate < as.Date("2019-07-07") & (regenddate > as.Date("2019-06-29") | is.na(regenddate)) ~ "denom_2019"),
    denom_2020 = case_when(regstartdate < as.Date("2020-07-05") & (regenddate > as.Date("2020-06-27") | is.na(regenddate)) ~ "denom_2020"),
    denom_2021 = case_when(regstartdate < as.Date("2021-07-11") & (regenddate > as.Date("2021-07-03") | is.na(regenddate)) ~ "denom_2021")
  )

# Create an empty list to store the results
hes_denom_list <- list()

# Loop through years 2013 to 2019
for (i in 2013:2021) {
  # Calculate age_group and denom for the current year
  age_group <- paste0("age_", i)
  denom_value <- paste0("denom_", i)
  
  # Create a data frame for the current year
  current_year_data <- hes_cprd_denominator %>%
    group_by(!!sym(age_group)) %>%
    summarise(!!sym(denom_value) := sum(!is.na(!!sym(denom_value)))) %>%
    ungroup() %>%
    rename(age_group = !!sym(age_group))
  
  # Store the current year data in the list
  hes_denom_list[[i - 2012]] <- current_year_data
}

# View the list of results
print(hes_denom_list)

# If you want to combine the results into a single data frame, you can use bind_rows
hes_annual_denominator <- hes_denom_list[[1]] |>
  as.data.frame() |> 
  left_join(hes_denom_list[[2]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[3]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[4]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[5]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[6]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[7]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[8]] |> as.data.frame(), by = "age_group") |> 
  left_join(hes_denom_list[[9]] |> as.data.frame(), by = "age_group") 

# View the final result
print(hes_annual_denominator)

hes_annual_denominator |>
  write_parquet(file.path(data_files_path, "hes_annual_denominator.parquet"))

