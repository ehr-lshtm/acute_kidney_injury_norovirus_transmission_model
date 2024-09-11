# HES patient list preparation for type 2 extraction

# preparation of patient list for CPRD in order to receive HES linkage data

#############
### AURUM ###
#############

# hes patient ids - Aurum

hes_aurum_patid <- fread(file.path(rawdatata_hes_id_path, "/23_003034-results/type1_request/23_003034_icd_aurum_hesapc.txt"))

# AURUM denominator file

all_pat_denominator_aurum <-
  fread(file.path(all_pat_denominator_path,"2024_03_CPRDAurum_AllPats.txt"))

all_prac_denominator_aurum <- fread(file.path(all_pat_denominator_path,"2024_03_CPRDAurum_Practices.txt"))

linkage_eligibility_aurum <- fread(file.path(linkage_path, "Aurum_enhanced_eligibility_January_2022.txt"))

# AURUM minimising HES patient list to records linkage eligible, acceptable, and in follow up period

hes_aurum_cohort <- hes_aurum_patid %>%
  left_join(all_pat_denominator_aurum, by = "patid") %>%              # join in patient denominator file to HES patient list
  left_join(linkage_eligibility_aurum, by = c("patid", "pracid")) %>%
  # left_join(icd_10_codes, by = c("ICD" = "icd")) %>% 
  mutate(
    across(
      c("eventdate", "regstartdate", "regenddate", "cprd_ddate", "lcd"),
      .fns = dmy ,
      .names = "{.col}"
    ),
    study_start = as.Date("2006-01-01"),
    study_end = as.Date("2019-12-31"),
    hes_coverage_start = as.Date("1997-04-01"),
    hes_coverage_end = as.Date("2021-03-31"),
    
    # only use when defining a cohort - in this instance we are keen on all events of hospitalisations and not linked all the way thourgh to CPRD
    # registration follow up period
    
    # start_date = pmax(regstartdate, hes_coverage_start, study_start, na.rm = TRUE),
    # end_date = pmin(
    #   # regenddate,
    #   # cprd_ddate,
    #   lcd,
    #   hes_coverage_end,
    #   study_end,
    #   na.rm = TRUE
    # ),
    
    follow_exclude = eventdate < study_start
  ) %>%
  filter(acceptable == 1,
         follow_exclude == FALSE,
         hes_apc_e == 1) %>%
  select(patid, hes_apc_e) |>
  distinct() |>   # deduplicating the patient ids
  mutate(file_number = (row_number() - 1) %/% 380000 + 1)
  # write_tsv(file.path(rawdatata_hes_id_path, "/23_003034-return/23_003034_LSHTM_patientlist_aurum.txt"))

# Split the data frame into smaller data frames based on the 'group' column
split_dfs <- hes_aurum_cohort %>%
  group_by(file_number) %>%
  group_split()

# Define the function to save split data frames to TSV files
save_split_dfs <- function(df_list, prefix, output_dir) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  lapply(seq_along(df_list), function(i) {
    # Generate the file name with the full path
    file_name <- file.path(output_dir, paste0(prefix, "_file_", i, ".txt"))
    # Save the data frame to a TSV file
    write_tsv(df_list[[i]], file_name)
  })
}

# Save the split data frames to CSV files with a given prefix
output_dir <- file.path(rawdatata_hes_id_path, "23_003034-return/")

save_split_dfs(split_dfs, "23_003034_LSHTM_patientlist_aurum", output_dir)

# Check the current working directory for the saved files
