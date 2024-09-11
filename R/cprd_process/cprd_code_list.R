# AKI cprd codelist processing

source("R/01_setup.R")

# Gastro cprd codelist processing

gastro_med_codes <- fread("codelists/gastro_aurum_code_list.txt") |> 
  mutate(medcodeid = as.character(tolower(MedCodeId))) |> 
  select(medcodeid) |> 
  distinct()

output <- paste(gastro_med_codes$medcodeid, collapse = ",")
write.table(output, file = "codelists/gastro_define_23_003034.txt", sep = "," , col.names = FALSE, row.names = FALSE, quote = FALSE)

gastro_aurum <- med_code_lookup |> 
  inner_join(gastro_med_codes, by = c("CleansedReadCode" = "readcode"))

# aki cprd codelist processing

aki_med_codes <- fread("codelists/aki_aurum_code_list.txt") |>  
  mutate(medcodeid = as.character(tolower(MedCodeId))) |> 
  select(medcodeid) |> 
  distinct()

output <- paste(aki_med_codes$medcodeid, collapse = ",")
write.table(output, file = "codelists/aki_define_23_003034.txt", sep = "," , col.names = FALSE, row.names = FALSE, quote = FALSE)

# combined codelist

combined_data <- rbind(aki_med_codes, gastro_med_codes)
output <- paste(combined_data$medcodeid, collapse = ",")
write.table(output, file = "codelists/aki_gastro_define_23_003034.txt", sep = "," , col.names = FALSE, row.names = FALSE, quote = FALSE)
