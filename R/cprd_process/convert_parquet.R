#---------------------------------------------------------------------------------------
# Program Name: create_parquet
# Author:  Modified by Rutendo Muzambi - code originally from Anne Suffel
# Date version created: 15/07/2022
# Description: converting unzipped text files into parquet files and reading and writing parquet files
#             Run time on 15/07/2022 of txt file to parquet file conversion = 5 hours and 10 mins              
#---------------------------------------------------------------------------------------

####Preparation

###Install packages

library(data.table)
library(vroom)
library(arrow)

##################Unzipping data#####################

#####Reading the flat files

#Make a list of all file names
#file path where raw data is
setwd(rawdata_hes_files_path)
getwd()

#file path were parquet files will be stored
Out_Dir <- parquet_file_path

hes_files <- list.files(path = rawdata_hes_files_path, recursive=FALSE, pattern = "\\hes_", full.names=TRUE)
hes_files <- hes_files[-1]        # remove the first file due to corruption

convert_parquet <- function(files, classes){
  filenames <- tools::file_path_sans_ext(basename(files))
  
  for (i in cli::cli_progress_along(files)) {
    write_parquet(data.table(read.table(files[i], header = TRUE, sep = "\t", na.strings = "", colClasses=classes)), 
                  paste0(Out_Dir, filenames[i], ".parquet"))
  }
}

#Convert txt files into parquet files
convert_parquet(files = hes_files, classes=c(patid="character"))

#File conversion for single corrupted file

hes_diagnosis_epi <- fread(file.path(rawdata_hes_files_path, "hes_diagnosis_epi_23_003034_DM.txt"))

hes_diagnosis_epi %>% 
  write_parquet(file.path(rawdata_hes_files_path, "parquethes_diagnosis_epi_23_003034_DM.parquet"))

####Example of reading a patient file into data table and writing to folder
patient_file <- list.files(path = rawdata_hes_files_path, pattern = "\\hes_")
patient_dt <- list()
patient_data <- as.data.table(arrow::read_parquet(paste0(parquet_files, sep="/", patient_file)))
patient_dt <- patient_data
write_parquet(patient_data, paste0(datafiles, "patients.parquet"))