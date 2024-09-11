# EHR group template for R Projects

## Purpose
This is the analysis code for the paper 'Quantifying the impact of norovirus transmission in the community on acute kidney injury hospitalisations in England: a mathematical modelling study'

## Untracked files
This repository contains only non-disclosive files, that is, code without file paths, and summary statistics. This template is set up so only files that are safe to upload to Github, such as code, are uploaded by default. The underlying data used for this study was from the Clinical Practice Research Datalink, which includes UK Primary Care Data, and linked data such as Hospital Episode Statistics. Access to this data is subject to protocol approval via CPRD’s Research Data Governance (RDG) Process. Given the sensitive nature of these data, this is not uploaded to the repository. Access to data is available on request to the Clinical Practice Research Datalink (https://cprd.com/how-access-cprd-data).

## File tree

```
template-r/
├── codelists/
│   ├── README.md
│   ├── aki_gastro_define_23_003034.txt
│   ├── gastro_aurum_code_list.txt
│   ├── gastro_define_23_003034.txt
│   └── icd10_code_list.txt
├── data/
│   ├── README.md
│   ├── age_incidence.csv (untracked)
│   └── age_incidence_fitting.csv (untracked)
├── docs/
├── │   └── HRGs/
│   ├── README.md
│   ├── 2012-13_national_schedule_of_reference_costs.xls
│   ├── 2013-14_national_schedule_of_reference_costs.xls
│   ├── 2014-15_national_schedule_of_reference_costs.xlsx
│   ├── 2015-16_national_schedule_of_reference_costs.xlsx
│   ├── 2016-17_national_schedule_of_reference_costs.xlsx
│   ├── 2017-18_national_schedule_of_reference_costs.xlsx
│   ├── 2018-19_national_schedule_of_reference_costs.xlsx
│   └── 2019-20_national_schedule_of_reference_costs.xlsx
├── paths/
│   ├── README.md
│   └── 00_filepath.R (untracked)
├── R/
│   ├── README.md
│   ├── 01_setup.R
│   ├── 02_observation_data.R
│   ├── 03_model_function.R
│   ├── 04_likelihood_prior_functions.R
│   ├── 05_trace_sample_storage.R
│   ├── 06_mcmc_outputs.R
│   ├── 07_reference_costing.R
│   ├── 08_supplement.R
│   ├── fixed_parameters.R
│   ├── replicates.R
│   ├── fixed_parameters.R
│   └── spline_functions.R
├── │   └── cprd_process/
│   ├── aurum_define.R
│   ├── convert_parquet.R
│   ├── cprd_code_list.R
│   ├── cprd_denominator.R
│   ├── cprd_processing.R
│   └── hes_patient_list.R
├── results/
├── │   └── trace_data/
│   ├── my_trace_19072024.txt
│   ├── trace_1_26072024.txt
│   ├── trace_2_26072024.txt
│   ├── trace_3_26072024.txt
│   └── trace_4_26072024.txt
├── r-template.Rproj
└── README.md
```

# File description summary in codelist folder

aki_aurum_code_list.txt
* Med codes list used to define acute kidney injury in gp attendances
aki_aurum_snomed_medcodes.txt
* Snomed codes list used to define acute kidney injury in gp attendances
aki_define_23_003034.txt
* Combined code list used to define acute kidney injury in gp attendances for study 23_003034
gastro_aurum_code_list.txt
* Med codes list used to define gastroenteritis in gp attendances
gastro_define_23_003034.txt
* Code list used to define gastroenteritis in gp attendances
aki_gastro_define_23_003034.txt
* Combined code list used to define acute kidney injury and gastroenteritis in gp attendances for study 23_003034
icd10_code_list.txt
* ICD10 codes to define acute kidney injury and gastroenteritis in hospital admissions

# Code description summary in docs folder

HRGS
* annual NHS national schedule of reference costs

# Code description summary in paths folder

00_filepath.R
* contains file paths and not tracked due to sensitivity

# Code description summary in R folder

01_setup.R
* contains packages used, and colour palettes used
02_observation_data.R
* loads observation data (surveillance, hospital admissions, GP attendances counts) and formats it for use in the fitting process
03_model_function.R
* model function simulating the noromod package, and applying an observation model to fit to the observation data
04_likelihood_prior_functions.R
* contains the likelihood and prior functions used in the fitting process
05_trace_sample_storage.R
* contains the mcmc execution and sample storage functions used in the fitting process
06_mcmc_outputs.R
* contains the mcmc outputs, diagnostics used in the fitting process, and figures produced in the manuscript - includes trace plots, density plots, and posterior predictive checks
07_reference_costing.R
* contains the reference costing functions used to calculate the cost of acute kidney injury hospital admissions
08_supplement.R
* contains the supplementary information used in the manuscript
fixed_parameters.R
* contains the fixed parameters used in the model simulations
replicates.R
* contains the function used to generate the uncertainty intervals for the model simulations
spline_functions.R
* contains the spline functions used to produce the observation model

cprd_process
* contains the code used to process the CPRD data and define the denominator to calculate incidence, including the aurum_define.R, convert_parquet.R, cprd_code_list.R, cprd_denominator.R, and hes_patient_list.R

# Code description summary in results folder

trace_data
* contains a zip file of trace data generated by executing mcmc (the model fitting process). Data used to genearte figures in results in manuscript are from my_trace. Trace 1 to 4 are for the multichain plots in the supplement.
