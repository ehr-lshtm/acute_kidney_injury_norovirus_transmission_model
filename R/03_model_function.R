# model function

simulate <- function(parameters, init.state, times, age.incidence = FALSE) {
  
  data <- noromod_cpp_boost(
    initial_conditions = init.state,
    params = parameters, time_end = times, increment = 1
  )
  
  data <- output_to_df(data)
  
  # spline function for aki_hosp observations
  
  source("R/spline_functions.R")
  
  # aggregating daily time series to weekly time series for model fitting
  
  # Assuming 'data' is your original data frame
  setDT(data)
  
  # Create new_date and week_date columns
  data[, new_date := seq.Date(from = as.Date("1990-01-01"),
                              by = "day",
                              length.out = .N)]
  data[, week_date := floor_date(new_date, "week", week_start = 1)]
  data[, year := year(new_date)]
  
  # Filter by week_date
  filtered_data <- data[week_date > as.Date('2013-01-01') & week_date <= as.Date('2019-12-30')]
  
  # Summarize by week_date
  summarized_data <- filtered_data[, .(exposed_1 = sum(exposed_1),
                                       exposed_2 = sum(exposed_2),
                                       exposed_3 = sum(exposed_3),
                                       exposed_4 = sum(exposed_4),
                                       infectious_symp_1 = sum(infectious_symp_1),
                                       infectious_symp_2 = sum(infectious_symp_2),
                                       infectious_symp_3 = sum(infectious_symp_3),
                                       infectious_symp_4 = sum(infectious_symp_4),
                                       recovered_1 = sum(recovered_1),
                                       recovered_2 = sum(recovered_2),
                                       recovered_3 = sum(recovered_3),
                                       recovered_4 = sum(recovered_4)),
                                   by = week_date]
  
  # Summarize by year
  age_total_infections <- filtered_data[, .(infectious_symp_1 = sum(infectious_symp_1),
                                              infectious_symp_2 = sum(infectious_symp_2),
                                              infectious_symp_3 = sum(infectious_symp_3),
                                              infectious_symp_4 = sum(infectious_symp_4))]
  
  age_total_incidence <- data.frame(age_total_infections) |> 
    pivot_longer(cols = everything(), names_to = "age", values_to = "total") |> 
    mutate(population_age = case_when(
      age == "infectious_symp_1" ~ init.state[1, 1]*7,
      age == "infectious_symp_2" ~ init.state[2, 1]*7,
      age == "infectious_symp_3" ~ init.state[3, 1]*7,
      age == "infectious_symp_4" ~ init.state[4, 1]*7),
      model_incidence = round((total / population_age)*1000),
      study = "model") |> 
    select(age, model_incidence) |> 
    mutate(age = case_when(
      age == "infectious_symp_1" ~ "0-4",
      age == "infectious_symp_2" ~ "5-14",
      age == "infectious_symp_3" ~ "15-64",
      age == "infectious_symp_4" ~ "65+",
    )) |> 
    as.data.table()
  
  # Add a new variable 'time' that increases by 1 for each row
  summarized_data[, time := 0:(.N - 1)]
  
  # transform aki parameter
  
  aki_hosp_param4 <- exp(parameters[["aki_hospitalisation_4"]])
  # aki_hosp_overall <- parameters[["aki_hospitalisation_overall"]]
    
  # transform gastro parameter
  
  gastro_hosp_param4 <- exp(parameters[["gastro_hospitalisation_4"]])
  
  # transform gastro gp parameter
  
  gastro_gp_param1 <- exp(parameters[["gastro_gp_attend_1"]])
  gastro_gp_param2 <- exp(parameters[["gastro_gp_attend_2"]])

  
  # left join spl data
  # summarized_data <- summarized_data[model_spl_pred4_diff, on = "time"]
  summarized_data <- summarized_data[model_spl_pred4, on = "time"]
  # summarized_data <- summarized_data[gastro_model_spl_pred4_diff, on = "time"]
  summarized_data <- summarized_data[gastro_model_spl_pred4, on = "time"]
  summarized_data <- summarized_data[gastro_gp_model_spl_pred2_diff, on = "time"]
  # summarized_data <- summarized_data[gastro_gp_model_spl_pred2, on = "time"]
  summarized_data <- summarized_data[gastro_gp_model_spl_pred1_diff, on = "time"]
  # summarized_data <- summarized_data[gastro_gp_model_spl_pred1, on = "time"]
  
  # Calculate noro_value and aki_hosp_model_4
  summarized_data[, `:=` (
    noro_model_1 = infectious_symp_1 * parameters[["surveillance_report_1"]],
    noro_model_2 = infectious_symp_2 * parameters[["surveillance_report_2"]],
    noro_model_3 = infectious_symp_3 * parameters[["surveillance_report_3"]],
    noro_model_4 = infectious_symp_4 * parameters[["surveillance_report_4"]],
    # noro_model = (exposed_4 / init.state[4, 1] * 1000000) * parameters[["surveillance_report"]],
    # test = (((infectious_symp_4 / init.state[4, 1] * 100000) + model_spl_pred4_diff)),
    # aki_hosp_model_4 = ((((infectious_symp_4 / init.state[4, 1] * 100000) + model_spl_pred4_diff))*aki_hosp_param4),
    # aki_hosp_model_4 = ((infectious_symp_4 * aki_hosp_param4) - median(infectious_symp_4 * aki_hosp_param4) + model_spl_pred4),
    aki_hosp_model_4 = ((((infectious_symp_4 / init.state[4, 1] * 100000) * aki_hosp_param4)
                                - median((infectious_symp_4 / init.state[4, 1] * 100000) * aki_hosp_param4)
    ) + model_spl_pred4),
    # aki_hosp_model_4 = (((((infectious_symp_4 / init.state[4, 1] * 100000)*aki_hosp_param4) + model_spl_pred4_diff))*aki_hosp_overall),
    # test_gastro_hosp_model_4 = ((((infectious_symp_4 / init.state[4, 1] * 100000) + gastro_model_spl_pred4_diff))),
    # gastro_hosp_model_4 = ((((infectious_symp_4 / init.state[4, 1] * 100000) + gastro_model_spl_pred4_diff))*gastro_hosp_param4),
    # # gastro_hosp_model_4 = ((infectious_symp_4 / init.state[4, 1] * 1000000) * gastro_hosp_param4),
    gastro_hosp_model_4 = ((((infectious_symp_4 / init.state[4, 1] * 100000) * gastro_hosp_param4)
                         - median((infectious_symp_4 / init.state[4, 1] * 100000) * gastro_hosp_param4)
    ) + gastro_model_spl_pred4),
    gastro_gp_model_1 = ((((infectious_symp_1 / init.state[1, 1] * 100000) + gastro_gp_model_spl_pred1_diff))*gastro_gp_param1),
    # gastro_gp_model_1 = ((infectious_symp_1 / init.state[1, 1] * 1000000) * gastro_gp_param1),
    # gastro_gp_model_1 = ((((infectious_symp_1 / init.state[1, 1] * 100000) * gastro_gp_param1)
    #                         - median((infectious_symp_1 / init.state[1, 1] * 100000) * gastro_gp_param1)
    # ) + gastro_gp_model_spl_pred1),
    # gastro_gp_model_median = median((infectious_symp_1 / init.state[1, 1] * 100000) * gastro_gp_param1),
    # gastro_gp_model_overall = (infectious_symp_1 / init.state[1, 1] * 100000) * gastro_gp_param1,
    # gastro_gp_model_difference = (((infectious_symp_1 / init.state[1, 1] * 100000) * gastro_gp_param1)
    #                               - median((infectious_symp_1 / init.state[1, 1] * 100000) * gastro_gp_param1)),
    # gastro_model_spl_pred4 = gastro_model_spl_pred4,
    # gastro_gp_model_1_new = ((((infectious_symp_1 / init.state[1, 1] * 100000))
    #                       - median((infectious_symp_1 / init.state[1, 1] * 100000))
    # ) + gastro_gp_model_spl_pred1),
    # test_gastro_gp_model_2 = ((((infectious_symp_2 / init.state[2, 1] * 100000) + gastro_gp_model_spl_pred2_diff))),
    gastro_gp_model_2 = ((((infectious_symp_2 / init.state[2, 1] * 100000) + gastro_gp_model_spl_pred2_diff))*gastro_gp_param2),
    # gastro_gp_model_2 = ((infectious_symp_2 / init.state[2, 1] * 1000000) * gastro_gp_param2),
    # gastro_gp_model_2 = ((((infectious_symp_2 / init.state[2, 1] * 100000) * gastro_gp_param2)
    #                      - median((infectious_symp_2 / init.state[2, 1] * 100000) * gastro_gp_param2)
    # ) + gastro_gp_model_spl_pred2),
    infectious_symp_1 = infectious_symp_1 / init.state[1, 1] * 100000,
    infectious_symp_2 = infectious_symp_2 / init.state[2, 1] * 100000,
    infectious_symp_3 = infectious_symp_3 / init.state[3, 1] * 100000,
    infectious_symp_4 = infectious_symp_4 / init.state[4, 1] * 100000,
    infectious_symp_1_count = infectious_symp_1,
    infectious_symp_2_count = infectious_symp_2,
    infectious_symp_3_count = infectious_symp_3,
    infectious_symp_4_count = infectious_symp_4,
    recovered_1 = recovered_1 / init.state[1, 1] * 100000,
    recovered_2 = recovered_2 / init.state[2, 1] * 100000,
    recovered_3 = recovered_3 / init.state[3, 1] * 100000,
    recovered_4 = recovered_4 / init.state[4, 1] * 100000,
    
    ######
    
    aki_hosp_model_4_infection = ((infectious_symp_4 / init.state[4, 1] * 100000) * aki_hosp_param4),
    aki_hosp_model_4_median = median((infectious_symp_4 / init.state[4, 1] * 100000) * aki_hosp_param4),
    aki_hosp_model_4_difference = ((((infectious_symp_4 / init.state[4, 1] * 100000) * aki_hosp_param4)
                                    - median((infectious_symp_4 / init.state[4, 1] * 100000) * aki_hosp_param4)
    )),
    gastro_gp_model_2_difference = ((((infectious_symp_4 / init.state[2, 1] * 100000) * gastro_gp_param2)
                                    - median((infectious_symp_4 / init.state[2, 1] * 100000) * gastro_gp_param2)
    )),
    # aki_hosp_model_4_with_param = (((((infectious_symp_4 / init.state[4, 1] * 100000)*aki_hosp_param4) + model_spl_pred4_diff))),
    # gastro_gp_model_2_infection = ((((infectious_symp_2 / init.state[2, 1] * 100000) + gastro_gp_model_spl_pred2_diff))),
    # gastro_gp_model_2_infection_model_spline = (median((infectious_symp_2 / init.state[2, 1] * 100000)) + gastro_gp_model_spl_pred2_diff),
    # gastro_gp_model_1_infection = ((((infectious_symp_1 / init.state[1, 1] * 100000) + gastro_gp_model_spl_pred1_diff))),
    # gastro_gp_model_1_infection_model_spline = (median((infectious_symp_1 / init.state[1, 1] * 100000)) + gastro_gp_model_spl_pred1_diff),

    gastro_hosp_model_4_infection = ((infectious_symp_4 / init.state[4, 1] * 100000) * gastro_hosp_param4),
    gastro_hosp_model_4_median = median((infectious_symp_4 / init.state[4, 1] * 100000) * gastro_hosp_param4),
    gastro_hosp_model_4_difference = ((((infectious_symp_4 / init.state[4, 1] * 100000) * gastro_hosp_param4)
                                    - median((infectious_symp_4 / init.state[4, 1] * 100000) * gastro_hosp_param4)
    ))

  )]
  

    # Select final columns
  traj <-
    summarized_data[, .(
      time,
      infectious_symp_1, 
      infectious_symp_2, 
      infectious_symp_3,
      infectious_symp_4,
      infectious_symp_1_count,
      infectious_symp_2_count,
      infectious_symp_3_count,
      infectious_symp_4_count,
      recovered_1,
      recovered_2,
      recovered_3,
      recovered_4,
      noro_model_1,
      noro_model_2,
      noro_model_3,
      noro_model_4,
      aki_hosp_model_4,
      gastro_hosp_model_4,
      gastro_gp_model_1,
      gastro_gp_model_2
      
    )]
  
  # Print the resulting data.table
  # print(traj)
  # 
  # print(age_total_incidence)
  
  # Use an if statement to conditionally return the result
  if (age.incidence) {
    return(age_total_incidence)
  } else {
    return(traj)
  }
  
}

