# check population structure

summary_stats<-summary(traceBurnThin)
# summary_stats<-summary(my_trace)

mean_mcmc<-summary_stats$statistics[,1]
median_mcmc<-summary_stats$quantiles[,3]
q2.5<-summary_stats$quantiles[,1]
q97.5<-summary_stats$quantiles[,5]
theta <- median_mcmc

parameters = c(theta, par)

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
summarized_data <- filtered_data[, .(susceptible_1 = mean(susceptible_1),
                                     susceptible_2 = mean(susceptible_2),
                                     susceptible_3 = mean(susceptible_3),
                                     susceptible_4 = mean(susceptible_4),
                                     exposed_1 = mean(exposed_1),
                                     exposed_2 = mean(exposed_2),
                                     exposed_3 = mean(exposed_3),
                                     exposed_4 = mean(exposed_4),
                                     infectious_symp_1 = mean(infectious_symp_1),
                                     infectious_symp_2 = mean(infectious_symp_2),
                                     infectious_symp_3 = mean(infectious_symp_3),
                                     infectious_symp_4 = mean(infectious_symp_4),
                                     infectious_asymp_1 = mean(infectious_asymp_1),
                                     infectious_asymp_2 = mean(infectious_asymp_2),
                                     infectious_asymp_3 = mean(infectious_asymp_3),
                                     infectious_asymp_4 = mean(infectious_asymp_4),
                                     recovered_1 = mean(recovered_1),
                                     recovered_2 = mean(recovered_2),
                                     recovered_3 = mean(recovered_3),
                                     recovered_4 = mean(recovered_4)),
                                 by = week_date]

## sum

# summarized_data %>%
#   mutate(
#     population_1 = susceptible_1 + exposed_1 + infectious_symp_1 + recovered_1,
#     population_2 = susceptible_2 + exposed_2 + infectious_symp_2 + recovered_2,
#     population_3 = susceptible_3 + exposed_3 + infectious_symp_3 + recovered_3,
#     population_4 = susceptible_4 + exposed_4 + infectious_symp_4 + recovered_4
#   ) %>%
#   group_by(week_date) %>%
#   summarise(
#     `Age group 1` = sum(population_1),
#     `Age group 2` = sum(population_2),
#     # `Age group 3` = sum(population_3),
#     `Age group 4` = sum(population_4)
#   ) %>%
#   pivot_longer(-week_date, names_to = "age_group", values_to = "population") %>%
#   ggplot(aes(x = week_date, y = population, colour = age_group)) +
#   geom_line() +
#   labs(title = "Population structure over time",
#        x = "Date", y = "Population", colour = "Age group") +
#   theme_bw()


##

# summarized_data %>%
#   mutate(
#     population_1 = susceptible_1 + exposed_1 + infectious_symp_1 + recovered_1,
#     population_2 = susceptible_2 +exposed_2 + infectious_symp_2 + recovered_2,
#     population_3 = susceptible_3 +exposed_3 + infectious_symp_3 + recovered_3,
#     population_4 = susceptible_4 +exposed_4 + infectious_symp_4 + recovered_4
#   ) %>%
#   group_by(week_date) %>%
#   summarise(
#     `Age group 1` = sum(population_1),
#     `Age group 2` = sum(population_2),
#     `Age group 3` = sum(population_3),
#     `Age group 4` = sum(population_4)
#   ) %>%
#   filter(week_date == min(week_date) | week_date == max(week_date)) %>%
#   mutate(time_point = ifelse(week_date == min(week_date), "Start", "End")) %>%
#   pivot_longer(cols = starts_with("Age"), names_to = "age_group", values_to = "population") %>%
#   ggplot(aes(x = age_group, y = population, fill = age_group)) +
#   geom_col() +
#   facet_wrap(~ time_point) +
#   labs(title = "Population distribution at start and end of study period",
#        x = "Age group", y = "Population", fill = "Age group") +
#   theme_bw()

##

# Extract initial population from init.state
initial_pop <- data.frame(
  time_point = "Initial",
  age_group  = paste("Age group", 1:4),
  population = init.state[, 1]  # first column = total initial population per age group
)

# ONS mid-year 2022 UK population estimates
ons_2022 <- data.frame(
  time_point = "ONS 2022 - UK",
  age_group  = c("Age group 1", "Age group 2", "Age group 3", "Age group 4"),
  population = c(3664446, 7430811, 42866627, 9674777)
)


# Get start and end from model output
model_pop <- summarized_data %>%
  mutate(
    population_1 = susceptible_1 + exposed_1 + infectious_symp_1 + infectious_asymp_1 + recovered_1,
    population_2 = susceptible_2 +exposed_2 + infectious_symp_2 + infectious_asymp_2 + recovered_2,
    population_3 = susceptible_3 +exposed_3 + infectious_symp_3 + infectious_asymp_3 + recovered_3,
    population_4 = susceptible_4 +exposed_4 + infectious_symp_4 + infectious_asymp_4 +recovered_4
  ) %>%
  group_by(week_date) %>%
  summarise(
    `Age group 1` = sum(population_1),
    `Age group 2` = sum(population_2),
    `Age group 3` = sum(population_3),
    `Age group 4` = sum(population_4)
  ) %>%
  filter(week_date == min(week_date) | week_date == max(week_date)) %>%
  mutate(time_point = ifelse(week_date == min(week_date), "Start", "End")) %>%
  pivot_longer(cols = starts_with("Age"), names_to = "age_group", values_to = "population") %>%
  select(time_point, age_group, population)

##

large_font <- theme_bw(base_size = 14) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(size = 14),
    axis.title       = element_text(size = 14),
    axis.text        = element_text(size = 12),
    plot.title       = element_text(size = 16)
  )


# Combine and plot
pop_count <- bind_rows(initial_pop, model_pop, ons_2022) %>%
  mutate(
    time_point = factor(time_point, 
                        levels = c("Initial", "Start", "End", "ONS 2022 - UK"),
                        labels = c("Initial", "Start - 2013", "End - 2019", "ONS 2022 - UK")),
    age_group  = factor(age_group,
                        levels = c("Age group 1", "Age group 2", "Age group 3", "Age group 4"),
                        labels = c("0-4", "5-14", "15-64", "65-81"))
  ) %>%
  
  ggplot(aes(x = age_group, y = population, fill = age_group)) +
  geom_col() +
  facet_wrap(~ time_point, nrow = 1) +
  scale_fill_viridis_d() +
  labs(x = NULL, y = "Population") +
  large_font


pop_prop <- bind_rows(initial_pop, model_pop, ons_2022) %>%
  mutate(
    time_point = factor(time_point, 
                        levels = c("Initial", "Start", "End", "ONS 2022 - UK"),
                        labels = c("Initial", "Start - 2013", "End - 2019", "ONS 2022 - UK")),
    age_group  = factor(age_group,
                        levels = c("Age group 1", "Age group 2", "Age group 3", "Age group 4"),
                        labels = c("0-4", "5-14", "15-64", "65-81"))
  ) %>%
  
  group_by(time_point) %>%
  mutate(proportion = population / sum(population)) %>%
  ggplot(aes(x = age_group, y = proportion, fill = age_group)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  facet_wrap(~ time_point, nrow = 1) +
  labs(x = "Age group", y = "Proportion of total population") +
  large_font

ggarrange(pop_count, pop_prop,
          nrow = 2,
          labels = c("A", "B"))
