#' ---
#' title: Supplementary information
#' date: "`r format(Sys.time(), '%d %B %Y %H:%M')`"
#' author: Hikaru Bolt
#' output: word_document
#' 
#' ---
#'
#'
# To run this
# rmarkdown::render("R/08_supplement.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("supplement", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      ft.align="left",
                      fig.width = 10,
                      fig.asp = 0.8,
                      out.width = "100%")

fpp <- fp_par(text.align = "left", padding = 3)

#+ time series

#' ## Norovirus surveillance data (SGSS) all ages

observation_data |>
  select(week_date, noro_obs_1, noro_obs_2, noro_obs_3, noro_obs_4) |>
  mutate(noro_obs65 = noro_obs_4,
         noro_obs15_64 = noro_obs_3,
         noro_obs5_14 = noro_obs_2,
         noro_obs0_4 = noro_obs_1,
  ) |>
  gather(noro_obs65,noro_obs15_64,noro_obs5_14,noro_obs0_4, key = "type", value = "case") |>
  mutate(type = case_when(
    type == "noro_obs0_4" ~ "0-4",
    type == "noro_obs5_14" ~ "5-14",
    type == "noro_obs15_64" ~ "15-64",
    type == "noro_obs65" ~ "65+"),
    type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
  ggplot(aes(week_date, case)) +
  geom_line(stat = "identity", linewidth = 1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Norovirus laboratory surveillance data (SGSS)",
       y = "Number of reports") +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette)

#' ## Gastroenteritis GP attendance data age stratified
# observation_data |> 
#   select(week_date, gastro_gp_obs_1, gastro_gp_obs_2, gastro_gp_obs_3, gastro_gp_obs_4) |>
#   gather(gastro_gp_obs_1, gastro_gp_obs_2, gastro_gp_obs_3, gastro_gp_obs_4, key = "type", value = "case") |> 
#   mutate(
#     type = case_when(
#       type == "gastro_gp_obs_1" ~ "0-4",
#       type == "gastro_gp_obs_2" ~ "5-14",
#       type == "gastro_gp_obs_3" ~ "15-64",
#       type == "gastro_gp_obs_4" ~ "65+"),
#     type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
#   ggplot(aes(week_date, case)) +
#   geom_line(stat = "identity", linewidth = 1) +
#   facet_wrap(~type, ncol = 1) +
#   theme_bw() +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, 800), breaks = seq(0, 800, by = 200)) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   scale_color_manual(values = cbbPalette) +
#   labs(title = "Gastroenteritis GP attendance (CPRD)",
#        y = "Incidence per 100,000 p-yrs")


observation_data |>
  filter(time < 363) |> 
  select(week_date, gastro_gp_obs_1, gastro_gp_obs_2, gastro_gp_obs_3, gastro_gp_obs_4) |>
  gather(gastro_gp_obs_1, gastro_gp_obs_2, gastro_gp_obs_3, gastro_gp_obs_4, key = "type", value = "case") |> 
  mutate(
    type = case_when(
      type == "gastro_gp_obs_1" ~ "0-4",
      type == "gastro_gp_obs_2" ~ "5-14",
      type == "gastro_gp_obs_3" ~ "15-64",
      type == "gastro_gp_obs_4" ~ "65+"),
    type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
  ggplot(aes(week_date, case)) +
  geom_line(stat = "identity", linewidth = 1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_bw() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 800), breaks = seq(0, 800, by = 200)) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette) +
  labs(title = "Gastroenteritis GP attendance (CPRD)",
       y = "Incidence per 100,000 p-yrs")


#' ## Gasstroenteritis hospitalisation data age stratified
# 
# observation_data |> 
#   select(week_date, gastro_hosp_obs_1, gastro_hosp_obs_2, gastro_hosp_obs_3, gastro_hosp_obs_4) |>
#   gather(gastro_hosp_obs_1, gastro_hosp_obs_2, gastro_hosp_obs_3, gastro_hosp_obs_4, key = "type", value = "case") |> 
#   mutate(type = case_when(
#     type == "gastro_hosp_obs_1" ~ "0-4",
#     type == "gastro_hosp_obs_2" ~ "5-14",
#     type == "gastro_hosp_obs_3" ~ "15-64",
#     type == "gastro_hosp_obs_4" ~ "65+"),
#     type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
#   ggplot(aes(week_date, case)) +
#   geom_line(stat = "identity", linewidth = 1) +
#   facet_wrap(~type, ncol = 1) +
#   theme_bw() +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   scale_color_manual(values = cbbPalette) +
#   labs(title = "Gastroenteritis hospitalisation (HES)",
#        y = "Incidence per 100,000 p-yrs")

observation_data |> 
  filter(time < 363) |> 
  select(week_date, gastro_hosp_obs_1, gastro_hosp_obs_2, gastro_hosp_obs_3, gastro_hosp_obs_4) |>
  gather(gastro_hosp_obs_1, gastro_hosp_obs_2, gastro_hosp_obs_3, gastro_hosp_obs_4, key = "type", value = "case") |> 
  mutate(type = case_when(
    type == "gastro_hosp_obs_1" ~ "0-4",
    type == "gastro_hosp_obs_2" ~ "5-14",
    type == "gastro_hosp_obs_3" ~ "15-64",
    type == "gastro_hosp_obs_4" ~ "65+"),
    type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
  ggplot(aes(week_date, case)) +
  geom_line(stat = "identity", linewidth = 1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_bw() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette) +
  labs(title = "Gastroenteritis hospitalisation (HES)",
       y = "Incidence per 100,000 p-yrs")


#' ## Acute kidney injury hospitalisation data age stratified

# observation_data |> 
#   select(week_date, aki_hosp_obs_1, aki_hosp_obs_2, aki_hosp_obs_3, aki_hosp_obs_4) |>
#   gather(aki_hosp_obs_1, aki_hosp_obs_2, aki_hosp_obs_3, aki_hosp_obs_4, key = "type", value = "case") |> 
#   mutate(type = case_when(
#     type == "aki_hosp_obs_1" ~ "0-4",
#     type == "aki_hosp_obs_2" ~ "5-14",
#     type == "aki_hosp_obs_3" ~ "15-64",
#     type == "aki_hosp_obs_4" ~ "65+"),
#     type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
#   ggplot(aes(week_date, case)) +
#   geom_line(stat = "identity", linewidth = 1) +
#   facet_wrap(~type, ncol = 1) +
#   theme_bw() +
#   scale_x_date(expand = c(0, 0),
#                date_breaks = "1 year",
#                date_labels = "%Y") +
#   scale_y_continuous(expand = c(0,0), limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   annotate("rect", fill = "grey", alpha = 0.3,
#            xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
#            ymin = -Inf, ymax = Inf) +
#   scale_color_manual(values = cbbPalette) +
#   labs(title = "Acute kidney injury hospitalisation (HES)",
#        y = "Incidence per 100,000 p-yrs")


observation_data |> 
  filter(time < 363) |> 
  select(week_date, aki_hosp_obs_1, aki_hosp_obs_2, aki_hosp_obs_3, aki_hosp_obs_4) |>
  gather(aki_hosp_obs_1, aki_hosp_obs_2, aki_hosp_obs_3, aki_hosp_obs_4, key = "type", value = "case") |> 
  mutate(type = case_when(
    type == "aki_hosp_obs_1" ~ "0-4",
    type == "aki_hosp_obs_2" ~ "5-14",
    type == "aki_hosp_obs_3" ~ "15-64",
    type == "aki_hosp_obs_4" ~ "65+"),
    type = factor(type, levels = c("0-4","5-14","15-64", "65+"))) |>
  ggplot(aes(week_date, case)) +
  geom_line(stat = "identity", linewidth = 1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_bw() +
  scale_x_date(expand = c(0, 0),
               date_breaks = "1 year",
               date_labels = "%Y") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 160), breaks = seq(0, 160, by = 40)) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2014-01-01", "%Y-%m-%d"), xmax = as.Date("2014-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2016-01-01", "%Y-%m-%d"), xmax = as.Date("2016-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2018-12-31",  "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf) +
  scale_color_manual(values = cbbPalette) +
  labs(title = "Acute kidney injury hospitalisation (HES)",
       y = "Incidence per 100,000 p-yrs")

#+ contact rates, results = 'asis'

age_groups <- c(0, 5, 15, 65)
polymod <- socialmixr::polymod
UK_structure <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  age.limits = c(age_groups),
  symmetric = TRUE
)

# Symmetrical contact matrix
uk_contact_rate_matrix <- as.matrix(UK_structure$matrix)

matrix_plot(uk_contact_rate_matrix)

#' Trace plots

xyplot( x = traceBurn_params)

#' Effective sample size

plotEssBurn(my_trace)

#' Autocorrelation plot

acfplot(x = traceBurnThin, lag.max = 60)

#' Multi chain plots

# xyplot(traceBurnThin_params_multi)

#' reference costs

reference_costs |> 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 8, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table: reference costs"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )
  
#+ table ICD10 codes , results = 'asis'

icd_10_codes <- fread("data/icd10_code_list.txt")

icd_10_codes %>% 
  select(icd, description) %>% 
  rename("ICD10 code" = icd,
         "Code description" = description) %>% 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 8, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 1: Hospitalisation ICD10 codes"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

aki_med_codes <- fread("data/aki_aurum_code_list.txt") |>  
  mutate(medcodeid = as.character(tolower(MedCodeId))) |> 
  select(medcodeid, CleansedReadCode, Term) |>
  distinct() |> 
  rename("Medcode ID" = medcodeid ,
         "Read code" = CleansedReadCode,
         "Code description" = Term)

aki_med_codes |> 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 8, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 2: GP attendance medical codes for acute kidney injury"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )

gastro_med_codes <- fread("data/gastro_aurum_code_list.txt") |> 
  mutate(medcodeid = as.character(tolower(MedCodeId))) |> 
  select(medcodeid, CleansedReadCode, Term) |>
  distinct() |> 
  rename("Medcode ID" = medcodeid ,
         "Read code" = CleansedReadCode,
         "Code description" = Term)

gastro_med_codes |> 
  flextable::flextable() %>%    # convert to pretty image
  padding(padding = 1.5, part = "all") %>% 
  flextable::autofit() %>% 
  fontsize(size = 8, part = "all") %>% 
  set_caption(
    caption = as_paragraph(
      "Supplementary table 2: GP attendance medical codes for gastroenteritis"),
    align_with_table = FALSE,
    word_stylename = "Table Caption",
    fp_p = fpp
  )
