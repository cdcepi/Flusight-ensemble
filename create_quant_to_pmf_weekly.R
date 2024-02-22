# Create PMF forecasts from baseline and ensemble quantile forecasts 
# Must run after baseline and ensemble are pushed to the FluSight hub

# remotes::install_github("Infectious-Disease-Modeling-Hubs/hubUtils")
# devtools::install_github("https://github.com/reichlab/idforecastutils")

library(hubUtils)
library(idforecastutils)
library(reshape2)
library(tidyverse)
library(gridExtra)

# set working directory
# work_dir <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-ensemble")
# setwd(work_dir)

# source get_pmf_forecasts_from_quantile.R - will eventually be implemented in idforecastutils package
#source("get_pmf_forecasts_from_quantile.R")

# Current category definitions 2023-2024 (as of 2023-11-04 update)
h_vec <- 0:3 
multiplier_matrix <- matrix(c(c(2, 3, 4, 5), c(1, 1, 2, 2.5), c(-1, -1, -2, -2.5), c(-2, -3, -4, -5)), ncol=4)
category_rule_matrix <- matrix(c(rep(10, 4), rep(10, 4), rep(-10, 4), rep(-10, 4)), ncol=4)

# hub function - specify model and get all of the data for that model 
flusight <- connect_hub(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flusight-forecast-hub"))

cat_data <- flusight %>% filter(output_type == "quantile", horizon != -1) %>%  dplyr::collect() %>%
  as_model_out_tbl()

flusight_ensemble <- cat_data %>% filter(model_id == "FluSight-ensemble", reference_date == max(reference_date)) 
flusight_baseline <- cat_data %>% filter(model_id == "FluSight-baseline", reference_date == max(reference_date)) 

location_data <- readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/auxiliary-data/locations.csv") %>% select(-`...5`) %>% rename(geo_value = abbreviation) %>% mutate(geo_value = tolower(geo_value))

target_data <- readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/target-data/target-hospital-admissions.csv") %>% 
  select(-c(`...1`)) %>% rename(time_value = date)%>%
  dplyr::inner_join(location_data,
                    by = join_by("location_name" == "location_name", "location" == "location")) %>% select(geo_value, time_value, value)

ens_pmf <- get_pmf_forecasts_from_quantile(
  quantile_forecasts = flusight_ensemble, 
  locations_df = location_data, 
  truth_df = target_data,
  categories = c("large_increase", "increase", "stable", "decrease", "large_decrease"),
  horizons=h_vec, # choose from above
  count_rate_multiplier=multiplier_matrix, # choose from above 
  category_rule=category_rule_matrix, # choose from above
  target_name="wk flu hosp rate change"
)%>% filter(output_type == "pmf") %>% select(-model_id)

ens_dates <- unique(ens_pmf$reference_date)

 write.csv(ens_pmf, paste0("C://Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-forecast-hub/model-output/FluSight-ens_q_cat/", ens_dates,"-FluSight-ens_q_cat.csv"), row.names = FALSE)

 baseline_df <- get_pmf_forecasts_from_quantile(
  quantile_forecasts = flusight_baseline, 
  locations_df = location_data, 
  truth_df = target_data,
  categories = c("large_increase", "increase", "stable", "decrease", "large_decrease"),
  horizons=h_vec, # choose from above
  count_rate_multiplier=multiplier_matrix, # choose from above 
  category_rule=category_rule_matrix, # choose from above
  target_name="wk flu hosp rate change"
) %>% filter(output_type == "pmf") %>% select(-model_id)


write.csv(baseline_df, paste0("C://Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-forecast-hub/model-output/FluSight-baseline_cat/", ens_dates ,"-FluSight-baseline_cat"), row.names = FALSE)
