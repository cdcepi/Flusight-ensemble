# Create PMF forecasts from baseline and ensemble quantile forecasts 
# Must run after baseline and ensemble are pushed to the FluSight hub



#remotes::install_github("Infectious-Disease-Modeling-Hubs/hubUtils")
library(hubUtils)

# zeallot perform multiple unpacking, and destructing assignment in R.  
# needed to install disfromq
# devtools::install_github("https://github.com/nteetor/zeallot")

# distfromq for approximating a distribution's pdf and cdf from a collection
# of quantiles
# https://github.com/reichlab/distfromq
# devtools::install_github("https://github.com/reichlab/distfromq")

library(distfromq)

# reshape2 Flexibily reshape data: a reboot of the reshape package
# https://github.com/hadley/reshape
library(reshape2)

# ggforce Accelerating 'ggplot2'
# https://github.com/thomasp85/ggforce
library (ggforce)

# tidyverse: Easily install and load the 'Tidyverse'
# https://github.com/tidyverse/tidyverse
library(tidyverse)

# readr: Read rectangular text data
# https://github.com/tidyverse/readr
#library(readr)

# dplyr: A grammar of data manipulation
# https://github.com/tidyverse/dplyr
#library(dplyr)

library(gridExtra)


select <- dplyr::select

# set working directory
work_dir <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-ensemble")
setwd(work_dir)

# source get_pmf_forecasts_from_quantile.R - will eventually be implemented in idforecastutils package
source("get_pmf_forecasts_from_quantile.R")

# Current category definitions 2023-2024 (as of 2023-11-04 update)
h_vec <- 0:3 
multiplier_matrix <- matrix(c(c(2, 3, 4, 5), c(1, 1, 2, 2.5), c(-1, -1, -2, -2.5), c(-2, -3, -4, -5)), ncol=4)
category_rule_matrix <- matrix(c(rep(10, 4), rep(10, 4), rep(-10, 4), rep(-10, 4)), ncol=4)

# # Initial category definitions 2023-2024 (before 2023-11-04 update)
# h_vec <- -1:3 
# multiplier_matrix <- matrix(c(c(2, 3, 4, 5, 5), c(1, 1, 2, 2.5, 2.5), c(-1, -1, -2, -2.5, -2.5), c(-2, -3, -4, -5, -5)), ncol=num_cat-1)
# category_rule_matrix <- matrix(c(rep(10, 5), rep(10, 5), rep(-10, 5), rep(-10, 5)), ncol=num_cat-1)

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
                    by = join_by("location_name" == "location_name", "location" == "location"))

output_df <- get_pmf_forecasts_from_quantile(
  quantile_forecasts = flusight_ensemble, locations_df = location_data, truth_df = target_data,
  
  #model_output_quantile_only,  
  #location_data, # locations.csv auxiliary data
  #target_data, # applicable target data, should have same temporal resolution as model_output
  categories = c("large_increase", "increase", "stable", "decrease", "large_decrease"),
  horizons=h_vec, # choose from above
  count_rate_multiplier=multiplier_matrix, # choose from above 
  category_rule=category_rule_matrix, # choose from above
  target_name="wk flu hosp rate change"
)

output_pmf <- output_df %>% filter(output_type == "pmf") %>% select(-model_id)
#FluSight-ens_q_cat
ens_dates <- output_pmf %>% {unique(.$reference_date)}



# write.csv(output_pmf, paste0("C://Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-forecast-hub/model-output/FluSight-ens_q_cat/", ens_dates,"-FluSight-ens_q_cat.csv"), row.names = FALSE)
# 

baseline_output <- get_pmf_forecasts_from_quantile(
  quantile_forecasts = flusight_baseline, locations_df = location_data, truth_df = target_data,
  
  #model_output_quantile_only,  
  #location_data, # locations.csv auxiliary data
  #target_data, # applicable target data, should have same temporal resolution as model_output
  categories = c("large_increase", "increase", "stable", "decrease", "large_decrease"),
  horizons=h_vec, # choose from above
  count_rate_multiplier=multiplier_matrix, # choose from above 
  category_rule=category_rule_matrix, # choose from above
  target_name="wk flu hosp rate change"
)

baseline_df <- baseline_output %>% filter(output_type == "pmf") %>% select(-model_id)

ens_dates <- baseline_df %>% {unique(.$reference_date)}

# write.csv(baseline_df, paste0("C://Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-forecast-hub/model-output/FluSight-baseline_cat/", ens_dates ,"-FluSight-baseline_cat"), row.names = FALSE)
