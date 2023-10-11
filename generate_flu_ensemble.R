library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(hubEnsembles)
library(hubUtils)

current_ref_date <- lubridate::ceiling_date(Sys.Date(), "week") - days(1)
task_id_cols <- c("reference_date", "location", "horizon", "target", "target_end_date")
  
hub_path <- "../FluSight-forecast-hub"
hub_con <- connect_hub(hub_path) 
current_forecasts <- hub_con |>
  dplyr::filter(
    reference_date == current_ref_date, 
    stringr::str_detect(model_id, "FluSight", negate=TRUE) # remove baseline and ensembles
  ) |> 
  dplyr::collect() |>
  as_model_out_tbl() 


# QUANTILE ENSEMBLE
quantile_forecasts <- current_forecasts |>
  dplyr::filter(output_type == "quantile") |>
  dplyr::mutate(output_type_id=as.character(as.numeric(output_type_id))) # ensures quantiles treated the same regardless of presence of trailing zeros
  
# generate median ensemble
median_name <- "FluSight-median"
median_ensemble_outputs <- quantile_forecasts |>
  hubEnsembles::simple_ensemble(
    agg_fun="median", 
    model_id=median_name, 
    task_id_cols=task_id_cols
  ) |>
  dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
  dplyr::select(-model_id)

# median_ensemble_path <- paste(hub_path, "/model-output/", median_name, "/", current_ref_date, "-", median_name, ".csv", sep="") 
# readr::write_csv(median_ensemble_outputs, median_ensemble_path)
  
# generate linear pool of quantiles (if desired)
lop_norm_name <- "FluSight-lop_norm"
lop_norm_outputs <- quantile_forecasts |>
  hubEnsembles::linear_pool(
    model_id=lop_norm_name,
    task_id_cols=task_id_cols
  ) |>
  dplyr::mutate(value = ifelse(value < 0, 0, value)) |>
  dplyr::select(-model_id)

lop_norm_path <- paste(hub_path, "/model-output/", lop_norm_name, "/", current_ref_date, "-", lop_norm_name, ".csv", sep="")
readr::write_csv(lop_norm_outputs, lop_norm_path)


# PMF ENSEMBLE
categorical_name <- "FluSight-categorical"
categorical_forecasts <- current_forecasts |>
  dplyr::filter(output_type == "pmf") 
  
categorical_ensemble_outputs <- categorical_forecasts |>
  hubEnsembles::simple_ensemble(
    agg_fun="mean", 
    model_id=categorical_name, 
    task_id_cols=task_id_cols
  ) |>
  dplyr::select(-model_id)

ensemble_name <- "FluSight-ensemble"
flusight_ensemble_outputs <- median_ensemble_outputs |>
  dplyr::bind_rows(categorical_ensemble_outputs)
flusight_ensemble_path <- paste(hub_path, "/model-output/", ensemble_name, "/", current_ref_date, "-", ensemble_name, ".csv", sep="") 
readr::write_csv(flusight_ensemble_outputs, flusight_ensemble_path)