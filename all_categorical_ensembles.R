library(tidyverse)
library(tidyr)
library(dplyr)
library(epiDisplay)
library(MMWRweek)
library(DT)
library(plotly)
library(gridExtra)
library(covidHubUtils)
library(ggridges)
library(viridis)
library(cowplot)
library(scales)
library(RSocrata)
library(lubridate)
library(readr)
library(stringr)
library(hubEnsembles)
library(hubUtils)
library(yaml)

userid="rpe5"

'%!in%' <- Negate('%in%') #previously %notin% possible #update
'%>%' <- dplyr::`%>%`

first.ref.date = as.Date("2023-10-14")
last.ref.date = as.Date("2024-01-06")

window.width = c(2, 4, 8)

eval.weeks = 8

weeks.to.eval = 
  seq(first.ref.date, last.ref.date, by=7) %>% 
  as.character()
# weeks.to.eval = 
#   seq((last.ref.date)- 7*(eval.weeks-1) - 7*(max(window.width)-1),
#       last.ref.date,
#       by=7) %>% 
#   as.character()


dashboard_r_code <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code")
flusight_forecast_data <-paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-forecast-hub")
dashboard_r_code_weekly_data <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flu-Visualizations/Dashboard R Code/Weekly Data/")

suppressMessages(invisible(source(paste0(dashboard_r_code_weekly_data,"Model names and colors.R"))))
source(paste0(dashboard_r_code,"/functions2022-2023.R"))

select = dplyr::select
filter = dplyr::filter

all_out <- character(0)

for (k in weeks.to.eval) {
  
  task_id_cols <- c("reference_date", "location", "horizon", "target", "target_end_date")
  
  out_path <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-ensemble")
  hub_path <- paste0("C:/Users/",userid,"/Desktop/GitHub/FluSight-forecast-hub")
  hub_con <- connect_hub(hub_path) 
  current_forecasts <- hub_con |>
    dplyr::filter(
      reference_date == k, 
      stringr::str_detect(model_id, "FluSight", negate=TRUE) # remove baseline and ensembles
    ) |> 
    dplyr::collect() |>
    as_model_out_tbl() 
  
  yml.files <- list.files(paste0(hub_path, "/model-metadata"), pattern = "\\.ya?ml$", full.names = T)
  
  designated_models <- character(0)
  file.names <- character(0)
  
  for (i in yml.files) {
    
    file.name <- tools::file_path_sans_ext(basename(i))
    file.names <- c(file.names, file.name)
    
    yml.dat <- yaml.load_file(i)
    
    if ("designated_model" %in% names(yml.dat)) {
      # Extract the value of "designated_model"
      designated_model_value <- yml.dat$designated_model
      designated_models <- c(designated_models, designated_model_value)
    } else {
      # If "designated_model" doesn't exist in the YAML file, you can set a default value or handle it as needed.
      designated_models <- c(designated_models, NA)  # For example, setting it to NA.
    }
  }
  
  # Print the values of "designated_model" for each YAML file
  for (i in seq_along(yml.files)) {
    cat("File:", yml.files[i], "\n")
    cat("designated_model:", designated_models[[i]], "\n")
    cat("\n")
    
  }
  
  eligible_models = read.csv(paste0(out_path, "/models-to-include-in-ensemble-", k, ".csv"),
                             header = TRUE)
  models = as.character(eligible_models$Model)

  
  current_forecasts <- current_forecasts[current_forecasts$model_id %in% models,]
  current_forecasts <- current_forecasts[current_forecasts$location != 78,]
  
  
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
  
  # PMF ENSEMBLE
  categorical_name <- "FluSight-categorical"
  categorical_forecasts <- current_forecasts |>
    dplyr::filter(output_type == "pmf") |>
    dplyr::group_by(reference_date, target, target_end_date, output_type) |> # create appropriate groups for `complete`
    tidyr::complete(model_id, horizon, location, output_type_id, fill=list(value=NA)) |> # add in missing output_type_ids and fill the missing values with zero
    unique()
  
  #categorical_forecasts <- categorical_forecasts[!(categorical_forecasts$model_id == "SGroup-RandomForest" & categorical_forecasts$location == "US" & categorical_forecasts$horizon == -1 & categorical_forecasts$output_type_id == "stable" & categorical_forecasts$value == 0),]
  categorical_ensemble_outputs <- categorical_forecasts |>
    hubEnsembles::simple_ensemble(
      agg_fun="mean", 
      agg_args = list(na.rm = T),
      model_id=categorical_name, 
      task_id_cols=task_id_cols
    ) |>
    dplyr::select(-model_id)
  
  ensemble_name <- "FluSight-ensemble"
  flusight_ensemble_outputs <- median_ensemble_outputs |>
    dplyr::bind_rows(categorical_ensemble_outputs)
  # flusight_ensemble_path <- paste(out_path, "/model-output/", ensemble_name, "/", k, "-", ensemble_name, ".csv", sep="") 
  # readr::write_csv(flusight_ensemble_outputs, flusight_ensemble_path)
  # 
  all_out <- rbind(all_out, flusight_ensemble_outputs) %>% unique()
}