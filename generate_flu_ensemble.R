library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(hubEnsembles)
library(hubUtils)
library(yaml)

userid <- "rpe5"

current_ref_date <- lubridate::ceiling_date(Sys.Date(), "week") - days(1)
task_id_cols <- c("reference_date", "location", "horizon", "target", "target_end_date")
  
out_path <- paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-ensemble")
hub_path <- paste0("C:/Users/",userid,"/Desktop/GitHub/FluSight-forecast-hub")
hub_con <- connect_hub(hub_path) 
current_forecasts <- hub_con |>
  dplyr::filter(
    reference_date == current_ref_date, 
    stringr::str_detect(model_id, "FluSight", negate=TRUE) # remove baseline and ensembles
  ) |> 
  dplyr::collect() |>
  as_model_out_tbl() 

# if(!file.exists(paste0(out_path, "models-to-include-in-ensemble-", current_ref_date, ".csv"))){
   file_names = list.files(path = paste0(hub_path, "/model-metadata"))
   all_metadata = file_names[!(file_names %in% c("FluSight-baseline", "FluSight-ensemble", "FluSight-lop_norm")) &
                            !grepl(paste0(".md", collapse = "|"), file_names)]# %>%

   
   
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

eligible_models <- data.frame(Model = file.names, Designated_Model = designated_models) %>% filter(Designated_Model == T)
write.csv(eligible_models ,paste0(out_path, "/models-to-include-in-ensemble-", current_ref_date, ".csv"))

eligible_models = read.csv(paste0(out_path, "/models-to-include-in-ensemble-", current_ref_date, ".csv"),
                           header = TRUE)
models = as.character(eligible_models$Model)
# 
#     lapply(., read.delim)
#   include <- c()
#   for(i in 1:length(all_metadata)){
#     
#     metadata = all_metadata[[i]]
#     
#     # this checks to see that this week's file is in the model directory and
#     # that it is a designated primary, secondary, or proposed model in the metadata
#     if(file.exists(paste0(hub_path, "/data-forecasts/", all_models[i], 
#                           "/", current_ref_date, "-", all_models[i], ".csv")) &
#        (colSums("team_model_designation: primary" == metadata) +
#         colSums("team_model_designation: proposed" == metadata) +
#         colSums("team_model_designation: secondary" == metadata) > 0)){
#       include = c(include, all_models[i])
#     }
#   }
#   write.csv(data.frame(model = include),paste0(out_path, "models-to-include-in-ensemble-", current_ref_date, ".csv"))
# }
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

# median_ensemble_path <- paste(hub_path, "/model-output/", median_name, "/", current_ref_date, "-", median_name, ".csv", sep="") 
# readr::write_csv(median_ensemble_outputs, median_ensemble_path)
  
# generate linear pool of quantiles (if desired)
lop_norm_name <- "FluSight-lop_norm"
lop_norm_outputs <- quantile_forecasts |>
  dplyr::mutate(output_type_id=as.numeric(output_type_id)) |>
  hubEnsembles::linear_pool(
    model_id=lop_norm_name, 
    task_id_cols=task_id_cols
  ) |>
  dplyr::mutate(
    value = ifelse(value < 0, 0, value), 
    output_type_id = as.character(output_type_id)
  ) |>
  dplyr::select(-model_id)

lop_norm_path <- paste(out_path, "/model-output/", lop_norm_name, "/", current_ref_date, "-", lop_norm_name, ".csv", sep="")
readr::write_csv(lop_norm_outputs, lop_norm_path)


# PMF ENSEMBLE
categorical_name <- "FluSight-categorical"
categorical_forecasts <- current_forecasts |>
  dplyr::filter(output_type == "pmf") |>
  dplyr::group_by(reference_date, target, target_end_date, output_type) |> # create appropriate groups for `complete`
  tidyr::complete(model_id, horizon, location, output_type_id, fill=list(value=0)) |> # add in missing output_type_ids and fill the missing values with zero
  unique()

#categorical_forecasts <- categorical_forecasts[!(categorical_forecasts$model_id == "SGroup-RandomForest" & categorical_forecasts$location == "US" & categorical_forecasts$horizon == -1 & categorical_forecasts$output_type_id == "stable" & categorical_forecasts$value == 0),]

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
flusight_ensemble_path <- paste(out_path, "/model-output/", ensemble_name, "/", current_ref_date, "-", ensemble_name, ".csv", sep="") 
readr::write_csv(flusight_ensemble_outputs, flusight_ensemble_path)


loc.dat <- read.csv(paste0(hub_path, "/auxiliary-data/locations.csv"))
quantile_forecasts$location_name <- loc.dat$location_name[match(quantile_forecasts$location, loc.dat$location)]
quantile_forecasts$location_name <- ifelse(quantile_forecasts$location_name == "US", "National", quantile_forecasts$location_name)
current_forecasts$location_name <- loc.dat$location_name[match(current_forecasts$location, loc.dat$location)]
current_forecasts$location_name <- ifelse(current_forecasts$location_name == "US", "National", current_forecasts$location_name)
 # readr::write_csv(quantile_forecasts, "C:/Users/rpe5/Desktop/GitHub/Flusight-ensemble/model-output/FluSight-ensemble/all_quantile_forecasts.csv")
 # readr::write_csv(current_forecasts, "C:/Users/rpe5/Desktop/GitHub/Flusight-ensemble/model-output/FluSight-ensemble/all_forecasts.csv")
