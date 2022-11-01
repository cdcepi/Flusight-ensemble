#### Developed from:
#### https://github.com/reichlab/hubEnsembles/blob/main/vignettes/basic-ensemble.Rmd
#### Developed by: P. Prasad

# Packages to Install

# devtools::install_github("reichlab/zoltr")
# devtools::install_github("reichlab/covidData")
# devtools::install_github("reichlab/simplets")
#devtools::install_github("reichlab/covidHubUtils")
#devtools::install_github("reichlab/hubEnsembles")

library(covidHubUtils)
library(hubEnsembles)
library(dplyr)

# Set the environment - dates should change each week & check to see if the file paths are correct 
userid = "rpe5"
forecast_date = "2022-10-31" # Monday
sixweeks_before_forecast_date = "2022-09-19" # 6 weeks ago Monday

ensemble_code_path = paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-ensemble")
flusight_path = paste0("C:/Users/",userid,"/Desktop/GitHub/Flusight-forecast-data") #using my forked repo for this now
setwd(flusight_path)

output_dir <- paste0(ensemble_code_path, "/", forecast_date, "/")

if(!dir.exists(output_dir)){
  dir.create(path = output_dir)
}

# Get the models to be included in the ensemble
if(!file.exists(paste0(output_dir, "models-to-include-in-ensemble-", forecast_date, ".csv"))){
  file_names = list.files(path = paste0(flusight_path, "/data-forecasts"))
  all_models = file_names[!(file_names %in% c("Flusight-baseline", "Flusight-ensemble")) &
                            !grepl(paste0(".md", collapse = "|"), file_names)]
  all_metadata = paste0(flusight_path, "/data-forecasts/", all_models,
                        "/metadata-", all_models, ".txt") %>%
    lapply(., read.delim)
    include <- c()
  for(i in 1:length(all_models)){
    
    metadata = all_metadata[[i]]
    
    # this checks to see that this week's file is in the model directory and
    # that it is a designated primary, secondary, or proposed model in the metadata
    if(file.exists(paste0(flusight_path, "/data-forecasts/", all_models[i], 
                          "/", forecast_date, "-", all_models[i], ".csv")) &
      (colSums("team_model_designation: primary" == metadata) +
       colSums("team_model_designation: proposed" == metadata) +
       colSums("team_model_designation: secondary" == metadata) > 0)){
      include = c(include, all_models[i])
    }
  }
  write.csv(data.frame(model = include),paste0(output_dir, "models-to-include-in-ensemble-", forecast_date, ".csv"))
}

# We start by loading the forecasts of weekly incident hospitalizations from selected models
eligible_models = read.csv(paste0(output_dir, "models-to-include-in-ensemble-", forecast_date, ".csv"),
                           header = TRUE)
models =as.character(eligible_models$model)


#Read in forecast data

forecast_data <- load_forecasts_repo(
  file_path = paste0(flusight_path, "/data-forecasts/"),
  models = models,
  targets = c(paste(1:4, "wk ahead inc flu hosp")),
  forecast_dates = forecast_date,
  hub = "FluSight",
  types = "quantile")%>%
  rename(full_location_name = location_name) %>%
  mutate(full_location_name = case_when(location == "US" ~ "United States",
                                        location != "US" ~ full_location_name))


#Read in truth data

truth_data <- load_truth(
  truth_source = "HealthData", 
  target_variable = "inc flu hosp", 
  locations = unique(forecast_data$location),
  hub = "FluSight"
) %>%
  rename(full_location_name = location_name)


#Plot individual team forecasts

all_locations = sort(unique(forecast_data$location))
starting_location = seq(1, length(all_locations), 3)

pdf(paste0(output_dir, "all-models-", forecast_date, ".pdf"))
for(i in starting_location){
  plot_forecasts(
    forecast_data = forecast_data %>% filter(location %in% all_locations[i:(i+2)]),
    facet = .~location,
    facet_scales = "free_y",
    facet_ncol = 1,
    facet_nrow = 3,
    truth_data = truth_data %>% filter(target_end_date > sixweeks_before_forecast_date),
    truth_source = "HealthData",
    use_median_as_point = TRUE, 
    fill_by_model = TRUE, 
    title = "Weekly Influenza Incident Hospitalizations: observed and forecasted",
    show_caption = FALSE, 
    fill_transparency = .3
  ) 
}
dev.off()

pdf(paste0(output_dir, "all-models-full-page-", forecast_date, ".pdf"))
for(i in seq(1, length(all_locations), 1)){
  plot_forecasts(
    forecast_data = forecast_data %>% filter(location %in% all_locations[i]),
    facet = .~location,
    facet_scales = "free_y",
    facet_ncol = 1,
    facet_nrow = 1,
    truth_data = truth_data %>% filter(target_end_date > sixweeks_before_forecast_date),
    truth_source = "HealthData",
    use_median_as_point = TRUE, 
    fill_by_model = TRUE, 
    title = "Weekly Influenza Incident Hospitalizations: observed and forecasted",
    show_caption = FALSE, 
    fill_transparency = .3
  ) 
}
dev.off()


# Build, save, and plot the ensemble 

ensemble_forecast <- build_quantile_ensemble(forecast_data, 
                                             method = "median",
                                             forecast_date = forecast_date,
                                             model_name = "Flusight-ensemble",
                                             location_data = hub_locations)


ensemble_forecast1 <- ensemble_forecast %>% 
  mutate(target = paste(horizon, temporal_resolution, "ahead", target_variable, sep = " ")) %>% 
  dplyr::select(forecast_date, target, target_end_date, location, type, quantile, value)

write.csv(ensemble_forecast1, paste0(flusight_path, "/data-forecasts/Flusight-ensemble/",forecast_date, "-Flusight-ensemble.csv"), row.names=FALSE)

pdf(paste0(output_dir, "ensemble-", forecast_date, ".pdf"))
for(i in starting_location){
  plot_forecasts(
    forecast_data = ensemble_forecast %>% filter(location %in% all_locations[i:(i+2)]),
    facet = .~location,
    facet_scales = "free_y",
    facet_ncol = 1,
    facet_nrow = 3,
    truth_data = truth_data %>% filter(target_end_date > sixweeks_before_forecast_date),
    truth_source = "HealthData", 
    title = "Weekly Influenza Incident Hospitalizations: observed and forecasted",
    use_median_as_point = TRUE
  )
}
dev.off()

pdf(paste0(output_dir, "ensemble-full-page-", forecast_date, ".pdf"))
for(i in seq(1, length(all_locations), 1)){
  plot_forecasts(
    forecast_data = ensemble_forecast %>% filter(location %in% all_locations[i]),
    facet = .~location,
    facet_scales = "free_y",
    facet_ncol = 1,
    facet_nrow = 1,
    truth_data = truth_data %>% filter(target_end_date > sixweeks_before_forecast_date),
    truth_source = "HealthData", 
    title = "Weekly Influenza Incident Hospitalizations: observed and forecasted",
    use_median_as_point = TRUE
  )
}
dev.off()

