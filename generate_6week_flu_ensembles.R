
library(plyr)
library(dplyr)
library(zoltr)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)
library(hubData)
library(hubAdmin)
library(yaml)
library(scoringutils)
library(lubridate)

####################################################################  Original code

#Need to load the truth data for the period.
#flu_truth_current <- read.csv(paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flusight-forecast-hub/target-data/target-hospital-admissions.csv"))
flu_truth_current <- read.csv("https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/target-data/target-hospital-admissions.csv")
flu_truth_current$target_variable<-'wk inc flu hosp'
flu_truth_current$model<-'flu-truth'

#flu_dates_24_25 <- as.Date("2024-11-23") + weeks(6:28)#Starting 6 weeks later than the start of forecast (burn period)
flu_dates_25_26 <- as.Date("2025-11-22") + weeks(6:28)#Starting 6 weeks later than the start of forecast (burn period)
task_id_cols <- c("reference_date", "location", "horizon", "target_variable", "target_end_date")


#Scaling parameter function
sigmoid <- function(x,theta) {
  return(exp(x*theta))
}


#Setting values to search across, can expand if needed
theta<-seq(0,100,0.1)

forecasts_6week<-data.frame()
current_ref_date <- lubridate::ceiling_date(Sys.Date(), "week") - days(1)
  #This the period of time we will use to evaluate performance
  #flu_dates_24_25_retro <- as.Date(current_ref_date) - weeks(1:6)
  flu_dates_25_26_retro <- as.Date(current_ref_date) - weeks(1)
  
  #Another loop across the training period
  for (i in 1:length(flu_dates_25_26_retro)) {
    forecast_date<-flu_dates_25_26_retro[i]
    #################################################################### End Original code
    ####################################################################  Flusight code
    #Change to location of new data
    #out_path <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flusight-ensemble/Test")
    #hub_path <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/FluSight-forecast-hub")
    #hub_path <- "FluSight-forecast-hub"
    hub_con <- connect_hub(hub_path) 
    forecast_data <- hub_con |>
      dplyr::filter(
        reference_date == forecast_date, 
        stringr::str_detect(model_id, "Flusight-ensemble", negate=TRUE) # remove baseline and ensembles
      ) |> 
      dplyr::collect() |>
      as_model_out_tbl() 
    
    # if(!file.exists(paste0(out_path, "models-to-include-in-ensemble-", current_ref_date, ".csv"))){
    file_names = list.files(path = paste0(hub_path, "/model-metadata"))
    all_metadata = file_names[!(file_names %in% c("FluSight-ensemble", "FluSight-lop_norm")) &
                                !grepl(paste0(".md", collapse = "|"), file_names)]# %>%
    
    
    
    yml.files <- list.files(paste0(hub_path, "/model-metadata"), pattern = "\\.ya?ml$", full.names = T)
    
    designated_models <- character(0)
    file.names <- character(0)
    
    for (q in yml.files) {
      
      file.name <- tools::file_path_sans_ext(basename(q))
      file.names <- c(file.names, file.name)
      
      yml.dat <- yaml.load_file(q)
      
      if ("designated_model" %in% names(yml.dat)) {
        # Extract the value of "designated_model"
        designated_model_value <- yml.dat$designated_model
        designated_models <- c(designated_models, designated_model_value)
      } else {
        # If "designated_model" doesn't exist in the YAML file, you can set a default value or handle it as needed.
        designated_models <- c(designated_models, NA)  # For example, setting it to NA.
      }
    }
    
 
    eligible_models <- data.frame(Model = file.names, Designated_Model = designated_models) %>% 
      filter(Designated_Model == T|Model =="FluSight-baseline")
    models = as.character(eligible_models$Model)

    forecast_data <- forecast_data[forecast_data$model_id %in% models,]
    forecast_data <- forecast_data[forecast_data$location != 78,]
    forecast_data<-forecast_data%>%filter(is.na(forecast_data$value)==F)
    # QUANTILE ENSEMBLE
    forecast_data <- forecast_data |>
      dplyr::filter(output_type == "quantile") |>
      dplyr::filter(target == "wk inc flu hosp") |>
      dplyr::mutate(output_type_id=as.character(as.numeric(output_type_id))) # ensures quantiles treated the same regardless of presence of trailing zeros
    
    
    forecasts_6week<-rbind(forecasts_6week,forecast_data)
    
    
    
  } 
  
  
  
  #################################################################### End Flusight code
  #################################################################### Original code
  
  forecasts_6week<-forecasts_6week%>%dplyr::rename( quantile=output_type_id, target_variable=target,
                                                    model=model_id)%>%filter(horizon!=-1)
  #Only want states and territories
  #flu_truth_states<-list_as_of[[j+9]]%>%filter(location!='US')
  flu_truth_states <- flu_truth_current #%>% filter(location_name != "US")
  
  flu_truth_states$target_end_date<-as.Date(flu_truth_states$date)
  
  forecasts_6week$temporal_resolution<-'week'
  
  forecasts_6week$quantile<-as.numeric(forecasts_6week$quantile)
  #Preparing for the ensemble/scoring functions
  
  #wouldn't have truth data from the future, so limiting evaluation to only those forecast were we have data
  #as of the current simulated date
  forecasts_6week3<-forecasts_6week%>%mutate(days_off=current_ref_date-reference_date)%>% 
    filter((horizon==3 & days_off>=21) |(horizon==2 & days_off>=14) |(horizon==1 & days_off>=7)|(horizon==0 & days_off>=0))

  #6 week burn
  forecasts_6week_state<-forecasts_6week3#%>%filter(location!='US')
 
  #Now will subset to only those models that have the correct number of forecasts for the given week
  #Unable to effectively score the models if they are missing a week
  #Will do this separately for each burn period and separately for US vs states
  #Also adding in the truth data

  count_state6<-forecasts_6week_state%>%group_by( model)%>%
    dplyr::summarise(n=n())%>%filter(n==max(n)|model=="FluSight-baseline")

  #6 week burn

  forecasts_6week_state<-forecasts_6week_state%>%right_join(count_state6)%>%
    dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                     by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)
  
  #Evaluate the forecasts

  
  score_6week_eval_state <-forecasts_6week_state %>%unique()%>%check_forecasts() %>%score()%>% 
    add_coverage(ranges = c(50, 95), by = c("model")) %>%
    summarise_scores(by = c("model"),relative_skill=TRUE,  baseline="FluSight-baseline")%>%
    mutate(cov_50=round(coverage_50*100,2),
           cov_95=round(coverage_95*100,2),
           wis=round(interval_score,2),
           mae=round(ae_median,2),
           rel_wis=round(scaled_rel_skill,2))%>%
    dplyr::select(model, wis,rel_wis, mae, cov_50, cov_95)%>% arrange(rel_wis, wis)
  
  #Bring in the forecasts for the "current" date
  
  # Get the models to be included in the ensemble, ie loading the data for the current week
  #################################################################### End Original code
  #################################################################### Flusight code 
  forecast_data <- hub_con |>
    dplyr::filter(
      reference_date == current_ref_date, 
      stringr::str_detect(model_id, "FluSight", negate=TRUE) # remove baseline and ensembles
    ) |> 
    dplyr::collect() |>
    as_model_out_tbl() 
  
  # if(!file.exists(paste0(out_path, "models-to-include-in-ensemble-", current_ref_date, ".csv"))){
  file_names = list.files(path = paste0(hub_path, "/model-metadata"))
  all_metadata = file_names[!(file_names %in% c("FluSight-ensemble", "FluSight-lop_norm")) &
                              !grepl(paste0(".md", collapse = "|"), file_names)]# %>%
  
  
  
  yml.files <- list.files(paste0(hub_path, "/model-metadata"), pattern = "\\.ya?ml$", full.names = T)
  
  designated_models <- character(0)
  file.names <- character(0)
  
  for (q in yml.files) {
    
    file.name <- tools::file_path_sans_ext(basename(q))
    file.names <- c(file.names, file.name)
    
    yml.dat <- yaml.load_file(q)
    
    if ("designated_model" %in% names(yml.dat)) {
      # Extract the value of "designated_model"
      designated_model_value <- yml.dat$designated_model
      designated_models <- c(designated_models, designated_model_value)
    } else {
      # If "designated_model" doesn't exist in the YAML file, you can set a default value or handle it as needed.
      designated_models <- c(designated_models, NA)  # For example, setting it to NA.
    }
  }
  

  eligible_models <- data.frame(Model = file.names, Designated_Model = designated_models) %>% 
    filter(Designated_Model == T)
  #write.csv(eligible_models ,paste0(out_path, current_ref_date, ".csv"))
  
  #eligible_models = read.csv(paste0(out_path, current_ref_date, ".csv"),
                             #header = TRUE)
  models = as.character(eligible_models$Model)

  forecast_data <- forecast_data[forecast_data$model_id %in% models,]
  forecast_data <- forecast_data[forecast_data$location != 78,]
  forecast_data<-forecast_data%>%filter(is.na(forecast_data$value)==F)
  
  # QUANTILE ENSEMBLE
  forecast_data <- forecast_data |>
    dplyr::filter(output_type == "quantile") |>
    dplyr:: filter(target == "wk inc flu hosp") |>
    dplyr::mutate(output_type_id=as.character(as.numeric(output_type_id)), target_variable=target)%>%
    filter(horizon!=-1) # ensures quantiles treated the same regardless of presence of trailing zeros
  
  #################################################################### End Flusight code
  #################################################################### Original code 
  forecast_data_state<-forecast_data%>%mutate(model=model_id)#%>%filter(location!='US')
  #Done reading in the "current data"
  
  #Limiting the weight calculation to those that are actually included this week's forecasts
  models_current_state<-as.data.frame(unique(forecast_data_state$model_id))%>%dplyr::rename(model=`unique(forecast_data_state$model_id)`)
  
  score_6week_eval_state<-score_6week_eval_state%>%inner_join(models_current_state)%>%mutate(model_id=model)

  forecast_weight_state6 <- right_join(forecast_data_state, score_6week_eval_state)
  
  #Creating a grid search for the optimal parameter to scale the weights
  #Need to do this grid search separately for each ensemble we are investigating 
  
  ####################################################################################################################  


  
  #6 Week median
  train_search_overall<-data.frame()
  
  for(q in 1:length(theta)){
    #Weights  
    score_6week_eval_state$unorm_weights <- sigmoid(-score_6week_eval_state$rel_wis,theta[q])
    score_6week_eval_state$weight <- score_6week_eval_state$unorm_weights / sum(score_6week_eval_state$unorm_weights)
    weights_state6<-score_6week_eval_state[,c('model_id','weight')]
    
    if(max(weights_state6$weight) > 0.3){ #Max weight for a given model is 30%, can change
      
      break()
    } else{
      #Need to select a theta that minimizes the WIS over the training window.
      
      #Forecasts to include
      check_WIS<-forecasts_6week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                     value=prediction)
      
      #Keep only those with weights
      check_WIS<-check_WIS%>%right_join(weights_state6)%>%dplyr::select(!weight)
      
 
      #Calculate weighted median
      median_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state6,
                                                  agg_fun = "median",
                                                  model_id="Weighted Median train state6",
                                                  task_id_cols = task_id_cols)
      
      
      median_weight_state_train$temporal_resolution<-'week'
      
        #Calculate WIS
        median_weight_state_train2<-median_weight_state_train%>%#select(!check)%>%
          dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                           by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
          dplyr::rename(model=model_id, quantile=output_type_id)
        
        train_wis <- median_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
          add_coverage(ranges = c(50, 95), by = c("model")) %>%
          summarise_scores(by = c("model"))%>%
          mutate(cov_50=round(coverage_50*100,2),
                 cov_95=round(coverage_95*100,2),
                 wis=round(interval_score,2),
                 mae=round(ae_median,2))%>%
          dplyr::select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
        
        
        train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis))) 
      }
    }
  
  #Lowest WIS is first. Can then statee this to loop over the calculation of the model
  theta_state6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)

  for(k in 1:length(theta_state6)){
    #There are normalized weights
    weights_state6 <- as.data.frame(cbind(score_6week_eval_state$model_id,
                                          sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k])/
                                            sum(sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k]))
    ))
    colnames(weights_state6)<-c('model_id','weight')
    
    median_weight_state6<- simple_ensemble(forecast_weight_state6,#weights=weights_state6,
                                           agg_fun = "median",
                                           model_id="Weighted 6 week median",
                                           task_id_cols = task_id_cols)  
  }
  
  
  
  #6 Week mean
  train_search_overall<-data.frame()
  
  for(q in 1:length(theta)){
    #Weights  
    score_6week_eval_state$unorm_weights <- sigmoid(-score_6week_eval_state$rel_wis,theta[q])
    score_6week_eval_state$weight <- score_6week_eval_state$unorm_weights / sum(score_6week_eval_state$unorm_weights)
    weights_state6<-score_6week_eval_state[,c('model_id','weight')]
    if(max(weights_state6$weight) > 0.3){ #Max weight for a given model is 30%, can change
      
      break()
    } else{
      #Need to select a theta that minimizes the WIS over the training window.
      
      #Forecasts to include
      check_WIS<-forecasts_6week_state%>%filter(model!="Flusight-baseline")%>%mutate(model_id=model, output_type_id=quantile,
                                                                                     value=prediction)
      #Keep only those with weights
      check_WIS<-check_WIS%>%right_join(weights_state6)%>%dplyr::select(!weight)
      
      #Calculate weighted mean
      mean_weight_state_train<- simple_ensemble(check_WIS,weights=weights_state6,
                                                agg_fun = "mean",
                                                model_id="Weighted mean train state6",
                                                task_id_cols = task_id_cols)
      
      
      mean_weight_state_train$temporal_resolution<-'week'
      
        mean_weight_state_train2<-median_weight_state_train%>%#select(!check)%>%
          dplyr::left_join(flu_truth_states %>%  dplyr::select(target_end_date , location, true_value=value),
                           by = c("location", "target_end_date"))%>%dplyr::rename(prediction=value)%>%
          dplyr::rename(model=model_id, quantile=output_type_id)
        
        train_wis <- mean_weight_state_train2  %>%unique()%>%check_forecasts() %>%score()%>% 
          add_coverage(ranges = c(50, 95), by = c("model")) %>%
          summarise_scores(by = c("model"))%>%
          mutate(cov_50=round(coverage_50*100,2),
                 cov_95=round(coverage_95*100,2),
                 wis=round(interval_score,2),
                 mae=round(ae_median,2))%>%
          dplyr::select(model, wis, mae, cov_50, cov_95)%>% arrange( wis)
        
        
        train_search_overall<-train_search_overall%>%rbind(cbind(theta[q],mean(train_wis$wis))) 
      }
    }
  
  #Lowest WIS is first. Can then use this to loop over the calculation of the model
  theta_state6<-(train_search_overall%>%arrange(by_group=V2))[1]%>%dplyr::pull(V1)
  
  for(k in 1:length(theta_state6)){
    #There are normalized weights
    weights_state6 <- as.data.frame(cbind(score_6week_eval_state$model_id,
                                          sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k])/
                                            sum(sigmoid(-score_6week_eval_state$rel_wis,theta_state6[k]))
    ))
    colnames(weights_state6)<-c('model_id','weight')
    
    mean_weight_state6<- simple_ensemble(forecast_weight_state6,weights=weights_state6,
                                         agg_fun = "mean",
                                         model_id="Weighted 6 week mean",
                                         task_id_cols = task_id_cols)  
    
  }
  
    
  # median_weight_state6 and mean_weight_state6 are the 6-week trained ensembles that are produced by the code
  
  #Change to submission format
  
  median_weight_state6<-median_weight_state6%>%dplyr::select(reference_date,location,horizon,target_variable,target_end_date,
                                                                                         output_type,output_type_id,value) %>% 
    dplyr::rename(target=target_variable)
  mean_weight_state6<-mean_weight_state6%>%dplyr::select(reference_date,location,horizon,target_variable,target_end_date,
                                                      output_type,output_type_id,value) %>% 
    dplyr::rename(target = target_variable)
  

  #out_path <- paste0("C:/Users/",Sys.info()["user"],"/Desktop/GitHub/Flusight-ensemble")
  ensemble_name <- "FluSight-trained_med"
  flusight_ensemble_path <- paste("model-output/", ensemble_name, "/", current_ref_date, "-", ensemble_name, ".csv", sep="") 
  readr::write_csv(median_weight_state6, flusight_ensemble_path)
  ensemble_name<-"FluSight-trained_mean"
  flusight_ensemble_path <- paste("model-output/", ensemble_name, "/", current_ref_date, "-", ensemble_name, ".csv", sep="") 
  readr::write_csv(mean_weight_state6, flusight_ensemble_path)
  
 
