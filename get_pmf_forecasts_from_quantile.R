#' Obtain pmf forecasts from quantile forecasts for flu hosp rate change categories
#'
#' @param quantile_forecasts a `model_out_tbl` of quantile forecasts with the 
#'   following columns: `model_id`, `location`, `reference_date`, `horizon`, 
#'   `target`, `target_end_date`, `output_type`, `output_type_id`, `value`
#' @param locations_df a data frame of locations to forecast for. Must contain 
#'   the following columns: `geo_value` (lowercase abbreviations), `location` 
#'   (fips codes), and `population`
#' @param truth_df a data frame of truth data with the following columns: 
#'   `geo_value`, `time_value`, `value`
#' @param categories a vector of strings containing category names (without spaces).
#' @param horizons a vector of integers containing the horizons for which categories
#'   are defined. Defaults to 1, which should be used if categories are not 
#'   horizon-specific.
#' @param count_rate_multiplier a matrix containing the count rate multipliers used
#'   to define categories. Must have dimension length(horizons) x length(categories)
#' @param category_rule a matrix containing the values against which to compare the
#'   count rates used to define categories when count rates are below the specified
#'   values. Must have dimension length(horizons) x length(categories)
#' @param target_name a string of the desired target name for the output forecasts
#'
#' @return a `model_out_tbl` of both pmf and quantile forecasts constructed from the 
#'   input quantile forecasts with the following columns: `model_id`, `location`,
#'   `reference_date`, `horizon`, `target`, `target_end_date`, `output_type`, 
#'   `output_type_id`, `value`
#' @export
#'
#' @examples
#' 
#' library(distfromq)
#' library(ggforce)
#' library(gridExtra)
#' library(reshape2)
#' horizons_2324_new <- 0:3; categories <- c("large_increase", "increase", "stable", "decrease", "large_decrease")
#' count_rate_multiplier_2324_new <- matrix(c(c(2, 3, 4, 5), c(1, 1, 2, 2.5), c(-1, -1, -2, -2.5), c(-2, -3, -4, -5)), ncol=4)
#' category_rule_2324_new <- matrix(c(rep(10, 4), rep(10, 4), rep(-10, 4), rep(-10, 4)), ncol=4)

get_pmf_forecasts_from_quantile <- function(quantile_forecasts, locations_df, truth_df, categories, horizons=1, count_rate_multiplier, category_rule, target_name="wk flu hosp rate change") {
  num_cat = length(categories)
  
  truth_df_all <- truth_df %>%
    dplyr::rename(target_end_date = time_value) %>%
    dplyr::ungroup() %>%
    #dplyr::inner_join(locations_df, by = c("geo_value"))  %>% #already joined location df with target data due to geo value being abbreviations
    dplyr::mutate(model_id="Observed Data", target_variable=target_name, .before=1)
  
  # Calculate category boundary values
  criteria_df_temp <- truth_df_all 
  criteria_df_all <- NULL
  for (j in 1:length(horizons)) {
    criteria_df_temp[["horizon"]] <- horizons[j]
    for (i in 1:(num_cat-1)) {
      criteria_df_temp[[paste0("crit", i, sep="")]] <-
        ifelse(
          abs(criteria_df_temp[["population"]]*count_rate_multiplier[j,i]/100000) < abs(category_rule[j,i]), 
          criteria_df_temp[["value"]] + category_rule[j,i], 
          criteria_df_temp[["value"]] + 
            round(criteria_df_temp[["population"]]*count_rate_multiplier[j,i]/100000, digits=0)
        )
    }
    criteria_df_all <- bind_rows(criteria_df_all, criteria_df_temp) #changed to bind_rows instead of rbind because we were losing horizon
  }
  
  criteria_df_all <- criteria_df_all %>%
    dplyr::select(model_id, location, value, target_end_date, horizon,target_variable, population, crit1:ncol(criteria_df_all)) %>%
    dplyr::filter(!is.na(value))
  
  train_forecasts <- criteria_df_all %>%
    dplyr::select(location, horizon, target_end_date, target_variable, population, crit1:ncol(criteria_df_all)) %>%
    dplyr::mutate(date=target_end_date+weeks(1), target_end_date = date+weeks(horizon), .before = 3) 
  
  
  # extract log pdf and cdf values for training set forecasts
  # we add a little noise to the value column so that there is a density to
  # work with in case the forecaster had a point mass anywhere
  quantile_forecasts <- quantile_forecasts %>%
    dplyr::mutate(reference_date=as.Date(reference_date)) 
  
  # filter for dates, horizons, locations to forecast for
  criteria_df_filtered <- train_forecasts %>%
    dplyr::inner_join(
      quantile_forecasts,
      by = c("date"="reference_date", "horizon", "target_end_date", "location")
    ) %>% mutate(output_type_id = as.numeric(output_type_id)) #getting errors this wasn't numeric
  
  # filter for dates, horizons, locations to forecast for (no distinct output_type_ids)
  train_forecasts <- criteria_df_filtered %>%
    dplyr::distinct(model_id, location, date, horizon, target, .keep_all=TRUE) %>%
    dplyr::select(-target, -output_type,-output_type_id,-value)
  
  # Calculate cdf category boundary values
  for (i in 1:(num_cat-1)) {
    criteria_df_filtered[["crit_current"]] <- criteria_df_filtered[[paste0("crit", i, sep="")]] 
    train_temp <- criteria_df_filtered %>%
      dplyr::group_by(model_id, date, location, horizon, target, target_end_date) %>%
      dplyr::summarize(
        cdf_crit_current = distfromq::make_p_fn(
          ps = output_type_id,
          qs = value)(unique(crit_current), log = FALSE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(target, target_end_date))
    train_forecasts[[paste0("cdf_crit", i)]] <- train_temp[["cdf_crit_current"]]
  }
  
  #calculate category percentages from cdf criteria, correcting for negative numbers
  exp_forecast <- train_forecasts %>%
    dplyr::ungroup() %>%
    dplyr::rename(reference_date=date, target=target_variable) %>%
    dplyr::mutate(cdf_crit0=1, .before=cdf_crit1)
  
  # exp_forecast[[paste0("cdf_crit", num_cat)]] <- exp_forecast[[paste0("crit", num_cat)]] <- 0
  # cdf_crit_sum <- 0
  # for (i in 1:(num_cat)) {
  #   if (cdf_crit_sum < 1) {
  #     exp_forecast[[categories[i]]] <- exp_forecast[[paste0("cdf_crit", i-1)]] - ifelse(exp_forecast[[paste0("crit", i)]] > 0, exp_forecast[[paste0("cdf_crit", i)]], 0)
  #   } else {
  #     exp_forecast[[categories[i]]] <- 0
  #   }
  #   cdf_crit_sum <- cdf_crit_sum + mean(exp_forecast[[categories[i]]])
  # }
  
  exp_forecast[[paste0("cdf_crit", num_cat)]] <-
    exp_forecast[[paste0("crit", num_cat)]] <- 0
  exp_forecast[["cdf_crit_sum"]] <- 0
  for (i in 1:(num_cat)) {
    exp_forecast[[categories[i]]] <-
      ifelse(exp_forecast[["cdf_crit_sum"]] < 1,
             ifelse(exp_forecast[[paste0("crit", i)]] > 0,
                    exp_forecast[[paste0("cdf_crit", i-1)]] - 
                      exp_forecast[[paste0("cdf_crit", i)]],
                    exp_forecast[[paste0("cdf_crit", i-1)]] - 0),
             0)
    exp_forecast[["cdf_crit_sum"]] <- 
      exp_forecast[["cdf_crit_sum"]] + exp_forecast[[categories[i]]]
  }
  
  exp_forecast <- exp_forecast %>%
    dplyr::select(model_id,reference_date,location,horizon, all_of(categories))
  
  
  #transpose data_frame to format for submission
  exp_t = melt(
    exp_forecast,
    id.vars = c("model_id","reference_date","location","horizon"),
    measure.vars = categories,
    variable.name="output_type_id",
    value.name="value"
  )
  exp_t <- exp_t %>%
    dplyr::mutate(target=target_name, output_type="pmf") %>%
    dplyr::select(model_id, reference_date, horizon, target, location, output_type, output_type_id, value)
  
  output_forecasts <- exp_t %>%
    dplyr::mutate(
      output_type_id=as.character(output_type_id),
      target_end_date=reference_date+weeks(horizon)
    ) %>%
    dplyr::bind_rows(mutate(quantile_forecasts, output_type_id=as.character(output_type_id)))%>%
    dplyr::filter(location %in% pull(locations_df, location)) %>%
    dplyr::select(model_id, reference_date, horizon, target, target_end_date, location, output_type, output_type_id, value)
  
  return (output_forecasts)
}
