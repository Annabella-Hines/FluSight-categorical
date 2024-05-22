# Observed categories for 2023-2024


library(tidyverse)
library(zoo)
library(RSocrata)
#pull target data 

#this function is the fetch flu function from get_target_data that is used for FluSight GitHub
#this pulls the final NHSN update from "https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh"
## including this archive in case the above link stops functioning in the future 
# "https://healthdata.gov/dataset/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/qqte-vkut/about_data"

fetch_flu <- function(temporal_resolution = "weekly", na.rm = TRUE){
  require(dplyr)
  require(lubridate)
  require(RSocrata)
  
  #read data from healthdata.gov, filtering for when flu reporting became mandatory
  health_data = RSocrata::read.socrata(url = "https://healthdata.gov/resource/g62h-syeh.json") %>% 
    dplyr::filter(date >= as.Date("2022-02-02"))
  
  #remove  VI and AS as they are not included for FluSight, keep only necessary vars and add epiweek and epiyear 
  recent_data = health_data %>% 
    dplyr::filter(!state %in% c("VI", "AS")) %>% 
    dplyr::select(state, date, previous_day_admission_influenza_confirmed) %>% 
    dplyr::rename("value" = "previous_day_admission_influenza_confirmed") %>% 
    dplyr::mutate(date = as.Date(date), 
                  value = as.numeric(value),
                  epiweek = lubridate::epiweek(date), 
                  epiyear = lubridate::epiyear(date))
  
  #summarize US Flu 
  us_data = recent_data %>% dplyr::group_by(date, epiweek, epiyear) %>% 
    dplyr::summarise(value = sum(value, na.rm = na.rm)) %>% 
    dplyr::mutate(state = "US") %>% 
    dplyr::ungroup()
  
  #bind state and US data
  full_data = rbind(recent_data, us_data) %>% 
    dplyr::left_join(., locations, by = join_by("state" == "abbreviation"))
  
  #convert counts to weekly and calculates weekly rate 
  weeklydat = full_data %>% 
    dplyr::group_by(state,epiweek,epiyear, location, location_name, population) %>% 
    dplyr::summarise(value = sum(value, na.rm = na.rm), date = max(date), num_days = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(num_days == 7L) %>% 
    dplyr::select(-num_days, -epiweek, -epiyear) %>% 
    dplyr::mutate(weekly_rate = (value*100000)/population )
  
  #if daily data is ever wanted, this returns correct final data
  if(temporal_resolution == "weekly"){
    final_dat = weeklydat %>% 
      dplyr::select(date, location, location_name, value, weekly_rate) %>% 
      dplyr::arrange(desc(date))
  } else{
    final_dat = full_data 
  }
  return(final_dat)
  
}

#location data is necessary to use fetch_flu function, must be named locations
locations <- read.csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/auxiliary-data/locations.csv") %>% dplyr::select(1:4)

target_data <- fetch_flu(temporal_resolution = "weekly")

#filter for beginning of FluSight Season
#join with locations, get rates diffs, set categories
obs_24 <- target_data %>% 
  filter(date >= as.Date("2023-10-01")) %>%
  select(-c(location))%>%
  dplyr::inner_join(locations,
                    by = c("location_name"))  %>% 
  group_by(location_name) %>% 
  arrange(date) %>% 
  mutate(rate_diff0 = weekly_rate - lag(weekly_rate, 1), 
         rate_diff1 = weekly_rate - lag(weekly_rate, 2), 
         rate_diff2 = weekly_rate - lag(weekly_rate, 3), 
         rate_diff3 = weekly_rate - lag(weekly_rate, 4),
         count_change0 = value - lag(value, 1), 
         count_change1 = value - lag(value, 2), 
         count_change2 = value - lag(value, 3), 
         count_change3 = value - lag(value, 4)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(rate_diff0, rate_diff1, rate_diff2, rate_diff3), names_to = "horizon", names_prefix = "rate_diff", values_to = "rate_diff", names_transform = list(horizon = as.integer)) %>% 
  mutate(category = case_when(abs(count_change0) < 10 | horizon == 0 & rate_diff < 1 & rate_diff > -1 ~ "stable",
                              horizon == 0 & rate_diff > 2 ~ "large_increase", 
                              horizon == 0 & rate_diff < -2 ~ "large_decrease", 
                              horizon == 0 & rate_diff >= 1 ~ "increase", 
                              horizon == 0 & rate_diff <= -1 ~ "decrease", 
                              abs(count_change1) < 10 | horizon == 1 & rate_diff < 1 & rate_diff > -1 ~ "stable", 
                              horizon == 1 & rate_diff > 3 ~ "large_increase", 
                              horizon == 1 & rate_diff < -3 ~ "large_decrease", 
                              horizon == 1 & rate_diff >= 1 ~ "increase", 
                              horizon == 1 & rate_diff <= -1 ~ "decrease", 
                              abs(count_change2) < 10 | horizon == 2 & rate_diff < 2  & rate_diff > -2~ "stable", 
                              horizon == 2 & rate_diff > 4 ~ "large_increase", 
                              horizon == 2 & rate_diff < -4 ~ "large_decrease", 
                              horizon == 2 & rate_diff >= 2 ~ "increase", 
                              horizon == 2 & rate_diff <= -2 ~ "decrease", 
                              abs(count_change3) < 10 | horizon == 3 & rate_diff < 2.5  & rate_diff > -2.5 ~ "stable", 
                              horizon == 3 & rate_diff > 5 ~ "large_increase", 
                              horizon == 3 & rate_diff < -5 ~ "large_decrease", 
                              horizon == 3 & rate_diff >= 2.5 ~ "increase", 
                              horizon == 3 & rate_diff <= -2.5 ~ "decrease"), 
         season = 24) %>% 
  rename(count = value) %>% 
  select(date, location_name, location, horizon, rate_diff, category, season, weekly_rate, count)

obs_data_with_numcat <- obs_24 %>% 
  mutate(numeric_category := factor(category, 
                                    levels= c("large_decrease", "decrease", "stable",
                                              "increase", "large_increase"),
                                    labels = seq(-2,2)) )
write.csv(obs_data_with_numcat,  paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-categorical/Target_data/obs_24_with_numcat.csv"), row.names = FALSE)
