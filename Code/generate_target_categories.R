# Observed categories for 2023-2024


library(tidyverse)

#pull target data 

## 2022-2023
target_23 <- read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-truth/truth-Incident%20Hospitalizations.csv")


#location data (2021-2022/2022-2023)
locations_23 <-read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv") %>%
  mutate(count_rate1=NULL,count_rate2=NULL,location=NULL)

obs_23 <- target_23 %>%  
  filter(date >= as.Date("2022-10-03"), date <= as.Date("2023-06-10")) %>% 
  dplyr::inner_join(locations_23,
                    by = c("location_name"))  %>% 
  filter(date > as.Date("2022-02-20"), abbreviation != "VI") %>%
  mutate(weekly_rate = value*100000/population) %>% 
  
  group_by(location_name) %>% 
  arrange(date) %>% 
  mutate(rate_diff = weekly_rate-lag(weekly_rate, 2),
         count_change = value - lag(value, 2), 
         count = value) %>% 
  ungroup() %>% 
  group_by(location_name,population) %>%
  mutate(category = ifelse(abs(count_change) <20, "stable",NA)) %>%
  mutate(category = ifelse(abs(rate_diff)<1, "stable",category)) %>%
  mutate(category = ifelse((abs(rate_diff) >1 & abs(count_change) <20), "stable",category)) %>%
  mutate(category = ifelse((rate_diff >1 & abs(count_change) >=20),"increase",category)) %>%
  mutate(category = ifelse((rate_diff >2 & abs(count_change) >=40),"large_increase",category)) %>%
  mutate(category = ifelse((rate_diff <(-1) & abs(count_change) >=20),"decrease",category)) %>%
  mutate(category = ifelse((rate_diff <(-2) & abs(count_change) >=40),"large_decrease",category)) %>% 
  mutate(horizon = 1, 
         rate_diff = rate_diff, 
         date = as.Date(date), 
         season = 23) %>% 
  ungroup()  %>% 
  mutate(numeric_category := factor(category, 
                                    levels= c("large_decrease", "decrease", "stable",
                                              "increase", "large_increase"),
                                    labels = seq(-2,2)) ) %>% 
  select(date, location_name, location, horizon, rate_diff, category, season, weekly_rate, count, numeric_category)


## 2023-2024
target_24 <-  readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/target-data/target-hospital-admissions.csv") %>% select(-`...1`)

# load location data 
locations_24 <- readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/auxiliary-data/locations.csv") %>%
  dplyr::select(location,location_name, count_rate1, count_rate2, count_rate2p5, count_rate3, count_rate4, count_rate5)


#filter for beginning of FluSight Season
#join with locations, get rates diffs, set categories
obs_24 <- target_24 %>% 
  filter(date >= as.Date("2023-10-01")) %>%
  select(-c(location))%>%
  dplyr::inner_join(locations_24,
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
  select(date, location_name, location, horizon, rate_diff, category, season, weekly_rate, count) %>% 
  mutate(numeric_category := factor(category, 
                                    levels= c("large_decrease", "decrease", "stable",
                                              "increase", "large_increase"),
                                    labels = seq(-2,2)) )




obs_data_with_numcat <- bind_rows(obs_23, obs_24)

# write.csv(obs_data_with_numcat,  paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-categorical/Target_data/obs_data_with_numcat.csv"), row.names = FALSE)



