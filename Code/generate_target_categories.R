## Observed categories for 2023-2024
## CDC FluSight Team
## May 24, 2024

library(tidyverse)

#pull target data 

## 2022-2023
target_23 <- read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-truth/truth-Incident%20Hospitalizations.csv")

#location data (2021-2022/2022-2023)
locations_23 <-read.csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv") %>%
  mutate(count_rate1=NULL,count_rate2=NULL,location=NULL) %>% rename(count_rate1 = count_rate1per100k,
                                                                     count_rate2 = count_rate2per100k)
#calculate 2022-2023 weekly rates using 2022-2023 population data
obs_23 <- target_23 %>%  
  filter(date < as.Date("2023-10-01")) %>% 
  dplyr::inner_join(locations_23,
                    by = c("location_name"))  %>% 
  filter(date > as.Date("2022-02-02"), abbreviation != "VI") %>%
  mutate(weekly_rate = value*100000/population, 
         season = "2022-2023") %>% select(-abbreviation, -population)

## 2023-2024
target_24 <-  readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/target-data/target-hospital-admissions.csv") %>% select(-`...1`)

# load location data 
locations_24 <- readr::read_csv(file = "https://raw.githubusercontent.com/cdcepi/FluSight-forecast-hub/main/auxiliary-data/locations.csv") %>%
  dplyr::select(location,location_name, count_rate1, count_rate2, count_rate2p5, count_rate3, count_rate4, count_rate5)

#filter 2023-2024 data for 2023-2024 season, pull in location data with count rates
obs_24 <- target_24 %>% filter(date >= as.Date("2023-10-01")) %>%
  select(-c(location))%>%
  dplyr::inner_join(locations_24,
                    by = c("location_name")) %>% 
  mutate(season = "2023-2024")
  
#bind obs 23 and 24 data, calculate rate differences at all the horizons 
obs_bind <- bind_rows(mutate(obs_23, date = as.Date(date)), obs_24) %>% 
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
  ungroup() 

#pivot longer for ease of use and create horizons
mandatory_reporting <- obs_bind %>% 
  pivot_longer(cols = c(rate_diff0, rate_diff1, rate_diff2, rate_diff3), names_to = "horizon", names_prefix = "rate_diff", values_to = "rate_diff", names_transform = list(horizon = as.integer)) 


#apply 2022-2023 definition to all data prior to start of 2023-2024 season
definition23 <- mandatory_reporting %>% filter(season == "2022-2023", horizon == 1) %>% 
  group_by(location_name) %>%
  mutate(category = ifelse(abs(count_change1) <20, "stable",NA)) %>%
  mutate(category = ifelse(abs(rate_diff)<1, "stable",category)) %>%
  mutate(category = ifelse((abs(rate_diff) >1 & abs(count_change1) <20), "stable",category)) %>%
  mutate(category = ifelse((rate_diff >1 & abs(count_change1) >=20),"increase",category)) %>%
  mutate(category = ifelse((rate_diff >2 & abs(count_change1) >=40),"large_increase",category)) %>%
  mutate(category = ifelse((rate_diff <(-1) & abs(count_change1) >=20),"decrease",category)) %>%
  mutate(category = ifelse((rate_diff <(-2) & abs(count_change1) >=40),"large_decrease",category)) %>% 
  mutate(horizon = 1, 
         rate_diff = rate_diff, 
         date = as.Date(date)) %>% 
  ungroup()  %>% 
  mutate(numeric_category := factor(category, 
                                    levels= c("large_decrease", "decrease", "stable",
                                              "increase", "large_increase"),
                                    labels = seq(-2,2)) ) %>% 
  select(date, location, location_name, season, horizon, weekly_rate, value, rate_diff, category, numeric_category)

#apply 2023-2024 definition to data starting with the 2023-2024 season
definition24 <- mandatory_reporting %>% filter(season == "2023-2024") %>% 
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
         date = as.Date(date)) %>% 
  select(date, location_name, location, horizon, rate_diff, category, season, weekly_rate, value) %>% 
  mutate(numeric_category := factor(category, 
                                    levels= c("large_decrease", "decrease", "stable",
                                              "increase", "large_increase"),
                                    labels = seq(-2,2)))

#bind all data to observed ata with numeric categories 
obs_data_with_numcat <- bind_rows(definition23, definition24) 

# write.csv(obs_data_with_numcat,  paste0("C:/Users/", Sys.info()["user"], "/Desktop/GitHub/FluSight-categorical/Target_data/obs_data_with_numcat.csv"), row.names = FALSE)

# ##testing viz to make sure everything looks right
# obs_data_with_numcat %>% filter(horizon == 1) %>% 
#   ggplot(aes(x = date, y = location_name))+
#   geom_tile(aes(fill = factor(category, levels= c("large_decrease", "decrease", "stable", "increase", "large_increase"))))+
#   scale_fill_manual(values = c("#006166",	"#3BBBB0",	"#E3E3E3",	"#C13897",	"#6B0057"),
#                     breaks = c("large_decrease", "decrease", "stable", "increase", "large_increase"),
#                     labels = c("Large decrease", "Decrease", "Stable", "Increase", "Large increase"),
#                     na.value = "grey50", 
#                     drop = FALSE)+
#   scale_y_discrete(limits = rev)+
#  # scale_x_date(breaks = seq(min(obs_data_with_numcat$date+3.1), max(obs_data_with_numcat$date+3.1), by = "month"),
#  #              labels = format(sort(unique(obs_data_with_numcat$date)), "%b %d"),
#   #             expand = c(0,0))+
#   labs(x = NULL, y = NULL, fill = "Category")+
#   theme(axis.text.x = element_text(angle =45, hjust =1))


