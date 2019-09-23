
library(tidyverse)

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/GuilfordCIP")


# Get Data -------------------------------------------------------------------------

# data from Weather and Climate Database 
# Link: https://climate.ncsu.edu/cronos/?station=KGSO&temporal=monthly

# Options to select:
# ------------------
# Retreive data from 48 months
# Daily Mean Air Temperature ((max+min)/2)
# Daily max air temperature	 average
# Daily min air temperature
# Average Daily Relative Humidity
# Daily Precipitation

# Download the data as an excel file, and save it as weather.csv in Data Sources/Play/Weather


# Clean data --------------------------------------------------------------

weather_raw <- read_csv(file = "./Data Sources/Play/Weather/weather.csv", skip = 13, col_names = T) %>% clean_names()

weather <- weather_raw %>% 
  mutate(month = month(parse_date_time(date_time_est, orders = "my"))) %>% 
  rename(daily_avg = monthly_avg_of_2m_mean_daily_temperature_f,
         max_temp = monthly_avg_of_2m_daily_max_air_temperature_f, 
         min_temp = monthly_avg_of_2m_daily_min_temperature_f,
         ht_index = x2m_calculated_maximum_heat_index_f,
         rel_humidity = monthly_avg_of_2m_relative_humidity_daily_avg_percent,
         precipitation = monthly_sum_of_2m_daily_precipitation_in) %>% 
  group_by(month) %>% 
  summarise_if(is.numeric, mean, na.rm = T)

save(weather, file = "./shiny/data/weather.rda")
