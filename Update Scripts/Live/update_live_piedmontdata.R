# This file contains data from the Piedmont Health Caounts dashboard

library(tidyverse)
library(zoo)


# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/GuilfordCIP")

# Food insecurity ---------------------------------------------------------

# From http://www.piedmonthealthcounts.org/indicators/index/view?indicatorId=2107&localeId=1982&localeChartIdxs=1

# Download data into a csv and save it in the folder Data Sources/Live/Piedmont Data

food_insecurity <- read_csv("./Data Sources/Live/Piedmont Data/food_insecurity.csv") %>% clean_names() %>% 
  filter(location == "Guilford") %>%
  select(indicator_value, period_of_measure)

save(food_insecurity, file = "./shiny/data/food_insecurity.rda")


# Infant Mortality Rate ---------------------------------------------------

# From http://www.piedmonthealthcounts.org/indicators/index/view?indicatorId=289&localeTypeId=2

# Download data into a csv and save it in the folder Data Sources/Live/Piedmont Data

imr <- read_csv("./Data Sources/Live/Piedmont Data/infant_mortality_rate.csv") %>% clean_names()

imr <- imr %>% 
  filter(location == "Guilford", period_of_measure=="2013-2017") %>% 
  select(indicator_value, breakout_subcategory, breakout_value) %>% 
  add_row(breakout_subcategory = "All") %>% 
  mutate(breakout_value = case_when(breakout_subcategory == "All"~na.locf(indicator_value),
                                    TRUE~breakout_value)) %>% 
  select(category = breakout_subcategory, value = breakout_value) %>% 
  mutate(category = str_remove(category, ", non-Hispanic")) %>% 
  rowid_to_column() %>% 
  arrange(desc(rowid)) %>% 
  select(-rowid)

save(imr, file = "./shiny/data/imr.rda")


# Diabetes ----------------------------------------------------------------

# From http://www.piedmonthealthcounts.org/indicators/index/view?indicatorId=40&localeTypeId=2&periodId=271&comparisonId=927

# Download data   into a csv and save it in the folder Web Sources (within Updated data)

diabetes <- read_csv("./Data Sources/Live/Piedmont Data/diabetes.csv") %>% clean_names()

diabetes <- diabetes %>% 
  filter(location == "Guilford") %>% 
  arrange(desc(period_of_measure)) %>% 
  slice(1) %>% 
  select(indicator_value)

save(diabetes, file = "./shiny/data/diabetes.rda")











