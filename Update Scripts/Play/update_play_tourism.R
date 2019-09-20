

library(tidyverse)

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")


# Get data ----------------------------------------------------------------

# tourism spending data from https://partners.visitnc.com/economic-impact-studies
# On the page, use the dropdown for county specific data and select Guilford County

# Copy the data and save it as travel_expenditures.xlsx in Data Sources/Play/Tourism

tourism <- read_xlsx("./Data Sources/Play/Tourism/travel_expenditures.xlsx") %>% 
  clean_names() %>% 
  filter(!is.na(year)) %>% 
  mutate(expenditures = as.numeric(expenditures), 
         tax_savings = as.numeric(tax_savings)) %>% 
  select(year, expenditures, tax_savings)


save(tourism, file = "./shiny/data/tourism.rda")

