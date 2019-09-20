library(tidyverse)

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")


# Data Source -------------------------------------------------------------

#This is data from the National Regiter of Deeds, obtained by contacting Jason Jones
# Save the data in the Data Sources/Live/Births


# Clean and organize the data ---------------------------------------------
births <- read_csv("./Data Sources/Live/Births/birthRecordsMaster.csv") %>%  clean_names()

births <- births %>% 
  select(gender, child_dob) %>% 
  mutate(yob = year(child_dob)) %>% 
  filter(gender == "M"|gender=="F") %>% 
  filter(yob >=2013, yob <2019) %>% 
  group_by(gender, yob) %>% 
  summarise(tot_births = n()) %>% 
  pivot_wider(names_from = gender, values_from = tot_births) %>% 
  rename(Females = `F`, Males = `M`) %>% 
  mutate(`All Births` = Females+Males)

save(births, file = "./shiny/data/births.rda")

