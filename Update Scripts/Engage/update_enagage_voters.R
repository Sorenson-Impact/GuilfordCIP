library(tidyverse)

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")


# Data Source -------------------------------------------------------------

#This is data from the NC State Board of Elections, obtained by contacting Jason Jones

# NOTE:::::Save the data in locally since this is a large file and git hub will not allow pushing/pulling it


# Clean and organize the data ---------------------------------------------
#vote <- read_csv("link on system/ncvoter41.csv") 


# Race codes: "A" "B" "I" "M" "O" "U" "W"

# A: Asian
# B: Black
# I: American Indian
# M: Multiracial
# O: Other
# U: Unknown
# W: White

active_voters <- vote %>% 
  filter(voter_status_desc == "ACTIVE") %>% 
  select(county_id, county_desc, voter_status_desc, race_code, ethnic_code, party_cd, gender_code, birth_year)


active_voters <- active_voters %>% 
  mutate(party_cd = case_when(party_cd == "CST"~"Constitution Party",
                              party_cd == "DEM"~ "Democratic Party",
                              party_cd == "GRE"~"Green Party",
                              party_cd == "LIB" ~ "Libertarian Party", 
                              party_cd == "REP"~ "Republican Party",
                              party_cd == "UNA" ~ "Unaffiliated")) %>% 
  mutate(race_code = case_when(race_code == "A" ~ "Asian",
                               race_code == "B" ~ "Black or African American", 
                               race_code == "I"~"American Indian or Alaska Native",
                               race_code == "M" ~ "Multiracial", 
                               race_code == "O" ~"Other", 
                               race_code == "U" ~"Unknown", 
                               race_code == "W" ~"White")) %>% 
  mutate(gender_code = case_when(gender_code == "M" ~ "Male", 
                                 gender_code == "F" ~ "Female", 
                                 gender_code == "U" ~ "Unknown"))


save (active_voters, file = "./shiny/data/voters.rda")
