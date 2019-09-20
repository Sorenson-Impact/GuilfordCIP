require(tidyverse)
require(leaflet)
require(tidycensus)
require(sf)
require(janitor)
library(crayon)

# Instructions ----------------------------------------------------------------------------------------------------
#This data was provided in response to a request. There is no automation or API to update it.

#It will be necessary to either request additional data with the inclusion of previous years going back to the start years of the existing data, or to do a bind_rows with new files and eliminate any duplicates.  Check that column headers are the same for new data.

#Data source:
# Data came via Tara Sandercock (tsandercock@cfgg.org) but originated from:
# Judith M. Penny, PhD
# Executive Director, Accountability & Research
# Guilford County Schools
# 501 West Washington Street
# Greensboro, NC  27401
# 336 – 370-8323
# pennyj@gcsnc.com


# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")

# Functions -------------------------------------------------------------------------------------------------------

# This is the fucntion to call data from ACS and format it so we can work with it easily

acsvars <- load_variables(2017, "acs1", cache = T) %>%
  mutate(level = str_count(label, pattern = "!!")) %>%
  rowwise() %>%
  mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>%
  ungroup() %>%
  mutate(concept = str_to_title(concept)) %>%
  rename(variable = name)

si_acs <- function(table, county = NULL, state = NULL, summary_var = "universe total", geography = NULL, survey = NULL) {
  cat(yellow(bold("Reminder: You must stay within the same level for any summary to be valid!\n")))
  
  if(summary_var == "universe total") summary_var = paste0(table, "_001")
  summary_label = acsvars %>% filter(variable == summary_var) %>% pull(levlab)
  
  get_acs(geography = geography,
          table = table,
          county = county,
          state = state,
          output = "tidy",
          year = 2017,
          cache_table = T,
          summary_var = summary_var, 
          geometry = FALSE, 
          cb = FALSE, 
          survey = survey) %>%
    clean_names() %>%
    left_join(acsvars) %>%
    select(-summary_moe, -variable) %>%
    select(geoid, county = name, level, levlab, estimate, everything()) %>%
    rename(!!summary_label := summary_est)
}




# Data pulls --------------------------------------------------------------
pop_age_gc <- si_acs("B01001", county = "Guilford County", state = "NC", geography = "county", survey = "acs1")

pop_age_cities <- si_acs("B01001", geography = "place", state = "NC",  survey = "acs1") %>% 
  filter(str_detect(county, "Greens")|str_detect(county, "High"))

pop_age <- bind_rows(pop_age_gc, pop_age_cities)

med_age_gc <- si_acs("B01002", geography = "county", state = "NC", survey = "acs1", county = "Guilford County")

med_age_cities <- si_acs("B01002", geography = "place", state = "NC",  survey = "acs1")%>% 
  filter(str_detect(county, "Greens")|str_detect(county, "High"))

med_age <- bind_rows(med_age_gc, med_age_cities)

# Infographics ------------------------------------------------------------

tot_pop_guilford <- pop_age %>% 
  filter(level==1, str_detect(county, "Guilford")) %>% 
  pull(estimate)

tot_pop_greensboro <- pop_age %>% 
  filter(level==1, str_detect(county, "Greens")) %>% 
  pull(estimate)

tot_pop_highpoint <- pop_age %>% 
  filter(level==1, str_detect(county, "High")) %>% 
  pull(estimate)

med_age_guilford <- med_age %>% 
  filter(levlab=="Total", str_detect(county, "Guilford")) %>% 
  pull(estimate)

med_age_greensboro <- med_age %>% 
  filter(levlab=="Total", str_detect(county, "Greens")) %>% 
  pull(estimate)

med_age_highpoint <- med_age %>% 
  filter(levlab=="Total", str_detect(county, "High")) %>% 
  pull(estimate)


# Age Distribution --------------------------------------------------------


ages <- pop_age %>%  filter(level ==3) %>% 
  select(levlab, estimate, label, county) %>% 
  group_by(county, levlab) %>% 
  summarise(total = sum(estimate)) %>% 
  spread(levlab, total) %>% 
  mutate(`0-9 Years` = sum(`Under 5 years`,`5 to 9 years` ),
         `10-19 Years` = sum(`10 to 14 years`, `15 to 17 years`, `18 and 19 years` ), 
         `20-29 Years` = sum(`20 years`, `21 years`, `22 to 24 years`, `25 to 29 years`),
         `30-39 Years` = sum(`30 to 34 years`, `35 to 39 years`), 
         `40-49 Years` = sum(`40 to 44 years`, `45 to 49 years`), 
         `50-59 Years` = sum(`50 to 54 years`, `55 to 59 years`),
         `60-69 Years`= sum(`60 and 61 years`, `62 to 64 years`, `65 and 66 years`, `67 to 69 years`),
         `70 - 79 Years` = sum(`70 to 74 years`, `75 to 79 years`), 
         `80 Years and Over` = sum(`80 to 84 years`, `85 years and over`)) %>% 
  select(county, contains("Years", ignore.case = F)) %>% 
  gather(c(2:10), key = "Ages", value = "Total") %>% 
  mutate(denom = sum(Total),
         perc = round((Total/denom)*100),0,
         location = str_remove(county, ", North Carolina")) %>% 
  ungroup() %>% 
  select(Ages, perc, location) 
  
save(ages, file = "./shiny/data/ages.rda")


# Sex ---------------------------------------------------------------------

sex <- pop_age %>% 
  filter(level == 2) %>% 
  select(levlab, estimate, location = county) %>% 
  mutate(location = str_remove(location, ", North Carolina"))

save(sex, file = "./shiny/data/sex.rda")


# Race --------------------------------------------------------------------


race_vec <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

race_gc <- map_df(race_vec, function(x) {
  tablenum <- paste0("B01001",x)
  si_acs(tablenum, county = "Guilford County", state = "NC", geography = "county", survey = "acs1") %>% 
    mutate(race = x) %>% 
    filter(level == 1) 
  
})


race_city <- map_df(race_vec, function(x) {
  tablenum <- paste0("B01001",x)
  si_acs(tablenum, geography = "place", state = "NC",  survey = "acs1") %>% 
    filter(str_detect(county, "Greens")|str_detect(county, "High")) %>% 
    mutate(race = x) %>% 
    filter(level == 1) 
  
})

race_ethn <- bind_rows(race_gc, race_city) %>% 
  select(county, estimate, race) %>% 
  mutate(race = case_when(race == "A" ~ "White",
                          race == "B"~ "Black or African American",
                          race == "C" ~ "American Indian and Alaska Native",
                          race == "D" ~ "Asian",
                          race == "E" ~ "Native Hawaiian and Other Pacific Islander",
                          race == "F" ~ "Some Other Race",
                          race == "G" ~ "Two or More Races",
                          race == "H" ~ "Not Hispanic or Latino", 
                          race == "I" ~ "Hispanic or Latino")) %>% 
  mutate(county = str_remove(county, ", North Carolina"))
  

race <- race_ethn %>% 
  filter(race!= "White Alone, Not Hispanic or Latino", race!= "Hispanic or Latino") %>% 
  filter(!is.na(estimate)) %>% 
  select(race, n = estimate, location = county)

save(race, file = "./shiny/data/race_all.rda")


# Ethnicity ---------------------------------------------------------------

ethnicity <- race_ethn %>% 
  filter(race== "White Alone, Not Hispanic or Latino"| race== "Hispanic or Latino") %>% 
  filter(!is.na(estimate)) %>% 
  select(race, n = estimate, location = county)

save(ethnicity, file = "./shiny/data/ethnicity_all.rda")
