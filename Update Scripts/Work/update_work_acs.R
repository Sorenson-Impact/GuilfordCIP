require(tidyverse)
require(leaflet)
require(tidycensus)
require(sf)
require(janitor)

# ACS Tables: 
# ACS B23002 (A-G)
# ACS B19013 (A-G)
# ACS B19013 (A-G)
# ACS B08105 (A-G)
# ACS B08105 (A-G)

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/GuilfordCIP")


# si_acs fn ---------------------------------------------------------------

acsvars <- load_variables(2017, "acs1", cache = T) %>%
  mutate(level = str_count(label, pattern = "!!")) %>%
  rowwise() %>%
  mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>%
  ungroup() %>%
  mutate(concept = str_to_title(concept)) %>%
  dplyr::rename(variable = name)


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



# Employed Percentage Bar -------------------------------------------------

emp_df <- lst()

for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
  tablenum <- paste0("B23002", letter)
  
  df_temp <- si_acs(tablenum, geography = "county", state = "NC", county  = "Guilford County", survey = "acs1") %>% 
    select(county, estimate, label, level) %>% 
    mutate(race = letter) 
  
  emp_df[[letter]] <- df_temp
}


emp_race <- bind_rows(emp_df)

emp_race <- emp_race %>% 
  mutate(race = case_when(race == "A" ~ "White alone",
                          race == "B"~ "Black or African American Alone",
                          race == "C" ~ "American Indian and Alaska Native Alone",
                          race == "D" ~ "Asian Alone",
                          race == "E" ~ "Native Hawaiian and Other Pacific Islander Alone",
                          race == "F" ~ "Some Other Race Alone",
                          race == "G" ~ "Two or More Races",
                          race == "H" ~ "White Alone, Not Hispanic or Latino", 
                          race == "I" ~ "Hispanic or Latino")) %>% 
  filter(race == "White alone"|race == "Black or African American Alone") %>% 
  filter(!is.na(estimate)) %>% 
  filter(str_detect(label, "Civilian")|str_detect(label,"Employed")|str_detect(label, "65 to 69")|str_detect(label, "70 years")) %>% 
  filter(!str_detect(label, "Unemployed")) %>% 
  filter(!str_detect(label, "Not in labor")) %>% 
  separate(label, c("l1", "l2", "l3", "l4", "l5", "l6", "l7"), sep = "!!", remove = FALSE) %>% 
  rename(sex = l3,
         ages = l4,
         laborforce = l5,
         civ_staus = l6,
         emp_status = l7) %>% 
  select(-l1, -l2) %>% 
  unite(sex_race, c(sex, race)) %>% 
  mutate(emp_status = case_when(civ_staus == "Employed"~ "Employed", 
                                TRUE~emp_status)) %>% 
  mutate(laborforce = case_when(str_detect(ages, "65")|str_detect(ages, "70")~"In labor force",
                                TRUE~laborforce)) %>% 
  filter(!is.na(laborforce))  %>% 
  filter(!(str_detect(ages, "70")&level ==4)) %>% 
  filter(!(str_detect(ages, "65")&level ==4)) %>%
  select(-civ_staus) %>% 
  group_by(ages, sex_race) %>% 
  arrange(emp_status, .by_group = TRUE) %>% 
  mutate(perc = round(estimate/lead(estimate)*100),0) %>% 
  ungroup() %>% 
  filter(!is.na(perc)) %>% 
  select(sex_race, perc, ages) %>% 
  spread(sex_race, perc) %>% 
  rename(`Female: Black or African American Alone` = `Female_Black or African American Alone`, 
         `Female: White Alone` = `Female_White alone`,
         `Male: Black or African American Alone` = `Male_Black or African American Alone`,
         `Male: White alone` = `Male_White alone`)

save(emp_race, file = "./shiny/data/emp_race.rda")

# Median Household Income by Race -----------------------------------------

med_inc_df <- lst()

for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
  tablenum <- paste0("B19013", letter)
  
  df_temp <- si_acs(tablenum, geography = "county", state = "NC", county  = "Guilford County", survey = "acs1") %>% 
    select(county, estimate, label, level) %>% 
    mutate(race = letter) 
  
  med_inc_df[[letter]] <- df_temp
}


med_income <- bind_rows(med_inc_df)

med_income <- med_income %>% 
  mutate(race = case_when(race == "A" ~ "White alone",
                          race == "B"~ "Black or African American Alone",
                          race == "C" ~ "American Indian and Alaska Native Alone",
                          race == "D" ~ "Asian Alone",
                          race == "E" ~ "Native Hawaiian and Other Pacific Islander Alone",
                          race == "F" ~ "Some Other Race Alone",
                          race == "G" ~ "Two or More Races",
                          race == "H" ~ "White Alone, Not Hispanic or Latino", 
                          race == "I" ~ "Hispanic or Latino"))

med_income_race <- med_income %>% 
  filter(race!="White Alone, Not Hispanic or Latino") %>% 
  filter(race!="Hispanic or Latino") %>% 
  select(race, estimate)

save(med_income_race, file = "./Updated data/med_income_race.rda")


# Median Income by ethnicity ----------------------------------------------

med_income_ethn <- med_income %>% 
  filter(race=="White Alone, Not Hispanic or Latino"| race =="Hispanic or Latino" ) %>% 
  select(race, estimate)

save(med_income_ethn, file = "./shiny/data/med_income_ethn.rda")


# Means of Transportation to Work Race ------------------------------------

tr_df <- lst()

for(letter in c("A", "B", "C", "D", "E", "F", "G", "H", "I")) {
  tablenum <- paste0("B08105", letter)
  
  df_temp <- si_acs(tablenum, geography = "county", state = "NC", county  = "Guilford County", survey = "acs1") %>% 
    select(county, estimate, label, level, Total) %>% 
    mutate(race = letter) 
  
  tr_df[[letter]] <- df_temp
}


transp <- bind_rows(tr_df)

transp <- transp %>% 
  mutate(race = case_when(race == "A" ~ "White alone",
                          race == "B"~ "Black or African American Alone",
                          race == "C" ~ "American Indian and Alaska Native Alone",
                          race == "D" ~ "Asian Alone",
                          race == "E" ~ "Native Hawaiian and Other Pacific Islander Alone",
                          race == "F" ~ "Some Other Race Alone",
                          race == "G" ~ "Two or More Races",
                          race == "H" ~ "White Alone, Not Hispanic or Latino", 
                          race == "I" ~ "Hispanic or Latino"))

transp_race <- transp %>% 
  filter(level!=1) %>% 
  filter(race == "White alone"|race == "Black or African American Alone"|race == "Two or More Races") %>% 
  mutate(label = str_remove(label, "Estimate!!Total!!")) %>% 
  mutate(perc = round(estimate/Total*100,0)) %>% 
  select(race, label, perc) %>% 
  spread(race, perc)

save(transp_race, file = "./shiny/data/transp_race.rda")


# Means of Transportation by ethnicity ------------------------------------

transp_ethn <- transp %>% 
  filter(level!=1) %>% 
  filter(race == "White Alone, Not Hispanic or Latino"|race == "Hispanic or Latino") %>% 
  mutate(label = str_remove(label, "Estimate!!Total!!")) %>% 
  mutate(perc = round(estimate/Total*100,0)) %>% 
  select(race, label, perc) %>% 
  filter(perc!=0) %>% 
  spread(race, perc)

save(transp_ethn, file = "../shiny/data/transp_eth.rda")



