require(tidyverse)
require(googleway)
require(readxl)
require(janitor)
require(naniar)


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


# You will need to set the google api key using the following code in the console.   DO NOT SAVE THE KEY in this file, as it will be visible to anyone on github!  Contact Tara if you do not have the key.

set_key("your key here") #Run in console only!

# Be sure that you have enabled the "Places API" in your developer console.



# Functions -------------------------------------------------------------------------------------------------------

#This function handles badly formatted excel files that have info spread over two headers.
read_excel_twoheaders <- function(path) {
  merged_names <- suppressMessages(read_excel(path, col_names = F)) %>%
    slice(1:2) %>%
    rownames_to_column %>%
    gather(var, value, -rowname) %>%
    spread(rowname, value) %>%
    fill(`1`, .direction = "down") %>%
    mutate(merged_name = paste(`1`, `2`, sep = "_"),
           merged_name = str_remove(merged_name, "_NA$"),
           merged_name = make_clean_names(merged_name)) %>%
    pull(merged_name)

  read_excel(path, col_names = merged_names) %>%
    slice(-1:-2) %>%
    mutate_if(lst(~Hmisc::all.is.numeric(., extras = c(".", "NA", NA, NA_character_))), as.numeric)
}


# Load Data -------------------------------------------------------------------------------------------------------

#NOTE: FOR ALL YEAR VALUES WE ARE USING THE END OF SCHOOL YEAR.  2017-2018 School Year = 2018

grd3_read <- read_excel("./Data Sources/Learn/Guilford County Schools/GCS Grade 3 EOG Reading Proficiency.xlsx") %>%
  clean_names() %>%
  fill(school, .direction = "down") %>% #deal with merged cells
  filter(school != "Guilford County Schools") %>% #remove county average
  filter(!is.na(year)) %>%
  replace_with_na(replace = list(reading_eog_3 = "NA")) %>%
  mutate(year = as.numeric(year),
         reading_eog_3 = as.numeric(reading_eog_3)) %>%
  rename(value = reading_eog_3) %>%
  add_column(label_metric = "End of Grade 3 Reading Proficiency")

kdib <- read_excel_twoheaders("./Data Sources/Learn/Guilford County Schools/GCS Kindergarten DIBELS BOY by School 2017-18 & 2018-19.xlsx") %>%
  rename(school = school_name) %>%
  filter(!is.na(gcs_school_code)) %>% #get rid of the 2nd header row and the county average
  select(-gcs_school_code) %>%
  gather(key = year, value = value, -school) %>%
  replace_with_na(replace = list(value = "*")) %>%
  mutate(year = parse_number(year) + 1,
         value = as.numeric(value)) %>%
  add_column(label_metric = "DIBELS At or Above Benchmark")

hsgrad <- read_excel_twoheaders("./Data Sources/Learn/Guilford County Schools/GCS Graduation Rates By School Over Time.xlsx") %>%
  filter(code != "LEA") %>% #remove county average
  select(-contains("rate_change"), -contains("number_students_in_cohort"), -code) %>%
  gather(key = year, value = value, -school) %>%
  mutate(year = parse_number(year)) %>%
  add_column(label_metric = "Graduation Rate")


act <- read_excel("./Data Sources/Learn/Guilford County Schools/GCS Grade 11 ACT Performance Over Time.xlsx") %>%
  clean_names() %>%
  rename(year = reporting_year) %>%
  mutate(act_proficiency = parse_number(act_proficiency)) %>% #has values of ">95".  This makes them "95"
  rename(value = act_proficiency) %>%
  add_column(label_metric = "Grade 11 ACT Proficiency")

post_hs_enrolled <- read_excel_twoheaders("./Data Sources/Learn/Guilford County Schools/GCS Student Clearinghouse Information By School & District May 2019.xlsx") %>%
  filter(school != "GUILFORD COUNTY SCHOOLS") %>%
  select(school, contains("1st_year_after")) %>% #Decision point to use first year rather than fall after HS
  select(-contains("change_from")) %>%
  gather(key = year, value = value, -school) %>%
  mutate(year = parse_number(year)) %>%
  add_column(label_metric = "College Attendance")

schools <- lst(post_hs_enrolled, act, hsgrad, kdib, grd3_read) %>%
  bind_rows(.id = "metric") %>%
  mutate(school = str_squish(school)) %>%
  mutate(label_school_year = paste0(year - 1, "-", year)) %>%
  group_by(year, metric) %>%
  mutate(value = case_when(any(value > 1, na.rm = T) & all(value <= 100, na.rm = T) ~ value / 100,
                           TRUE ~ value)) %>% #Convert any using 0-100 to percent scale 0-1
  mutate(mean_metric_year = mean(value, na.rm = T)) %>%
  ungroup()



# Geocode ---------------------------------------------------------------------------------------------------------

schools <- schools %>%
  distinct(school) %>%
  mutate(place = map(school,
                     function(school) {
                       google_geocode(address = paste0(school, ", Guilford County, North Carolina")) %>%
                         geocode_coordinates() %>%
                         slice(1) #Just keep the first in case there are multiple results.  This is prone to errors because it isn't based on any ranking.
                       }
                     )) %>%
  unnest(place) %>%
  right_join(schools)



# Building data for dash --------------------------------------------------

schools1 <- schools %>% 
  select(school, lat, lng, metric, year, value, label_metric, label_school_year)



schools2 <- schools %>% 
  select(school, lat, lng, metric, year, value = mean_metric_year, label_metric, label_school_year) %>% 
  mutate(metric = str_glue("{metric}_p"),
         metric = as.character(metric),
         label_metric = str_glue("Mean {label_metric}"),
         label_metric = as.character(label_metric),
         value = round(value,3))


schools <- bind_rows(schools1,schools2)


# Save file for dashboard -----------------------------------------------------------------------------------------

save(schools, file = "./shiny/data/schools.rda")



