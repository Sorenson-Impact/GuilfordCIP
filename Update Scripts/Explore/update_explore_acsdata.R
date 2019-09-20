require(tidyverse)
require(leaflet)
require(tidycensus)
require(sf)
require(janitor)

# Options ---------------------------------------------------------------------------------------------------------

#If uncertain about most recent ACS year, check the release schedule here: https://www.census.gov/programs-surveys/acs/news/data-releases.html
most_recent_acs_year <- 2017

# You will need to set the census api key using the following code.   DO NOT SAVE THE KEY in this file, as it will be visible to anyone on github!  Contact Tara if you do not have the key.

#census_api_key("your key here") #do not save key here, run this in console


# Define Functions ------------------------------------------------------------------------------------------------

acsvars <- bind_rows(acs1 = load_variables(most_recent_acs_year, "acs1", cache = T),
                     acs5 = load_variables(most_recent_acs_year, "acs5", cache = T),
                     .id = "dataset") %>%
  add_column(year = most_recent_acs_year) %>%
  mutate(level = str_count(label, pattern = "!!")) %>%
  rowwise() %>%
  mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>%
  ungroup() %>%
  mutate(concept = str_to_title(concept)) %>%
  rename(variable = name) %>%
  separate(col = variable, into = c("table", "row"), sep = "_", remove = F) %>%
  group_by(year, dataset, table) %>%
  mutate(label_lead = lead(label)) %>%
  mutate(is_summary = str_detect(label_lead, label) & !is.na(label_lead)) %>%
  ungroup() %>%
  select(-label_lead)


options(tigris_use_cache = TRUE)

si_acs <- function(table,
                   geography = "county",
                   county = NULL,
                   state = NULL,
                   summary_var = "universe total",
                   survey = "acs5",
                   year = most_recent_acs_year,
                   discard_summary_rows = TRUE,
                   geometry = FALSE,
                   keep_geo_vars = FALSE,
                   shift_geo = TRUE,
                   key = NULL) {

  acsvars <- acsvars %>% filter(dataset == survey, year == year)

  #shift_geo settings
  if(geography != "county" | geography != "state") shift_geo <- F #shift_geo is only available for states and counties
  if(!geometry) shift_geo <- F #get_acs() will throw and error if we ask for shift without asking for geo

  #Set up the summary var
  if(acsvars %>% filter(table == !!table) %>% nrow() > 1) { #Some tables only have 1 row, and therefore no summary row

    if(summary_var == "universe total") summary_var = paste0(table, "_001")
    summary_label = acsvars %>% filter(variable == summary_var) %>% pull(levlab)
  } else summary_var <- NULL

  df <- get_acs(geography = geography,
                table = table,
                county = county,
                state = state,
                output = "tidy",
                year = year,
                cache_table = T,
                summary_var = summary_var,
                survey = survey,
                geometry = geometry,
                keep_geo_vars = keep_geo_vars,
                shift_geo = shift_geo,
                key = key) %>%
    clean_names() %>%
    left_join(acsvars) %>%
    select(-matches("summary_moe")) %>% #may not have this for a 1 row table, matches avoids an error
    select(geoid, geo_name = name, level, levlab, estimate, everything()) %>%
    try(rename(!!summary_label := summary_est)) #may not have this for a 1 row table

  if(discard_summary_rows) {
    df <- df %>%
      filter(!is_summary)
    message("Dropping summary rows.  To keep, run this function with `discard_summary_rows = FALSE`")
  }

  return(df)
}


# Pull tables -----------------------------------------------------------------------------------------------------

explore_tables <- tribble(~short_title, ~table, ~geography, ~value_type,
                          "Total Population", "B01003", "block group", "estimate",
                          "Median Household Income", "B19013", "block group", "estimate",
                          "White Population", "B02008", "block group", "estimate",
                          "Black Population", "B02009", "block group", "estimate",
                          "Am. Indian & Alaska Native Pop.", "B02011", "block group", "estimate",
                          "Asian Population", "B02010", "block group", "estimate",
                          "Native Hawaiian and Pacific Isl. Population", "B02012", "block group", "estimate",
                          "Other Population", "B02013", "block group", "estimate",
                          "Receipt of Food Stamps / SNAP", "B22003_002", "tract", "percent")

explore_years <- 2013:(most_recent_acs_year - 1)

explore_acsdata <- explore_tables %>%
  group_split(table) %>%
  map(function(table) {

    var_only <- str_detect(table$table, ".*_\\d{3}") #if we specified a variable in the table column

    as_percent <- table$value_type == "percent"

    if(var_only) { #split the variable into table and row
      table <- table %>% separate(table, into = c("table", "row"), sep = "_")
    }


    #First get the most recent year with the geometry attached
    geo_table <- si_acs(table$table, geography = table$geography, year = most_recent_acs_year, county = "Guilford County", state = "NC", geometry = T, key = temp_census_api_key, discard_summary_rows = !var_only) %>%
      st_transform(crs = "+init=epsg:4326") %>%
      add_column(short_title = table$short_title)

    if(as_percent) {
      geo_table <- geo_table %>% mutate(estimate = estimate / summary_est)
    }

    geo_table <- geo_table %>%
      rename(!!paste0("est", most_recent_acs_year) := estimate) %>%
      select(-moe, -year, -matches("Total"), -matches("summary_est")) #some tables may not have these, so this "matches" avoids an error



    #Now get the rest without geometry and attach to the table with geometry
    data_tables <- map(explore_years, function(explore_years) {
      new_table <- si_acs(table$table, geography = table$geography, year = explore_years, county = "Guilford County", state = "NC", geometry = F, key = temp_census_api_key, discard_summary_rows = !var_only)

      if(as_percent) {
        new_table <- new_table %>% mutate(estimate = estimate / summary_est)
      }

      new_table <- new_table %>%
        rename(!!paste0("est", explore_years) := estimate) %>%
        select(-moe, -year, -matches("Total"), -matches("summary_est")) #some tables may not these, so this "matches" avoids an error
    }) %>%
      reduce(left_join)

    if(var_only) { #Cull it if we were passed a specific variable
      geo_table <- geo_table %>% filter(row == !!table$row)
      data_tables <- data_tables %>% filter(row == !!table$row)
    }

    combined_table <- left_join(geo_table, data_tables) %>%
      add_column(as_percent)



    return(combined_table)
  })

write_rds(explore_acsdata, "~/Google Drive/SI/DataScience/data/Guilford County CIP/dashboard/explore_acsdata.rds")