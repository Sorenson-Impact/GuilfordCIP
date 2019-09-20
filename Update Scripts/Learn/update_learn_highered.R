require(tidyverse)
require(ipeds)
require(janitor)
require(rscorecard)

# Options ---------------------------------------------------------------------------------------------------------

survey_year <- 2018

#College Scorecard API
# You will need an API key to pull the college scorecard data.  You can get one here: https://api.data.gov/signup/

# You will need to set the college scorecard API key using the following code in the console.   DO NOT SAVE THE KEY in this file, as it will be visible to anyone on github!

sc_key("your key here") #Run in console only!

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")

# Download all surveys --------------------------------------------------------------------------------------------


download_ipeds(year = survey_year) #By default it looks for the year 1 previous to the current year. (ie now - 1).


# Get GEOID info --------------------------------------------------------------------------------------------------

hd <- ipeds_survey("HD", year = survey_year) %>%
  as_tibble() %>%
  clean_names() %>%
  select(unitid, instnm, zip, stabbr, countynm, sector, latitude, longitud) %>%
  mutate(sector = recodeSector(sector)) %>%
  mutate(is_guilford = countynm == "Guilford County")

# Entering class size ---------------------------------------------------------------------------------------------


efa <- ipeds_survey("EFA", year = survey_year) %>%
  as_tibble() %>%
  clean_names()

total_students <- efa %>%
  filter(efalevel == 1) %>% #All Students, Total (ie includes graduate and undergraduate)
  select(unitid,
         total_students = eftotlt) %>%
  left_join(hd) %>%
  filter(!str_detect(sector, "Private for-profit")) %>%
  filter(is_guilford) %>%
  select(-is_guilford)


# Retention Rates -------------------------------------------------------------------------------------------------

efd <- ipeds_survey("EFD", year = survey_year) %>%
  as_tibble() %>%
  clean_names() %>%
  select(unitid,
         full_time_retention_rate = ret_pcf) %>%
  mutate(full_time_retention_rate = full_time_retention_rate / 100) %>%
  left_join(hd)

retention_guilford <- efd %>%
  filter(is_guilford) %>%
  select(instnm, full_time_retention_rate)

retention_nc <- efd %>%
  filter(stabbr == "NC") %>%
  filter(str_detect(sector, "Public")) %>%
  group_by(sector) %>%
  dplyr::summarize(full_time_retention_rate = mean(full_time_retention_rate, na.rm = T)) %>%
  mutate(instnm = paste0("NC State Avg: ", sector)) %>%
  select(-sector)

retention_natl <- efd %>%
  filter(str_detect(sector, "Public")) %>%
  group_by(sector) %>%
  dplyr::summarize(full_time_retention_rate = mean(full_time_retention_rate, na.rm = T)) %>%
  mutate(instnm = paste0("National Avg: ", sector)) %>%
  select(-sector)

retention_rates <- bind_rows(guilford = retention_guilford, ncstate = retention_nc, national = retention_natl, .id = "group")


# Graduation ------------------------------------------------------------------------------------------------------

gr <- ipeds_survey("GR", year = survey_year) %>%
  as_tibble() %>%
  clean_names() %>%
  left_join(hd)

#From the frequencies tab of https://nces.ed.gov/ipeds/datacenter/data/GR2017_Dict.zip.  If IPEDS changes the code definitions this will need to be updated, however this is unlikely as the only time they did this was pre 2006 and in 2008 for race/ethnicity.
grad_status_codes <- tribble(~var, ~code, ~descrip,
  "CHRTSTAT",  10, "Revised cohort",
  "CHRTSTAT",  11, "Exclusions",
  "CHRTSTAT",  12, "Adjusted cohort (revised cohort minus exclusions)",
  "CHRTSTAT",  13, "Completers within 150% of normal time",
  "CHRTSTAT",  14, "Completers of programs of less than 2 years (150% of normal time)",
  "CHRTSTAT",  15, "Completers of programs of 2 but less than 4 years (150% of normal time)",
  "CHRTSTAT",  16, "Completers of bachelor's or equivalent degrees (150% of normal time)",
  "CHRTSTAT",  17, "Completers of bachelor's or equivalent degrees in 4 years or less",
  "CHRTSTAT",  18, "Completers of bachelor's or equivalent degrees in 5 years",
  "CHRTSTAT",  19, "Completers of bachelor's or equivalent degrees in 6 years",
  "CHRTSTAT",  20, "Transfer-out students",
  "CHRTSTAT",  22, "Completers of programs within 100% of normal time total",
  "CHRTSTAT",  23, "Completers of programs of < 2 yrs within 100% of normal time (not available by race or gender)",
  "CHRTSTAT",  24, "Completers of programs of 2 but < 4 yrs within 100% of normal time (not available by race or gender)",
  "CHRTSTAT",  31, "Noncompleters, still enrolled",
  "CHRTSTAT",  32, "Noncompleters, no longer enrolled"
  )

cohort_data_codes <- tribble(~var, ~code, ~descrip,
                       "GRTYPE",  40, "Total exclusions 4-year schools",
                       "GRTYPE",   2, "4-year institutions, Adjusted cohort (revised cohort minus exclusions)",
                       "GRTYPE",   3, "4-year institutions, Completers within 150% of normal time",
                       "GRTYPE",   4, "4-year institutions, Transfer-out students",
                       "GRTYPE",  41, "4-year institutions, noncompleters still enrolled",
                       "GRTYPE",  42, "4-year institutions, No longer enrolled",
                       "GRTYPE",   6, "Bachelor's or equiv subcohort (4-yr institution)",
                       "GRTYPE",   7, "Bachelor's or equiv subcohort (4-yr institution) exclusions",
                       "GRTYPE",   8, "Bachelor's or equiv subcohort (4-yr institution) adjusted cohort (revised cohort minus exclusions)",
                       "GRTYPE",   9, "Bachelor's or equiv subcohort (4-yr institution) Completers within 150% of normal time total",
                       "GRTYPE",  10, "Bachelor's or equiv subcohort (4-yr institution) Completers of programs of < 2 yrs (150% of normal time)",
                       "GRTYPE",  11, "Bachelor's or equiv subcohort (4-yr institution) Completers of programs of 2 but <4 yrs (150% of normal time)",
                       "GRTYPE",  12, "Bachelor's or equiv subcohort (4-yr institution) Completers of bachelor's or equiv degrees total (150% of normal time)",
                       "GRTYPE",  13, "Bachelor's or equiv subcohort (4-yr institution) Completers of bachelor's or equiv degrees in 4 years or less",
                       "GRTYPE",  14, "Bachelor's or equiv subcohort (4-yr institution) Completers of bachelor's or equiv degrees in 5 years",
                       "GRTYPE",  15, "Bachelor's or equiv subcohort (4-yr institution) Completers of bachelor's or equiv degrees in 6 years",
                       "GRTYPE",  16, "Bachelor's or equiv subcohort (4-yr institution) Transfer-out students",
                       "GRTYPE",  43, "Bachelor's or equiv subcohort (4-yr institution) noncompleters still enrolled",
                       "GRTYPE",  44, "Bachelor's or equiv subcohort (4-yr institution), No longer enrolled",
                       "GRTYPE",  18, "Other degree/certif-seeking subcohort (4-yr institution)",
                       "GRTYPE",  19, "Other degree/certificate-seeking subcohort(4-yr institution) exclusions",
                       "GRTYPE",  20, "Other degree/certif-seeking subcohort (4-yr institution) Adjusted cohort (revised cohort minus exclusions)",
                       "GRTYPE",  21, "Other degree/certif-seeking subcohort (4-yr institution) Completers within 150% of normal time total",
                       "GRTYPE",  22, "Other degree/certif-seeking subcohort (4-yr institution) Completers of programs < 2 yrs (150% of normal time)",
                       "GRTYPE",  23, "Other degree/certif-seeking subcohort (4-yr institution) Completers of programs of 2 but < 4 yrs (150% of normal time)",
                       "GRTYPE",  24, "Other degree/certif-seeking subcohort (4-yr institution) Completers of bachelor's or equiv degrees (150% of normal time)",
                       "GRTYPE",  25, "Other degree/certif-seeking subcohort (4-yr institution) Transfer-out students",
                       "GRTYPE",  45, "Other degree/certif-seeking subcohort (4-yr institution) noncompleters still enrolled",
                       "GRTYPE",  46, "Other degree/certif-seeking subcohort (4-yr institution) No longer enrolled",
                       "GRTYPE",  27, "Degree/certif-seeking students ( 2-yr institution)",
                       "GRTYPE",  28, "Degree/certificate-seeking subcohort(2-yr institution) exclusions",
                       "GRTYPE",  29, "Degree/certif-seeking students ( 2-yr institution) Adjusted cohort (revised cohort minus exclusions)",
                       "GRTYPE",  30, "Degree/certif-seeking students ( 2-yr institution) Completers within 150% of normal time total",
                       "GRTYPE",  31, "Degree/certif-seeking students ( 2-yr institution) Completers of programs of < 2 yrs (150% of normal time)",
                       "GRTYPE",  32, "Degree/certificate-seeking students ( 2-yr institution) Completers of programs of 2 but < 4 yrs (150% of normal time)",
                       "GRTYPE",  35, "Degree/certif-seeking students ( 2-yr institution) Completers within 100% of normal time total",
                       "GRTYPE",  36, "Degree/certif-seeking students ( 2-yr institution) Completers of programs of < 2 yrs (100% of normal time)",
                       "GRTYPE",  37, "Degree/certificate-seeking students ( 2-yr institution) Completers of programs of 2 but < 4 yrs (100% of normal time)",
                       "GRTYPE",  33, "Degree/certif-seeking students ( 2-yr institution) Transfer-out students",
                       "GRTYPE",  47, "Degree/certif-seeking students ( 2-yr institution) noncompleters still enrolled",
                       "GRTYPE",  48, "Degree/certif-seeking students ( 2-yr institution) No longer enrolled"
                       )


gr <- gr %>% left_join(grad_status_codes %>% select(-var), by = c("chrtstat" = "code")) %>%
  select(-chrtstat) %>%
  rename(graduation_rate_status_in_cohort = descrip) %>%
  left_join(cohort_data_codes %>% select(-var), by = c("grtype" = "code")) %>%
  select(-grtype) %>%
  rename(cohort_data = descrip) %>%
  rename(grand_total = grtotlt)


grad <- gr %>%
  filter(graduation_rate_status_in_cohort %in% c(
    "Adjusted cohort (revised cohort minus exclusions)",
    "Completers within 150% of normal time",
    "Completers of programs of less than 2 years (150% of normal time)",
    "Completers of programs of 2 but less than 4 years (150% of normal time)",
    "Completers of bachelor's or equivalent degrees in 6 years",
    "Completers of bachelor's or equivalent degrees (150% of normal time)",
    "Transfer-out students"
  )) %>%
  filter(cohort_data %in% c(
    "4-year institutions, Adjusted cohort (revised cohort minus exclusions)",
    "4-year institutions, Completers within 150% of normal time",
    "4-year institutions, Transfer-out students",
    "Degree/certif-seeking students ( 2-yr institution) Adjusted cohort (revised cohort minus exclusions)",
    "Degree/certif-seeking students ( 2-yr institution) Completers within 150% of normal time total",
    "Degree/certif-seeking students ( 2-yr institution) Transfer-out students"
  ))

grad <- grad %>%
  mutate(graduation_rate = graduation_rate_status_in_cohort) %>%
  mutate(cohort_data = paste(cohort_data, 'grand_total', sep = "_")) %>%
  select(unitid, cohort_data, grand_total) %>%
  spread(cohort_data, grand_total) %>%
  clean_names() %>%
  mutate(
    transfer_students_grand_total = case_when(
      !is.na(x4_year_institutions_transfer_out_students_grand_total) ~ x4_year_institutions_transfer_out_students_grand_total,
      !is.na(
        degree_certif_seeking_students_2_yr_institution_transfer_out_students_grand_total
      ) ~ degree_certif_seeking_students_2_yr_institution_transfer_out_students_grand_total
    )
  ) %>%
  mutate(
    adjusted_cohort_grand_total = case_when(
      !is.na(
        x4_year_institutions_adjusted_cohort_revised_cohort_minus_exclusions_grand_total
      ) ~ x4_year_institutions_adjusted_cohort_revised_cohort_minus_exclusions_grand_total,
      !is.na(
        degree_certif_seeking_students_2_yr_institution_adjusted_cohort_revised_cohort_minus_exclusions_grand_total
      ) ~ degree_certif_seeking_students_2_yr_institution_adjusted_cohort_revised_cohort_minus_exclusions_grand_total
    )
  ) %>%
  mutate(
    completes_150perc_grand_total = case_when(
      !is.na(
        x4_year_institutions_completers_within_150_percent_of_normal_time_grand_total
      ) ~ x4_year_institutions_completers_within_150_percent_of_normal_time_grand_total,
      !is.na(
        degree_certif_seeking_students_2_yr_institution_completers_within_150_percent_of_normal_time_total_grand_total
      ) ~ degree_certif_seeking_students_2_yr_institution_completers_within_150_percent_of_normal_time_total_grand_total
    )
  ) %>%
  select(
    unitid,
    adjusted_cohort_grand_total,
    transfer_students_grand_total,
    completes_150perc_grand_total
  )

grad <- grad %>%
  mutate(completion_rate = completes_150perc_grand_total / adjusted_cohort_grand_total) %>%
  select(-completes_150perc_grand_total, -adjusted_cohort_grand_total) %>%
  left_join(hd)

grad_guilford <- grad %>%
  filter(is_guilford) %>%
  select(instnm, completion_rate)

grad_nc <- grad %>%
  filter(stabbr == "NC") %>%
  filter(str_detect(sector, "Public")) %>%
  group_by(sector) %>%
  dplyr::summarize(completion_rate = mean(completion_rate, na.rm = T)) %>%
  mutate(instnm = paste0("NC State Avg: ", sector)) %>%
  select(-sector)

grad_natl <- grad %>%
  filter(str_detect(sector, "Public")) %>%
  group_by(sector) %>%
  dplyr::summarize(completion_rate = mean(completion_rate, na.rm = T)) %>%
  mutate(instnm = paste0("National Avg: ", sector)) %>%
  select(-sector)

completion_rates <- bind_rows(guilford = grad_guilford, ncstate = grad_nc, national = grad_natl, .id = "group")




# College Scorecard: Debt to Earnings Ratio -----------------------------------------------------------------------

scorecard <- sc_init() %>%
  sc_filter(control != 3) %>% #removes private for-profit
  sc_year('latest') %>%
  sc_select(unitid, instnm, stabbr, debt_mdn, md_earn_wne_p10) %>%
  sc_get()


d2e <- scorecard %>%
  left_join(hd) %>%
  mutate(debt_to_earnings_ratio = debt_mdn/md_earn_wne_p10)


d2e_guilford <- d2e %>%
  filter(is_guilford) %>%
  select(instnm, debt_to_earnings_ratio) %>%
  filter(instnm != "John Wesley University",
         instnm != "Virginia College-Greensboro") #Removed per CFGG due to closure

d2e_nc <- d2e %>%
  filter(stabbr == "NC") %>%
  filter(str_detect(sector, "Public")) %>%
  group_by(sector) %>%
  dplyr::summarize(debt_to_earnings_ratio = mean(debt_to_earnings_ratio, na.rm = T)) %>%
  mutate(instnm = paste0("NC State Avg: ", sector)) %>%
  select(-sector)

d2e_natl <- d2e %>%
  filter(str_detect(sector, "Public")) %>%
  group_by(sector) %>%
  dplyr::summarize(debt_to_earnings_ratio = mean(debt_to_earnings_ratio, na.rm = T)) %>%
  mutate(instnm = paste0("National Avg: ", sector)) %>%
  select(-sector)

debt_to_earnings <- bind_rows(guilford = d2e_guilford, ncstate = d2e_nc, national = d2e_natl, .id = "group")




# Combine data ----------------------------------------------------------------------------------------------------

higher_ed <- lst(completion_rates, debt_to_earnings, total_students, retention_rates) %>%
  reduce(left_join) %>%
  select(-c(zip, stabbr, countynm, latitude, longitud)) %>% #Left these until here in case someone wants to map later
  filter(instnm != "John Wesley University",
         instnm != "Virginia College-Greensboro") %>% #Removed per CFGG due to closure
  select(instnm, sector, completion_rate, debt_to_earnings_ratio, full_time_retention_rate, total_students)




# Save object for dashboard ---------------------------------------------------------------------------------------

save(higher_ed, file = "./shiny/data/higher_ed.rda")


# delete ------------------------------------------------------------------



