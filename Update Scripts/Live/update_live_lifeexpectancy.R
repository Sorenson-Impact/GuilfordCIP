
library(tidyverse)

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")


# Life Expectancy Infographic-------------------------------------------------------------------------

# data from North Carolina Center for Health Statistics (2017)
# Link: https://schs.dph.ncdhhs.gov/data/county.cfm
# Find the Life expectancy dataset, and click on the report for the state and county life expectancies
# Copy the whole report, paste in an excel file and save as a 'Life Expectancy.xlsx' file in the folder save it in the folder Data Sources/Live/Life Expectancy


le <- read_xlsx(path = "./Data Sources/Live/Life Expectancy/Life Expectancy.xlsx", 
                sheet = "Sheet1") %>% clean_names()

le <- le %>% 
  rename(location = 1,  males = 4, females = 6, white = 8, african_american=10) %>% 
  select(location, males, females, white, african_american) %>% 
  filter(!is.na(location), 
         !str_detect(location, "Geographic"), 
         !is.na(males)) %>% 
  filter(str_detect(location, "North Carolina")|
           str_detect(location, "Guilford")) %>% 
  mutate_at(vars(-location), as.numeric)


print(le) #this has the numbers for the infographic



