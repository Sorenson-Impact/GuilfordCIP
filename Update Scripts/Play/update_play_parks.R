require(tidyverse)
require(googleway)
options(warnPartialMatchArgs = F)


# Instructions ----------------------------------------------------------------------------------------------------

# This script pulls data from Google Places API (Maps) matching "Parks".  Because some places have bad categories, the result should be examined by looking suspicious items up on google maps and manually removing ones that are bad (examples of this are plentiful toward the end).

# Options and Settings --------------------------------------------------------------------------------------------

# Set working directory to the root of the local repo location.  This will likely vary for each computer these scripts are run on:
setwd("~/Github/Guilford-CIP")

# You will need to set the google api key using the following code in the console.   DO NOT SAVE THE KEY in this file, as it will be visible to anyone on github!  Contact Tara if you do not have the key.

set_key("your key here") #Run in console only!

# Be sure that you have enabled the "Places API" in your developer console.



# Data Pull -------------------------------------------------------------------------------------------------------

#Google places only allows 20 results at a time (a "page") and then only up to 3 total pages (60 results total).  In order to make sure I'm not missing anything, I've generated a grid of locations around Guilford county, and then done a search around a radius of each point, getting as many pages as possible.  The reuslting list is expected to be full of duplicates.

guilford <- c(36.086046, -79.796661)

place_types <- c("park")


bottomleft <- c(35.861792, -80.112286)
topright <-  c(36.239347, -79.583953)
bottomright <- c(35.910726, -79.580493)
topleft <- c(36.243619, -80.023036)

lat <- topleft[1] - seq(0, topleft[1] - bottomleft[1], length.out = 5)
lon <- bottomleft[2] - seq(0,  bottomleft[2] - bottomright[2], length.out = 5)

location_grid <- tibble(lat, lon) %>%
  expand(lat, lon)

reslst <- lst()
iter <- 0

for(h in 1:nrow(location_grid)) {
  location <- location_grid %>% slice(h) %>% as_vector()
  message(h)

  for(j in seq(place_types)) {
    last_token <- NULL

    for(i in 1:3) {

      res <- google_places(location = location,
                           radius = 20000,
                           place_type = place_types[j],
                           page_token = last_token)

      results <- res$results

      last_token <- res$next_page_token

      iter <- iter + 1

      reslst[[iter]] <- results %>% select(id, name, place_id, rating, user_ratings_total, types) %>% as_tibble() %>% add_column(iter = iter, place_type_search = place_types[j], location_grid = paste(location[1], location[2], sep = ", ")) %>% bind_cols(lat = results$geometry$location$lat, lon = results$geometry$location$lng)

      if(is.null(last_token)) break

      Sys.sleep(time = 3)

    }
  }
}


parks <- bind_rows(reslst)

parks <- parks %>%
  distinct(id, name, place_id, rating, user_ratings_total, lat, lon)

parks <- parks %>% #Pull additional data to get zip code and other stuff
  mutate(place_details = map(place_id, ~google_place_details(.x)))

parks <- parks %>% #unnest the zip and name
  mutate(zip = map_dbl(place_details, ~.x %>% pluck("result") %>% pluck("address_components") %>% filter(types == "postal_code") %>% pull(short_name) %>% as.numeric))

# Zip codes in Guilford county: https://www.bestplaces.net/find/zip.aspx?st=nc&county=37081
guilfordzips <- c(27263, 27214, 27233, 27235, 27249, 27401, 27403, 27405, 27406, 27407, 27408, 27409, 27410, 27455, 27265, 27282, 27260, 27262, 27283, 27301, 27310, 27313, 27357, 27358, 27377)

parks <- parks %>% filter(zip %in% guilfordzips | is.na(zip)) #keep only those that are in Guilford County


parks <- read_rds("~/Google Drive/SI/DataScience/data/Guilford County CIP/Parks/parks_temp.rds")

# Manually filter incorrect results -------------------------------------------------------------------------------

parks %>% arrange(name) %>% View()

parks <- parks %>%

  filter(name != "Archdale Park and Recreation Maintenance Dept.") %>%
  filter(name != "B and B Lenten Roses") %>%
  filter(name != "Bur-Mil Clubhouse") %>%
  filter(name != "Bur-Mil Park Pier") %>%
  filter(name != "Camp Gray Rock") %>%
  filter(name != "Carolina Marina") %>%
  filter(name != "Carolina Air Canine LLC") %>%
  filter(name != "Greensboro KOA Journey") %>%
  filter(name != "Black Diamond Backyard") %>%
  filter(name != "Brentwood soccer fields") %>%
  filter(name != "Brooks Bridge") %>%
  filter(name != "Cove Creek Gardens Inc") %>%
  filter(name != "Creekside Park Disc Golf Course") %>%
  filter(name != "Crockett Trail") %>%
  filter(name != "Deep River Recreation Center") %>%
  filter(name != "Downtown Greenway Morehead Park Trailhead Parking") %>%
  filter(name != "Edible Schoolyard") %>%
  filter(name != "Ellen Ashley") %>%
  filter(name != "Gibsonville Community Garden") %>%
  filter(name != "Gillespie Driving Range") %>%
  filter(name != "Government Plaza") %>%
  filter(name != "Greensboro Beautiful") %>%
  filter(name != "Greensboro Botanical Gardens") %>%
  filter(name != "Greensboro Children's Museum") %>%
  filter(name != "Greensboro Parks & Recreation") %>%
  filter(name != "Greensboro Radio Aero Modelers (GRAMS)") %>%
  filter(name != "Haw River State Park, Iron Ore Belt Access") %>%
  filter(name != "Hester Park Facility") %>%
  filter(name != "High Point Parks & Recreation") %>%
  filter(name != "Hohn Dairy Farm (historical)") %>%
  filter(name != "Hunting at horse farm") %>%
  filter(name != "Ideal Landscaping & Irrigation") %>%
  filter(name != "Iron Ore Belt Access") %>%
  filter(name != "J Razz and Tazz Farm") %>%
  filter(name != "Jamestown Park Golf Club") %>%
  filter(name != "Knight Brown Nature Preserve") %>%
  filter(name != "Lake Brandt Marina") %>%
  filter(name != "Laurel Bluff Trail Head - Lake Brandt") %>%
  filter(name != "Little Loop Trail") %>%
  filter(name != "Lynwood Lakes Lakeside Picnic Area (Members Only,not open to the public))") %>%
  filter(name != "Nat Greene Trail, Atlantic & Yadkin Greenway") %>%
  filter(name != "Nathaniel Greene Trail Head @ Lake Brandt Marina") %>%
  filter(name != "Nathaniel Greene Trail Head Lake Brandt Marina") %>%
  filter(name != "National Park Services") %>%
  filter(name != "Northeast Community Trail") %>%
  filter(name != "Northwood Animal Hospital") %>%
  filter(name != "Oak Hollow Campground") %>%
  filter(name != "Oak Hollow Festival Park") %>%
  filter(name != "Oak Hollow Marina") %>%
  filter(name != "Observation Deck") %>%
  filter(name != "Overlook") %>%
  filter(name != "Owl's Roost Trail") %>%
  filter(name != "Owl's Roost Trail, Atlantic & Yadkin Greenway") %>%
  filter(name != "Palmetto Trail, Atlantic & Yadkin Greenway") %>%
  filter(name != "Park & Ride") %>%
  filter(name != "Park Centre Management") %>%
  filter(name != "Pegram Lake") %>%
  filter(name != "Piedmont Environmental Center") %>%
  filter(name != "Piedmont Trail at Atlantic & Yadkin Greenway") %>%
  filter(name != "Piedmont Trail Parking") %>%
  filter(name != "Reedy Fork Trail Head at Lake Brandt") %>%
  filter(name != "Reedy Fork Trail Parking") %>%
  filter(name != "Rich Fork Preserve") %>%
  filter(name != "Richardson Taylor Preserve") %>%
  filter(name != "Richardson-Taylor Preserve") %>%
  filter(name != "Rock Creek Center") %>%
  filter(name != "Saferight Preserve") %>%
  filter(name != "Squirrel-Off / SDI, Inc.") %>%
  filter(name != "Summerfield Town") %>%
  filter(name != "Summit Conference Center") %>%
  filter(name != "Timberlake Earth Sanctuary") %>%
  filter(name != "Terrace Apartments") %>%
  filter(name != "Townsend Trailhead Parking") %>%
  filter(name != "UNCG Gardens") %>%
  filter(name != "University Park") %>%
  filter(name != "Washington Terrace Community Center") %>%
  filter(name != "Watershed Trail Parking") %>%
  filter(name != "Westminster Gardens Cemetery and Crematory") %>%
  filter(name != "Wild Turkey Mountain Bike Trail") %>%
  filter(name != "Wild Turkey Trail, Atlantic & Yadkin Greenway") %>%
  filter(name != "Weaver Bridge, Atlantic & Yadkin Greenway") %>%
  filter(name != "West House Trail")


save(parks, file = "./shiny/data/parks.rda")

