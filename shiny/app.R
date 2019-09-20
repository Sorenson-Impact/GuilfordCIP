library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(billboarder)
library(tidyverse)
library(leaflet)
library(plotly)
library(sf)
library(siverse)
library(janitor)
library(scales)


# Load data ---------------------------------------------------------------


#load from github

load("./data/ages1.rda")
load("./data/race_all.rda")
load("./data/ethnicity_all.rda")
load("./data/sex_all.rda")
load("./data/emp_race.rda")
load("./data/age.rda")
load("./data/med_income.rda")
load("./data/vacant_housing.rda")
load("./data/geo_places.rda")
load("./data/hh_race.rda")
load("./data/life_expectancy.rda")
load("./data/births.rda")
load("./data/weather.rda")
load("./data/higher_ed.rda")
load("./data/transportation.rda")
load("./data/voters.rda")
load("./data/deaths.rda")
load("./data/tourism.rda")
load("./data/tenure_gc.rda")
load("./data/imr.rda")
load("./data/diabetes.rda")
load("./data/food_insecurity.rda")
load("./data/snap.rda")
load("./data/pov_race.rda")
load("./data/schools.rda")
load("./data/parks.rda")
load("./data/food_stores.rda")
load("./data/explore_acsdata.rda")



# Editable text files
live_resources <- read_csv("https://raw.githubusercontent.com/Sorenson-Impact/GuilfordCIP/master/shiny/edit/live_resources.txt")
work_resources <- read_csv("https://raw.githubusercontent.com/Sorenson-Impact/GuilfordCIP/master/shiny/edit/work_resources.txt")
play_resources <- read_csv("https://raw.githubusercontent.com/Sorenson-Impact/GuilfordCIP/master/shiny/edit/play_resources.txt")
learn_resources <- read_csv("https://raw.githubusercontent.com/Sorenson-Impact/GuilfordCIP/master/shiny/edit/learn_resources.txt")
engage_resources <- read_csv("https://raw.githubusercontent.com/Sorenson-Impact/GuilfordCIP/master/shiny/edit/engage_resources.txt")




# Define individual UI elements -------------------------------------------

# Define the sidebar ----

# Define the body ----

body <- mainPanel(width = 12,
  fluidRow(),
  br(),
  fluidRow(
    img(src = './Images/line.png',class = "lineImg")
  ),
  br(),

  tabsetPanel(

        type = "tabs",

        # Demographics Tab ----

        tabPanel(

            title = "DEMOGRAPHICS", icon = icon("binoculars"),
            tags$div(class = "demographics-tab",

                     br(),

                     fluidRow(
                       column(
                         12,
                         img(src = "./Images/demographics.png", class = "sec-bannerImg")
                       )
                     ),
                     br(),

                     fluidRow(
                       column(
                         12,
                         "Like most of North Carolina, Guilford County has undergone significant changes in the last few years, which is a trend that will likely continue. In learning more about Guilford County we begin with general demographics information. This section includes population numbers, age, and race/ethnicity for all of Guilford County as well as for the two major cities; High Point and Greensboro. Click on the ‘Select Location’ buttons below for different views!"
                        )
                     ),
                     br(),
                     fluidRow(
                       column(
                         2,
                         radioButtons("location_rb_d", label = h3("Select Location"),choices = c("Guilford County", "Greensboro City" = "Greensboro city", "High Point City" = "High Point city"))
                       ),
                       column(
                         2,
                         uiOutput("popAge", inline = TRUE),
                         h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Tables B01001 Sex by Age, B01002 Median Age by Sex")
                       ),

                       column(
                         4,
                         h3("Age Distribution", align ="center"),
                         billboarderOutput("age"),
                         h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B01001 Sex by Age", align = "center")

                       ),
                       column(
                         4,
                         h3("Males to Females in Guilford County", align = "center"),
                         billboarderOutput("sex"),
                         h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B01001 Sex by Age", align = "center")

                       )

                     ),
                     br(),
                     br(),
                     br(),
                     fluidRow(),
                     fluidRow(
                       column(2,
                              radioButtons("location_rb_d1", label = h3("Select Location"),choices = c("Guilford County", "Greensboro City" = "Greensboro city", "High Point City" = "High Point city"))
                       ),
                       column(
                         5,
                         h3("Race", align = "center"),
                         billboarderOutput("race"),
                         h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B01001 (A-G Racial Iterations): Sex by Age.", align = "center")
                       ),
                       column(
                         5,
                         h3("Ethnicity", align = "center"),
                         billboarderOutput("ethnicity"),
                         h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B03003: Hispanic or Latino Origin.", align = "center")
                       )


                     ),
                     fluidRow(),
                     br()





                     )
        ),



        # Live Tab -----
        tabPanel(title = dropdown(
          icon = icon("home"),
          label = "LIVE",
          status= "liveMenu",
          align = "left",
          HTML("<a href='#people'><h4> People </h4> </a>"),
          HTML("<a href='#housing'><h4> Housing </h4> </a>"),
          HTML("<a href='#health'><h4> Health </h4> </a>")
        ),

          #title = "LIVE", icon = icon("home"),
                 tags$div(class = "live-tab",
                          br(),
                          fluidRow(column(12,
                                          img(src = "./Images/live.png", class = "sec-bannerImg"))),
                          br(),

                          fluidRow(
                            column(8,
                                   "This section delves deeper into the people of Guilford County. Currently this page focuses on people, housing, and health. In the people section there is information about births, deaths, and life expectancy across the county. The housing section touches on food insecurity as this is often related to where people live. The health section features several select data points about Guilford County."
                                   ),
                            tags$div(column(4, class ="verticalLineLive",
                                   align = "left",
                                   
                                     h3("Community Projects/Resources"),
                                     htmlOutput("live_resources")
                            ))
                            ),
                          br(),
                          br(),
                         
                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("PEOPLE"),id = "people")

                            )
                          )),
                          br(),
                          br(),


                          fluidRow(
                            column(
                              5,
                              img(src = "./Images/life_expectancy.png", class = "leInfo"),
                              h6("Data Source: North Carolina Center for Health Statistics (2017)", align = "center")

                            ),

                            column(
                              7,
                              align = "center",
                              img(src = "./Images/live01.jpg", class = "live01")

                            )
                          ),
                          br(),
                          fluidRow(
                            column(
                              8,
                              offset = 4,
                              h3("Births Over Time", align = "center")
                            )
                          ),

                          fluidRow(
                            column(
                              4,
                              "This graph illustrates births in Guilford County from 2013 through 2018."
                              ),
                            column(
                              8,
                              align= "center",
                              plotlyOutput("birth2"),
                              h6("Data Source: Register of Deeds, Guilford County (2019)", align = "center")

                            )
                          ),

                          fluidRow(
                            column(12,
                                   "The bar chart on the left shows the percentage of poverty by race and ethnicity in the county.Guilford County has garnered attention for its high levels of food insecurity and large numbers of food deserts. It is important to track these issues in order to understand them and in the future to see if efforts are fixing these problems."
                                  
                            )
                          ),
                          fluidRow(
                            column(
                              7,
                              align = "center",
                              h3("Poverty by Race and Ethnicity", align  = "center"),
                              billboarderOutput("poverty"),
                              h6("U.S. Census Bureau (2017). American Community Survey 1-year estimates. Table B17020 (A-G Racial Iterations) Poverty Status by Age.", align  = "center")

                            ),
                            column(
                              5,
                              h3("Food Insecurity", align ="center"),
                              plotlyOutput("food_insecurity"),
                              h6("Source: Piedmont Health Counts", align = "center")

                            )
                          ),

                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                                div(h1("HOUSING"),id = "housing")

                            )
                          )),
                          fluidRow(
                            column(
                              10,
                              offset = 1,
                              align = "center",
                              img(src = "./Images/live02.JPG", class = "live02")
                            )
                          ),
                          br(),

                          fluidRow(
                            column(12,
                                   "Guilford County has garnered attention for its high levels of food insecurity and large numbers of food deserts. It is important to track these issues in order to understand them and in the future to see if efforts are fixing these problems."
                                   )
                          ),
                          br(),
                          br(),



                          fluidRow(),
                          br(),
                          fluidRow(
                            column(8,
                                   offset = 2,
                                   h3("Access to Food Stores", align = "center"),
                                   leafletOutput("food_stores_map"),
                                   h6("Data Source: Google Maps API Pulls (2019)", align = "center"))

                          ),
                         br(),
                         br(),
                         fluidRow(
                           column(
                             12,
                             "The graphics below features information on the race of householders in Guilford County. According to the Census, a householder is defined as, The person, or one of the people, in whose name the home is owned, being bought, or rented. The race of householders in Guilford County can be seen in the circle graphic. The bar chart breaks down owned and rented homes by race."
                             )
                         ),
                         br(),

                         fluidRow(
                           column(
                             6,
                             h3("Race of Householder", align = "center"),
                             billboarderOutput("hh_race"),
                             h6("Data Source: U.S. Census Bureau (2017). American Community Survey 1-year estimates. Table B25006 Race of Householder", align  = "center")
                           ),
                           column(
                             6,
                             h3("Owned vs Rented Homes", align = "center"),
                             billboarderOutput("tenure_race"),
                             h6("Data Source: U.S. Census Bureau (2017). American Community Survey 1-year estimates. Table B25003 (A-G Racial Iterations) Tenure.", align  = "center")
                           )
                         ),



                          fluidRow(

                            column(
                              8,
                              offset = 2,
                              h3("Percentage of Vacant Houses", align = "center"),
                              "Below is an interactive map that can be used to show percentages of vacant houses by Census tract in Guilford County. The lighter colors demonstrate a higher concentration of vacant houses in an area.",
                              fluidRow(br()),
                              leafletOutput("vacant_houses_map", height = 600),
                              h6("Data Source: U.S. Census Bureau (2017).American Community Survey 1-year estimates. Table B25002 Occupancy Status", align = "center")


                            )


                          ),

                         div(

                           fluidRow(class= "subtitle",
                             column(
                               12,
                               align = "center",
                               div(h1("HEALTH"), id = "health")
                             )
                           )
                         ),

                         br(),

                         fluidRow(
                           column(
                             12,
                             "The health section features a few variables about Guilford County. These include; SNAP use, infant mortality, and diabetes. "
                           )
                         ),
                         br(),
                 fluidRow(

                    column(
                      3,
                      h3("Receipt of Food Stamps/SNAP ", align = "center"),
                      billboarderOutput("snap"),
                      h6("Data Source: U.S. Census Bureau (2017).American Community Survey 1-year estimates. Table B22003 Receipt of Food Stamps/SNAP", align = "center")
                    ),


                   column(
                     6,
                          h3("Infant Mortality Rate", align = "center"),
                          billboarderOutput("imr"),
                          h6("Source: Piedmont Health Counts", align = "center")
                          ),
                   column(
                     3,
                     h3("Diabetes", align = "center"),
                     billboarderOutput("diabetes"),
                     h6("Source: Piedmont Health Counts", align = "center")
                   )
                 ),
                 br(),

                 fluidRow(
                   column(
                     6,
                      "Piedmont Health Counts is a local resource that is made up of health indicators brought together by health and human service professionals. Several of their data points are featured here.
                     To delve deeper into health data for this area with Piedmont Health Counts click on the image. "
                   ),
                   column(
                     align = "center",
                     6,
                     a(
                       img(src = "./Images/live03.png", class = "live03"),
                       href = "http://www.piedmonthealthcounts.org/",
                       target = "_blank"
                     )
                   )
                 ),
                 br(),
                 fluidRow(
                   tags$div(class = "missingText",
                     column(
                       12, 
                       "This dashboard is a community project that needs your participation. If you have any suggestions regarding the content of this page or have data that can be added here, please contact [NAME] at [email address]."
                     )
                   )
                 ),
                 
                         # fluidRow(
                         #    column(4,
                         #           align = "center",
                         #           wellPanel(
                         #           h3("What's Missing?"),
                         #           htmlOutput("live_missing"))
                         #    )
                         #  ),
                         br()
                 )),
        # Work Tab ----
        tabPanel(title = "WORK", icon = icon("cogs"),

                 tags$div(class = "work-tab",
                          br(),
                          fluidRow(column(
                            12,
                            img(src = "./Images/work.png", class = "sec-bannerImg")

                          )),
                          br(),

                          fluidRow(
                            column(
                              8,
                              "Guilford County is a working, driven community with diverse industry sectors and an active employee population. While the county was previously known for its furniture and textiles the local economy has continued to move into the future. This page highlights key data indexes to help best identify this aspect of our community."
                              ),
                            tags$div(column(4, class ="verticalLineWork",
                                            align = "left",
                                     h3("Community Projects/Resources"),
                                     htmlOutput("work_resources"))
                            )
                          ),

                          fluidRow(
                            column(10,
                                   offset = 1,
                                   align = "center",
                                   h3("Employment  Percentage (Civilian Labor Force) by Race and Sex", align = "center"),
                                   billboarderOutput("emp_race"),
                                   h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B23002 (A-G Racial Iterations) Sex by Age by Employment Status.", align = "center")
                            )
                          ),

                          fluidRow(
                            column(
                              12,
                              "Guilford Apprenticeship Partners (GAP) is a nationally-recognized, rapidly growing youth apprenticeship program for rising high school seniors and recent graduates that pass rigorous academic and behavioral screening, to further their education while obtaining paid work experience. Upon completion of GAP, apprentices receive their associate’s degree and a journeyman’s certificate, graduate without debt, and have a well-paying job. Through GAP, forward thinking companies in Guilford County can identify their next generation of workforce. Founded five years ago, GAP, through collaborating with local employers, have launched apprenticeship tracks in advanced manufacturing, hvac/field service, aviation, cybersecurity/IT and automotive. "
                              )
                          ),
                          br(),

                          fluidRow(
                            column(
                              10, offset = 1,
                              align = "center",
                              img(src = "./Images/work01.jpg", class ="work01")
                            )
                          ),
                          br(),


                          fluidRow(
                            column(6,
                                   align  ="center",
                                   h3("Median Household Income by Race", align = "center"),
                                   "The data below highlights the median household income by race in Guilford County. The incomes are measured in US dollars as seen on the left.",
                                   fluidRow(br()),
                                   billboarderOutput("med_inc_race"),
                                   h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B19013 (A-G Racial Iterations) Median Household Income.", align = "center")
                                   ),
                            column(5,
                                   h3("Median Household Income by Ethnicity", align = "center"),
                                   "The data below highlights the median household income by ethnicity in Guilford County. The incomes are measured in US dollars as seen on the left.",
                                   fluidRow(br()),
                                    billboarderOutput("med_inc_ethn"),
                                   h6("Data Source: U.S. Census Bureau (2013-2017). American Community Survey 5-year estimates. Table B19013 (H, I Ethnic Iterations) Median Household Income.", align = "center")
                                   )
                          ),

                          br(),
                          br(),

                          fluidRow(
                            column(10, offset = 1,
                                   align = "center",
                                   img(src = "./Images/work02.jpg", class = "work02")
                                   )
                          ),

                          br(),

                          fluidRow(
                            column(12,
                                   "It is also important to consider how people get to their jobs in the area. Like many other metropolitan areas Guilford County is concerned with how people get around. Local buses operate in cities, across the county and even the region. As a whole, the North Carolina train system also keeps people connected. Below is information on how individuals in Guilford County get to work. "
                                   )
                          ),
                          fluidRow(
                            column(6,
                                   h3("Means of Transportation to Work by Race", align = "center"),
                                       billboarderOutput("transportation_race"),
                                   h6("Data Source: U.S. Census Bureau (2017). American Community Survey 1-year estimates. Table B08105 (A-G Racial Iterations) Means of Transportation to Work.", align = "center")
                                   ),
                            column(6,
                                   h3("Means of Transportation to Work by Ethnicity", align = "center"),
                                     billboarderOutput("transportation_ethnicity"),
                                   h6("Data Source: U.S. Census Bureau (2017). American Community Survey 1-year estimates. Table B08105 (H, I Ethnic Iterations) Means of Transportation to Work.", align = "center")


                            )
                          ),
                          br(),
                          fluidRow(
                            tags$div(class = "missingText",
                                     column(
                                       12, 
                                       "This dashboard is a community project that needs your participation. If you have any suggestions regarding the content of this page or have data that can be added here, please contact [NAME] at [email address]."
                                     )
                            )
                          ),
                          
                          # fluidRow(
                          # column(4,
                          #        align = "center",
                          #        wellPanel(
                          #          h3("What's Missing?"),
                          #          htmlOutput("work_missing"))
                          # )),
                          br()
                 )),
        # Play tab ----
        tabPanel(title = dropdown(

          icon = icon("child"),
          label = "PLAY",
          status= "playMenu",
          align = "left",
          HTML("<a href='#parks'><h4> Parks </h4> </a>"),
          HTML("<a href='#arts'><h4> Arts </h4> </a>"),
          HTML("<a href='#sports'><h4> Sports </h4> </a>")

        ),
          #title = "PLAY", icon = icon("child"),
                 tags$div(class = "play-tab",
                          br(),
                          fluidRow(column(12,
                                          align = "center",
                                          img(src = "./Images/play.png", class = "sec-bannerImg")
                                          )),
                          br(),
                          fluidRow(
                            column(
                            8,
                            "Guilford County is an active community with a generally high quality of life! This section features information on parks, arts, and sports. As seen below, Guilford County ranks 3rd among other counties in North Carolina for visitor spending. The graph below shows the change in visitor spending over time. "
                            ),
                            tags$div(column(4, class ="verticalLinePlay",
                                            align = "left",
                                     h3("Community Projects/Resources"),
                                     htmlOutput("play_resources"))
                            )
                          ),
                          br(),
                          fluidRow(


                            column(3,
                                   img(src = "./Images/tourism.png", class = "tourismInfo")
                            ),
                            column(5,
                                   h3("Visitor Spending Over Time", align = "center"),
                                   plotlyOutput("tourism_spending"),
                                   h6("Data Source: The Economic Impact of Travel on North Carolina Counties” study prepared for Visit North Carolina by the U.S. Travel Association", align = "center")
                            ),
                            column(4,
                                   br(),
                                   br(),
                                   br(),
                                   "The American for the Arts Prosperity Study conducted a comprehensive survey of the arts sector’s effect on economic vitality. The arts inject  $168 million of economic impact into Guilford County annually, support nearly 6,000 full-time jobs that generate $112 million in resident household income, and collect nearly $16 million in state and local tax revenue.")



                          ),

                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("PARKS"),id = "parks")

                            )
                          )),
                          br(),
                          fluidRow(
                            column(10,
                                   offset = 1,
                                   align = "center",
                                   img(src = "./Images/play02.jpg", class = "play02"))
                          ),
                          br(),
                          br(),

                          fluidRow(
                            column(
                              12,
                             "In the midst of Guilford County’s two largest cities, Greensboro and High Point, one can easily find greenspace. Guilford County is home to more than 100 public parks. The vast outdoor space allows residents and visitors to interact with nature easily. This area offers trails for hiking and biking, lakes with fishing, boating, and canoeing options, as well as welcoming gardens."
                              )),
                          br(),
                          fluidRow(),

                          fluidRow(
                            column(8,
                                   h3("Parks Map", align = "center"))
                          ),

                          fluidRow(

                            column(8,
                                   offset =2,
                                   leafletOutput("parks_map", height = 600),
                                   h6("Data Source: Google Maps API Pulls (2019)", align = "center")
                            )


                          ),
                          br(),


                          fluidRow(
                            column(10,
                                   offset=1,
                                   h3("Average Weather Over the Last 48 Months", align = "center"),
                                   "The chart below shows both the average temperatures for each month as well as the average precipitation amounts over the last four years. Average temperature can be seen in the line graph and it coincides with the months on the bottom axis as well as the temperature on the left side of the graph. Average precipitation is shown in the bars on the graph and coincides with the number in inches on the right side of the chart.",
                                   plotlyOutput("weather"),
                                  h6("Data Source: State Climate Office of North Carolina, NC CRONOS Database", align = "center")
                                   )
                          ),

                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("ARTS"),id = "arts")

                            )
                          )),
                          br(),
                          fluidRow(

                            column(10,
                                   offset = 1,
                                   align = "center",
                                   img(src = "./Images/play03.png", class = "play03")
                            )


                          ),
                          br(),
                          fluidRow(
                            column(
                              12,
                              "The arts and cultural assets in Guilford County are important to our lives and help define who we are as a community. Whatever their medium - music, poetry, painting, dance, film, theatre - artists are telling a story. In doing so, they help us make sense of our world and communicate with each other in ways that might not be possible otherwise. The arts can bring us together while connecting us to the past, present, and future. Our museums and zoos thrill and enrich through teaching us about our ecology, history, and possibilities for the future. They help us understand the unique contributions members of our community have made in shaping our city, region, state, and nation."
                               )),
                          br(),

                          fluidRow(),

                          fluidRow(),
                          br(),
                          fluidRow(),
                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("SPORTS"),id = "sports")

                            )
                          )),
                          br(),
                          fluidRow(
                            column(
                              12,
                              "While the city of Greensboro boasts the nickname “Tournament Town”, due to its history of hosting national championships, all of Guilford County is a hub for sports enthusiasts. There are several professional sports teams, seven local universities, and many options for adults and youth to get out and play."
                              )),
                          br(),

                          fluidRow(
                            column(10,
                                   offset = 1,
                                   align = "center",
                                   img(src = "./Images/play01.jpg", class = "play01")
                                   )

                          ),
                          br(),
                          fluidRow(
                            tags$div(class = "missingText",
                                     column(
                                       12, 
                                       "This dashboard is a community project that needs your participation. If you have any suggestions regarding the content of this page or have data that can be added here, please contact [NAME] at [email address]."
                                     )
                            )
                          ),
                          

                          # fluidRow(
                          # 
                          # 
                          #   column(4,
                          #          align = "center",
                          #          wellPanel(
                          #            h3("What's Missing?"),
                          #            htmlOutput("play_missing"))
                          #   )
                          # 
                          # ),
                          br()
                 )),
        # Learn Tab ----
        tabPanel(title = "LEARN", icon = icon("graduation-cap"),
                 tags$div(class = "learn-tab",
                          br(),
                          fluidRow(column(12,
                                          align = "center",
                                          img(src = "./Images/learn.png", class = "sec-bannerImg")

                                          )),
                          br(),

                          fluidRow(
                            column(8,
                                   "Guilford County is home to many different educational institutions. This page features information about Guilford County Schools that serves students from kindergarten through 12th grade, and our local colleges and universities. It is also important to mention early childhood education which is a growing focus area for this county. Recent investments plan to improve early childhood education, which will help children, families, and the county as a whole.",
                                   fluidRow(br()),
                                   "This is an interactive map that can be used to view the school data from each of the Guilford County Schools. In order to learn about a school use the map to zoom in and then click that school. A graph will appear below after a school is selected."
                                   ),
                            tags$div(column(4, class ="verticalLineLearn",
                                            align = "left",
                                     h3("Community Projects/Resources"),
                                     htmlOutput("learn_resources"))
                            )),
                          br(),
                          br(),
                          br(),
                          fluidRow(
                            column(10,
                                   offset = 1,
                                   h3("Schools Map", align = "center"),
                                     leafletOutput("schools_map", height = 600),
                                   h6("Data Source: Guilford County Schools", align = "center")
                           )),

                          br(),
                          fluidRow(
                            column(
                              8,
                              offset = 2,
                              h3("School Performance Metrics", align = "center"),
                              div(billboarderOutput("schools_details"), align = "center"),
                              h6("Data Source: Guilford County Schools", align = "center")


                            )
                          ),

                          br(),
                          br(),
                          fluidRow(
                            column(6,
                                   img(src = "./Images/learn01.JPG", class = "learn01")
                                   ),
                            column(6,
                                   "Guilford County has seven colleges/universities, each with unique features and histories. There are five private colleges/universities, two historically black colleges/universities (HBCUs), and one women’s college. The post-secondary education options in Guilford County are numerous. According to the Lumina Foundation, 45.3% of adults 25-64 possess at least an associate’s degree. Several organizations across Guilford County hope to increase this percentage to 60% in order to prepare for economic shifts in the future. "
                                   )
                          ),
                          br(),
                          br(),
                          fluidRow(
                            column(
                              12,
                              "The circle graphic on the left shows the percent of college students that attend each of the local colleges/universities. The bar chart on the right shows the completion rates for each of the colleges/universities. It is important to note that these percentages represent students who graduated within 150% of the expected time to graduate."
                            )
                          ),
                          br(),
                          fluidRow(column(
                            6,
                            h3("Percent of College and University Students by Institution", align = "center"),
                                billboarderOutput("students"),
                            h6(" Data Source: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS) [2017]", align = "center")
                          ),
                          column(
                            6,
                            h3("Completion Rates for Guilford County's Colleges and Universities", align = "center"),
                                billboarderOutput("completion"),
                            h6(" Data Source: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS) [2017]", align = "center")
                          )),
                          br(),
                          br(),
                          br(),
                          fluidRow(),
                          br(),

                          fluidRow(
                            column(10,
                                   offset = 1,
                                   align = "center",
                                   img(src = "./Images/learn02.png", class= "learn02")
                            )
                          ),
                          fluidRow(column(
                            6,
                            h3("Debt to Income Ratio for Guilford County's Colleges and Universities", align = "center"),
                            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Rhoncus aenean vel elit scelerisque mauris pellentesque. Et ligula ullamcorper malesuada proin libero nunc consequat interdum.",
                            fluidRow(br()),
                             billboarderOutput("debt"),
                            h6(" Data Source: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS) [2017]", align = "center")

                          ),
                          column(
                            6,
                            h3("Retention Rates for Guilford County's Colleges and Universities", align = "center"),
                            "This graph shows the percentage of students that continue on from their first year of college to the next year. ",
                            fluidRow(br()),
                            billboarderOutput("retention"),
                            h6(" Data Source: U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS) [2017]", align = "center"))
                          ),
                          br(),
                          fluidRow(
                            tags$div(class = "missingText",
                                     column(
                                       12, 
                                       "This dashboard is a community project that needs your participation. If you have any suggestions regarding the content of this page or have data that can be added here, please contact [NAME] at [email address]."
                                     )
                            )
                          ),
                          
                          # fluidRow(
                          # 
                          # column(4,
                          #        align = "center",
                          #        wellPanel(
                          #          h3("What's Missing?"),
                          #          htmlOutput("learn_missing"))
                          # )
                          # ),
                          br()

                 )),



        #Engage tab ----

        tabPanel(title = dropdown(
          icon = icon("handshake"),
          label = "ENGAGE",
          status= "engageMenu",
          align = "left",
          HTML("<a href='#vote'><h4> Vote </h4> </a>"),
          HTML("<a href='#serve'><h4> Serve </h4> </a>"),
          HTML("<a href='#give'><h4> Give </h4> </a>")

        ),
          #title = "ENGAGE", icon = icon("handshake"),
                 tags$div(class = "engage-tab",
                          br(),
                          fluidRow(column(12,
                                          align = "center",
                                          img(src = "./Images/engage.png", class = "sec-bannerImg")
                                          )),
                          br(),
                          fluidRow(
                            column(
                              8,
                              "Guilford County has always been known for its high amounts of “social capital” – that “glue” that binds us together as people and drives us to be engaged in the community!   There are many ways that people engage with the community and we’ve selected a few key indicators to highlight in this section:  voting, serving, and giving.   We have tremendous resources here locally to help us with these activities as these are linked here. "
                              ),
                            tags$div(column(4, class ="verticalLineEngage",
                                            align = "left",
                                     h3("Resources"),
                                     htmlOutput("engage_resources"))
                            )),
                          br(),

                          # fluidRow(
                          #
                          #   column(
                          #     2,
                          #     offset = 3,
                          #     wellPanel(
                          #       align = "center",
                          #       HTML("<a href='#vote'><h4> Jump to Vote </h4> </a>")
                          #     )
                          #   ),
                          #   column(
                          #     2,
                          #     wellPanel(
                          #       align = "center",
                          #       HTML("<a href='#serve'><h4> Jump to Serve </h4> </a>")
                          #     )
                          #   ),
                          #   column(
                          #     2,
                          #     wellPanel(
                          #       align = "center",
                          #       HTML("<a href='#give'><h4> Jump to Give </h4> </a>")
                          #     )
                          #   )
                          #
                          # ),

                          br(),
                          br(),

                          fluidRow(
                            column(
                              8,
                              offset = 2,
                              img(src = "./Images/engage01.jpg", class ="engage01")
                            )
                          ),
                          br(),

                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("VOTE"),id = "vote")

                            )
                          )),

                          fluidRow(
                            column(12,
                                   "Voting is important because it helps to determine which individuals will hold offices that represent communities, and this may play a role in what legislation is passed. The charts below illustrate party affiliation by race and gender."
                                   )),
                          fluidRow(
                            column(6,
                                   h3("Race and Party Affliation of Registered Voters", align = "center"),
                                     billboarderOutput("voters_rp"),
                                   h6("Data Source: NC State Board of Elections (2019)", align = "center")),
                            column(6,
                                   h3("Gender and Party Affiliation of Registered Voters", align = "center"),
                                     billboarderOutput("voters_gp"),
                                   h6("Data Source: NC State Board of Elections (2019)", align = "center")
                                   )
                          ),
                          br(),
                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("SERVE"),id = "serve")

                            )
                          )),
                          br(),
                          fluidRow(
                            column(
                              12,
                              "The Piedmont Triad embodies the spirit of volunteerism wholeheartedly. The Volunteer Center of Greensboro, the largest Volunteer Center in North Carolina per capita of people volunteering and serving, reported in 2018 alone 405,000 hours of volunteer service provided.  With 1 volunteer hour being valued at $24.69 according to the Independent Sector, volunteering made a huge economic impact of close to $10 million in our community.  Whether your passionate about  arts and culture, education, hunger and homelessness, environmental opportunities or animal welfare and much, much more our community has the something for you to give of your time to."
                              )),
                          br(),

                          tags$div(fluidRow(
                            class = "subtitle",
                            column(
                              12,
                              align = "center",

                              div(h1("GIVE"),id = "give")

                            )
                          )),
                          br(),
                          fluidRow(
                            column(
                              12,
                             "Everyone can be a philanthropist!   Nonprofit organizations rely on our charitable donations to deliver the essential and enriching services they provide.  Individual donors give the majority of philanthropic dollars.  Guilford County is well positioned will a wide array of vehicles to promote charitable giving through our United Ways, Community Foundations, scholarship programs and many faith-based institutions."
                              )),
                          br(),
                          
                          fluidRow(
                            tags$div(class = "missingText",
                                     column(
                                       12, 
                                       "This dashboard is a community project that needs your participation. If you have any suggestions regarding the content of this page or have data that can be added here, please contact [NAME] at [email address]."
                                     )
                            )
                          ),
                          
# 
#                           fluidRow(
# 
#                             column(4,
#                                    align = "center",
#                                    wellPanel(
#                                      h3("What's Missing?"),
#                                      htmlOutput("engage_missing"))
#                             )),

                          br()
                 )

        ),


        # Explore tab ----

        tabPanel(title = "EXPLORE", icon = icon("map-marked-alt"),
                 tags$div(class = "explore-tab",

                 br(),
                 fluidRow(
                   column(12,
                          align = "center",
                          img(src = "./Images/explore.png", class = "sec-bannerImg")
                   )

                 ),
                 br(),
                 fluidRow(
                   column(
                     12,
                     "This page is intended for those of you who wish to explore the data further. The Map below has different  data layers that can be chosen as the base layer and the markers can be added to this to gain understanding and insight into the community."
                     )
                 ),
                 br(),
                 fluidRow(
                   column(
                     10,
                     offset = 1,
                   leafletOutput("explore_map", height = 600))
                 ),
                 br(),
                 br(),
                 fluidRow(
                   column(
                     12,
                     align = "center",
                     h3("EXPLORE DIFFERENCE OVER TIME")

                   )
                 ),

                 br(),
                 fluidRow(
                   column(
                     12,
                     "This data can also be explored over time. The map below shows the difference in the data over time for the selected base layer. "

                   )
                 ),

                 fluidRow(
                   column(7,
                          sliderInput("yr", label = NULL, min = 2013, max = 2017, value = c(2013, 2017), step = 1, sep = ""),
                          leafletOutput("compare", height = 600))
                          ,
                   column(
                     5,
                     h3("Trend Over Time", align = "center"),
                     plotlyOutput("exploreline")

                   )),

                 br(),
                 br()
        )
                 ),
        # About Us Tab ----
        tabPanel(title = "ABOUT US", icon = icon("users"),
                 tags$div(class = "aboutus-tab"),
                 br(),
                 fluidRow(
                   column(12,
                          align = "center",
                          img(src = "./Images/about_us.png", class = "sec-bannerImg")
                   )

                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h3("PURPOSE"),
                          "The purpose of the Guilford Community Indicators Project is to develop an interactive, community-wide platform that provides data on community issues for everyone so that data can help drive decisions.",
                          fluidRow(br()),
                          h3("CONVENOR"),
                          "The Project is convened by the",tags$b( "Greensboro-High Point Collaborative"), "a local chapter of Forward Cities, a national learning network of over 30 cities committed to advancing inclusive innovation and economic development in their communities.  The Collaborative is led by the Community Foundation of Greater Greensboro and Business High Point-Chamber of Commerce" ,
                          "The Community Indicators Project is led by a Steering Committee from across the county with representatives from a wide range of sectors including education, government, business, health, human services, nonprofits, community organizations, and philanthropy.",
                          fluidRow(br()),
                          "Our aspiration is for these indicators to support community efforts in drafting visions of a better future -- helping communities build participation, set priorities, develop action plans, and track progress toward realization of goals. "
                   )

                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h3("WHAT ARE COMMUNITY INDICATORS?"))
                 ),
                 fluidRow(
                   column(
                     12,
                     "Community indicators are ways to measure the well-being of communities.
Community indicator projects collect and compile data in an interactive, community-wide platform that gives everyone access to the information. They measure items that have an impact on issues like quality of life, environmental sustainability and health. Making the same information available to all facilitates a county-wide conversation and advances our community narrative. Perhaps the most common use of indicators is to support community efforts in drafting visions of a better future. They help communities build participation, set priorities, develop action plans, and track progress toward realization of goals."
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h3("VALUES"))
                 ),
                 fluidRow(
                   column(
                     12,
                     "Guilford Community Indicators is based on the values of open data, inclusive and transparent analysis, and the belief that we can improve by looking at accurate information about ourselves as a community.  "
                   )
                 ),

                 br(),
                 fluidRow(
                   column(
                     12,
                     h3(
                       "WHO USES COMMUNITY INDICATORS?"
                     )
                   )
                 ),
                 fluidRow(
                   column(
                     12,
                     "Community indicators can be used by many people. Community members use them to learn more about their neighborhoods.  Business groups use them to assess the market or the long-term prospects for the local workforce. Schools use them to educate students about local history and issues. Advocacy groups refer to them to make their case to the media, the public, foundations, and political figures. The media use them to report on what's happening in the community. Increasingly, philanthropic foundations (as well as corporate and government grantmakers) are using indicators to help them identify priorities for funding and for identifying", tags$i ("high-leverage strategies"), "where a little money in one place will have a lot of positive ripplw effects"
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h3("KEY FACTS ABOUT GUILFORD COMMUNITY INDICATORS"))
                 ),
                 fluidRow(
                   column(
                     12,
                     tags$ul(
                       tags$li("County-wide Initiative"),
                       tags$li("Shaped by a high level of inclusive community involvement, through focus groups, a steering committee, and other public engagement activities"),
                       tags$li("Funded by Community Foundation of Greater Greensboro, Foundation for a Healthy High Point, City of Greensboro, County of Guilford, and others")
                     )
                   )
                 ),
                 br(),
                 fluidRow(
                   column(
                     12,
                     h3("DEVELOPMENT OF GUILFORD COMMUNITY INDICATORS")
                   )
                 ),
                 fluidRow(
                   column(
                     12,
                     "Development of Guilford Community Indicators was led by a Steering Committee from across the county with representatives from a wide range of sectors including education, government, business, health, human services, nonprofits, community organizations, faith institutions, and philanthropy.  The Steering Committee was co-chaired by Leslie Kilgore and Gary Palmer and partnered with Sorenson Impact Center of the University of Utah - a widely recognized think-and-do tank focused on solving social problems using data, evidence, and innovation.  "
                   )
                 ),
                 br(),
                 fluidRow(
                   column(
                     12,
                     h3(
                       "FUTURE OF GUILFORD COMMUNITY INDICATORS"
                     )
                   )
                 ),
                 fluidRow(
                   column(
                     12,
                     "Guilford Community Indicators is now part of the Piedmont Regional Data Collaborative (PRDC)  -- a collaboration between NC A&T University and UNC Greensboro to support the data collaboration needs of the Piedmont region. The PRDC aims to provide the technology platform(s) and expertise that foster data driven research and information to serve the greater community, allowing partners to focus on the data instead of the underlying technology.",
                     tags$br(),
                     tags$br(),
                     "The PRDC is established under the UNCG Institute for Data, Evaluation, and Analytics (IDEA) and is managed jointly by the CIO’s from both NC A&T and UNCG, Tom Jackson and Donna Heath respectively. A stakeholder advisory board called the Community Cabinet has been assembled to oversee projects, provide strategic advice, and serve as community connections of the PRDC.",
                     tags$br(),
                     tags$br(),
                     "Both UNC Greensboro and NC A&T realized the similar roles of both institutions within the regional community and the opportunity to work together to make community and research data findable, accessible, interoperable, and reusable (FAIR). Collaborating provides the community a single place to go and allows both universities to make optimal use of technology infrastructure.",
                     tags$br(),
                     tags$br(),
                     "The PRDC focuses in 5 main areas:",
                     tags$ol(
                       tags$li("Serving as a local data intermediary"),
                       tags$li("Cultivate local capacity for informed action by enhancing the data capacities of other local institutions "),
                       tags$li("Promoting a culture of learning and collaboration among education, government, business, nonprofit, and philanthropic community sectors"),
                       tags$li("Leveraging data to advance both research and the community"),
                       tags$li("Provide a framework within which NC A&T and UNCG can collaborate on data related projects")
                     )
                     )
                 ),
                 br(),
                 fluidRow(
                   column(
                     12,
                     h3("COMMITTEE MEMBERS")
                   )
                 ),
                 fluidRow(
                   column(12,
                          tags$b("Joe Blosser"),"High Point University, ",
                          tags$b("Sadie Blue")," Building Stronger Neighborhoods",
                          tags$b(" Patrick Chapin")," Business High Point Chamber of Commerce",
                          tags$b("Kathy Colville")," Cone Health",
                          tags$b(" Mike Halford")," County of Guilford/Budget, Management & Evaluation",
                          tags$b(" Bryle Hatch")," North Carolina A&T State University",
                          tags$b(" Steve Hayes")," Guilford Nonprofit Consortium",
                          tags$b(" Donna Heath")," UNC Greensboro",
                          tags$b(" Tom Jackson")," North Carolina A&T State University",
                          tags$b(" Jason Jones")," County of Guilford/Budget, Management & Evaluation",
                          tags$b(" Leslie Kilgore")," Thomas Built Buses",
                          tags$b(" Ed Kitchen")," Bryan Foundation",
                          tags$b(" Leonard Lawson")," Ready for School, Ready for Life",
                          tags$b(" Paul Lessard")," High Point Community Foundation",
                          tags$b(" Kevin Lundy")," The Community Foundation of Greater Greensboro",
                          tags$b(" Sharon Maney")," Berkshire Hathaway",
                          tags$b(" Tina Markanda")," The Foundation for a Healthy High Point",
                          tags$b(" David Martin")," Guilford County Schools",
                          tags$b(" Frank McCain")," United Way of Greater Greensboro",
                          tags$b(" Winston McGregor")," Guilford Education Alliance",
                          tags$b(" Michael McNair")," City of High Point",
                          tags$b(" Skip Moore")," TEDx Greensboro",
                          tags$b(" Jane Nickles")," City of Greensboro",
                          tags$b(" Brian Norris")," Business High Point Chamber of Commerce",
                          tags$b(" Gary Palmer")," Replacements/CFGG Board",
                          tags$b(" Judy Penny")," Guilford County Schools",
                          tags$b(" Bob Powell")," North Carolina A&T State University",
                          tags$b(" Pilar Powell")," Keller Williams Realty",
                          tags$b(" Maria Rosales")," Guilford College",
                          tags$b(" Tara Sandercock")," The Community Foundation of Greater Greensboro",
                          tags$b(" Walker Sanders")," The Community Foundation of Greater Greensboro",
                          tags$b(" Terri Shelton")," UNC Greensboro",
                          tags$b(" Mark Smith")," County of Guilford/Public Health",
                          tags$b(" Michelle Sorrells")," The Community Foundation of Greater Greensboro",
                          tags$b(" Jeff Thigpen")," County of Guilford/Register of Deeds",
                          tags$b(" Marcus Thomas")," Mount Zion Baptist Church of Greensboro",
                          tags$b(" Ray Trapp")," North Carolina A&T State University",
                          tags$b(" Tom Tricot")," City of High Point"
                          
                 )),
                 fluidRow(),
                 
                 br(),
                 fluidRow(
                   column(
                     12,
                     align = "center",
                     img(src = "./Images/all_logos.png", class = "all_logosImg")
                   )
                 ),
                 br(),
                 fluidRow(
                   tags$div(class = "missingText",
                            column(
                              12, 
                              align = "right",
                              "This dashboard was developed and designed by the Sorenson Impact Center."
                            )
                   )
                 ),
                 br()
                 
        )


    ))




# Define the UI -----------------------------------------------------------

ui <- fluidPage(theme = "sdashboard.css",
                tags$script(src = "./scripts/tabHelper.js"),
                titlePanel(
                  windowTitle = "Guilford CIP",
                  title = div(
                  img(src = './Images/line.png',class = "lineImg"),
                  img(src = './Images/guilford-logo.png',class= "logoImg"),
                  img(src = "./Images/guilford.jpg", class = "bannerImg")

                )
                ),
                div(body, class= "fullpage"))


# Define the server -------------------------------------------------------
server <- function(input, output) {

  fdicon <- awesomeIcons(icon = 'shopping-cart', iconColor = "#50d1d8", library = 'fa', markerColor = "white")
  scicon <- awesomeIcons(icon = 'graduation-cap', iconColor = "#61aa43", library = 'fa', markerColor = "white")
  parkicon <- awesomeIcons(icon = 'tree', iconColor = "#ffc91d", library = 'fa', markerColor = "white")

  # DEMOGRAPHICS Tab ----

  location_reactive_d <- reactive({
    input$location_rb_d
  })

  location_reactive_d1 <- reactive({
    input$location_rb_d1
  })


  output$popAge <- renderUI({


    if (location_reactive_d () == "Guilford County") {
      img(src = './Images/popAge_guilford.png', width = "100%")
    }
    else if (location_reactive_d () == "Greensboro city") {
      img(src = './Images/popAge_greensboro.png', width = "100%")
    }
    else if (location_reactive_d () == "High Point city") {
      img(src = './Images/popAge_HighPoint.png', width = "100%")
    }

  })




    output$age <- renderBillboarder({

      ages <- ages %>% filter(location ==location_reactive_d())


        billboarder() %>%
            bb_lollipop(data = ages, point_size = 5, point_color = "#61AA43", line_color = "#61AA43") %>%
            bb_axis(x =list(height = 80)) %>%
          bb_y_axis(tick = list(format = suffix("%"))) %>%
          bb_tooltip(
            format = list(
              name = htmlwidgets::JS("function(name, ratio, id, index) {return 'Perc';}")
            ))
        #bb_barchart(data = gc_ages, stacked = T)
    })

    output$sex <- renderBillboarder({

      sex <- sex %>% filter(location ==location_reactive_d())

        billboarder() %>%
            bb_donutchart(data = sex) %>%
            bb_color(palette = c("#61AA43", "#FF5A27"))


    })

    output$race <- renderBillboarder({

      race <- race %>% filter(location ==location_reactive_d1())

        billboarder() %>%
            bb_donutchart(data = race) %>%
            bb_donut() %>%
            bb_color(palette = c("#B42025", "#50D1D8", "#026BC1", "#FFC91D", "#61AA43", "#FF5A27", "#00544E"))

    })

    output$ethnicity <- renderBillboarder({
      ethnicity <- ethnicity %>% filter(location ==location_reactive_d1())

        billboarder() %>%
            bb_donutchart(data = ethnicity) %>%
            bb_color(palette = c("#026BC1", "#FFC91D"))

    })




# LIVE Tab ----




output$deaths <- renderBillboarder({
  #deaths_un <- deaths %>% filter(manner== "Accident"|manner == "Homicide"| manner == "Suicide"| manner == "Unknown")

  billboarder() %>%
    bb_barchart(data = deaths_un) %>%
    bb_bar(padding =2) %>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_color(palette = c("#b42025", "#50d1d8", "#026bc1", "#61aa43")) %>%
    bb_tooltip(format = list(value = htmlwidgets::JS("d3.format(',.2')")))


})



output$birth2 <- renderPlotly({

  plot_ly(data = births, x =~yob, y = ~Females, type = 'scatter', mode = 'lines+markers',
          line= list(color= '#50D1D8', width =2.5),
          marker = list(color= '#50D1D8', width =3),
          name = 'Females') %>%
    add_trace(y=~Males, line = list(color='#00544E'),
              marker = list(color= '#00544E'),
              name = 'Males') %>%
    add_trace(y=~`All Births`, line = list(color='#b42025'),
              marker = list(color= '#b42025'),
              name = 'All Births') %>% 
    layout(yaxis = list(title = "", separatethousands = TRUE),
           xaxis = list(title = "", tickangle = 45, tickfont = list(size = 10)),
           legend = list(orientation = 'h', y = -0.2, x = 0.2))
})



output$hh_race <- renderBillboarder({

  hh_race_bb <- hh_race1 %>%
    select(levlab, estimate)

  billboarder() %>%
    bb_donutchart(data = hh_race_bb) %>%
    bb_color(palette = c("#B42025", "#50D1D8", "#026BC1", "#FFC91D", "#61AA43", "#FF5A27", "#00544E"))

})

output$poverty <- renderBillboarder({
  
  
 race_et <- unique(pov_race$race)
  cols <- rep("#00544e", length(race_et))
  names(cols) <- race_et
  
  cols[["Hispanic or Latino"]] <- "#61aa43"
  
  
  billboarder() %>%
    bb_barchart(data = pov_race) %>%
    bb_legend(show =F) %>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_axis(x =list(height = 80)) %>% 
    bb_bar_color_manual(values = cols)
})



output$tenure_race <- renderBillboarder({
  tenure_bb <- tenure_gc %>%
    mutate(perc = round(perc*100,2)) %>%
    select(race, perc, tenure)

  billboarder() %>%
    bb_barchart(data = tenure_bb,
                mapping = bbaes(race, perc, group= tenure)) %>%
    bb_bar(padding = 2) %>%
    bb_color(palette = c("#B42025", "#50D1D8", "#026BC1", "#FFC91D", "#61AA43", "#FF5A27", "#00544E"))%>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_axis(x =list(height = 80))
})

output$vacant_houses_map <- renderLeaflet({

  #pal <- colorQuantile(palette = "viridis", domain = vacant_housing$estimate,  probs = seq(0, 1, 0.1))
  vacant_housing <- vacant_housing %>%
    mutate(percent = estimate/Total) %>%
    mutate(percent = ifelse(is.nan(percent), 0, percent)) %>%
    st_transform(crs = "+init=epsg:4326")

  pal <- colorNumeric(palette = "viridis",   domain = vacant_housing$percent)


  leaflet(data = vacant_housing) %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    setMaxBounds(-84, 35, -79, 37) %>%
    addPolygons(
      stroke = F,
      fillColor = ~pal(percent),
      fillOpacity = 0.7
    ) %>%
    addPolygons(data = geo_places,
                stroke = T, weight = 1,
                label = ~ NAME,
                color = "white",
                dashArray = "3"
    ) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~ percent,
              labFormat = labelFormat(transform = function(x) 100*x, suffix = "%"),
              title = "Vacant Houses",
              opacity = 1)
})

output$food_stores_map <- renderLeaflet({
  leaflet(data = food_stores) %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    setMaxBounds(-84, 35, -79, 37) %>%
    addAwesomeMarkers(lat = ~lat, lng = ~lon, popup = ~name, icon = fdicon,
               clusterOptions = markerClusterOptions())
})



output$imr <- renderBillboarder({
  billboarder() %>%
    bb_barchart(data = imr) %>%
    bb_legend(show =F) %>%
    bb_color (palette = c("#61aa43")) %>%
    bb_y_axis(tick = list(format = suffix("%")))
})


output$diabetes <- renderBillboarder({
  val <- diabetes %>%
    pull(indicator_value)

  billboarder()%>%
    bb_gauge(fullCircle = TRUE, width = 50 , startingAngle = -100.5) %>% 
    bb_gaugechart(val, steps = c(50,100), steps_color = c("#ffc91d", "#ffc91d")) 


})



output$snap <- renderBillboarder({


  val <- snap %>%
    pull(perc)

  billboarder()%>%
    bb_gaugechart(val, steps = c(50,100), steps_color = c("#b42025", "#b42025")) %>%
    bb_gauge(fullCircle = TRUE, width = 50, startingAngle = -100.5)


})

output$food_insecurity <- renderPlotly({

  plot_ly(
    data = food_insecurity, x = ~period_of_measure, y = ~indicator_value/100, type= 'scatter', mode = 'lines+markers',
    line= list(color= '#E54B21', width =2.5),
    marker = list(color= '#E54B21', width =3)
  )  %>%
    layout(yaxis = list(rangemode = "tozero", title = "Percentage of population <br> that experienced food insecurity", tickformat = "%"),
           xaxis = list(title = ""))

})




output$live_resources <- renderUI({
  vec <- vector()

  for(i in 1:nrow(live_resources)){
    ln <- live_resources[i,1]
    ln1 <- paste(i, ": ", ln)
    vec <- append(vec, ln1)
  }
  HTML(paste(vec, collapse = "<br>"))
})



# WORK Tab ----

output$emp_race <- renderBillboarder({
  billboarder() %>%
    bb_barchart(data = emp_race) %>%
    bb_bar(padding = 2) %>%
    bb_color(palette = c("#50d1d8", "#ffc91d", "#ff5a27", "#00544e")) %>%
    bb_y_axis(tick = list(format = suffix("%")))

})

output$med_inc_race <- renderBillboarder({
  med_income_race <- med_income %>%
    filter(race!="White Alone, Not Hispanic or Latino") %>%
    filter(race!="Hispanic or Latino") %>%
    filter(!is.na(estimate)) %>%
    select(race, estimate) %>%
    arrange(desc(estimate))

  billboarder() %>%
    bb_lollipop(data = med_income_race, point_color = "#b42025", line_color = "#b42025") %>%
    bb_axis(x =list(height = 40))%>%
    bb_y_axis(tick = list(format = htmlwidgets::JS("d3.format('$,')"))) %>%
    bb_tooltip(format = list(
      name = htmlwidgets::JS("function(name, ratio, id, index) {return 'Median Income';}"),
      value = htmlwidgets::JS("d3.format('$,')")))

})

output$med_inc_ethn <- renderBillboarder({

  med_income_ethn <- med_income %>%
    filter(race=="White Alone, Not Hispanic or Latino"| race =="Hispanic or Latino" ) %>%
    select(race, estimate)

  billboarder() %>%
    bb_lollipop(data = med_income_ethn, point_color = "#61aa43", line_color = "#61aa43") %>%
    bb_axis(x =list(height = 20)) %>%
    bb_y_axis(tick = list(format = htmlwidgets::JS("d3.format('$,')")
    ))%>%
    bb_tooltip(format = list(
      name = htmlwidgets::JS("function(name, ratio, id, index) {return 'Median Income';}")))


})


output$transportation_race <- renderBillboarder({

  transp_race <- transp %>%
    filter(level!=1) %>%
    filter(race == "White alone"|race == "Black or African American Alone"|race == "Two or More Races") %>%
    mutate(label = str_remove(label, "Estimate!!Total!!")) %>%
    mutate(perc = round(estimate/Total*100,0)) %>%
    select(race, label, perc) %>%
    spread(race, perc)

  billboarder() %>%
    bb_barchart(data = transp_race, rotated = T) %>%
    bb_bar(padding = 2) %>%
    bb_color(palette = c("#50d1d8", "#ffc91d", "#61aa43"))%>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_x_axis(tick = list(width = 250))

})


output$transportation_ethnicity <- renderBillboarder({

  transp_ethn <- transp %>%
    filter(level!=1) %>%
    filter(race == "White Alone, Not Hispanic or Latino"|race == "Hispanic or Latino") %>%
    mutate(label = str_remove(label, "Estimate!!Total!!")) %>%
    mutate(perc = round(estimate/Total*100,0)) %>%
    select(race, label, perc) %>%
    filter(perc!=0) %>%
    spread(race, perc)

  billboarder() %>%
    bb_barchart(data = transp_ethn, rotated = T) %>%
    bb_bar(padding = 2) %>%
    bb_color(palette = c("#ff5a27", "#00544e")) %>%
    bb_x_axis(tick = list(width = 250)) %>%
    bb_y_axis(tick = list(format = suffix("%")))


})



output$work_resources <- renderUI({
  vec <- vector()

  for(i in 1:nrow(work_resources)){
    ln <- work_resources[i,1]
    ln1 <- paste(i, ": ", ln)
    vec <- append(vec, ln1)
  }
  HTML(paste(vec, collapse = "<br>"))
})




# PLAY Tab ----

output$weather <- renderPlotly({

  plot_ly(data = weather) %>%
    add_trace(x =~month,y=~precipitation, type = 'bar',
              marker = list(color= 'rgb(215,235,205)'),
              name = 'PPtn') %>%
    add_trace(x =~month, y = ~daily_avg, type = 'scatter', mode = 'lines',
              line= list(color= 'rgb(97,170,67)', width =2.5),
              name = 'Average Temperature',
              yaxis = "y2") %>%
    layout(xaxis = list(title = ""),
           yaxis = list(side = 'right', title = "Precipitation (in inches)", showgrid = F),
           yaxis2 = list(side = 'left', overlaying = "y", title = "Temp (in F)", showgrid = F),
           legend = list(orientation = 'h', y = -0.2, x = 0.2))
})


output$parks_map <- renderLeaflet({
  leaflet(data = parks) %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    setMaxBounds(-84, 35, -79, 37) %>%
    addAwesomeMarkers(lat = ~lat, lng = ~lon, popup = ~name, icon = parkicon,
               clusterOptions = markerClusterOptions())

})


output$tourism_spending <- renderBillboarder({
  plot <- tourism %>%
    select(year, expenditures, tax_savings)

  plot_ly(data = plot) %>%
    add_trace(x =~year, y = ~expenditures, type = 'scatter', mode = 'lines+markers',
              line= list(color= '#50d1d8', width =2.5),
              marker = list(color= '#50d1d8', width =3),
              name = 'Expenditures') %>%
    layout(yaxis = list(title = " Expenditure in Millions", separatethousands = TRUE, side = 'left'),
           xaxis = list(title = "", tickangle = 45, tickfont = list(size = 10)),
           legend = list(orientation = 'h', y = -0.2, x = 0.2))
})



output$play_resources <- renderUI({
  vec <- vector()

  for(i in 1:nrow(play_resources)){
    ln <- play_resources[i,1]
    ln1 <- paste(i, ": ", ln)
    vec <- append(vec, ln1)
  }
  HTML(paste(vec, collapse = "<br>"))
})




# LEARN tab ----

output$students <- renderBillboarder({

  plot <- higher_ed %>%
    filter(!is.na(sector)) %>% 
    select(instnm, total_students)


  billboarder() %>%
    bb_donutchart(data = plot) %>%
    bb_color(palette = c("#FFc91D", "#50D1D8", "#026BC1", "#B42025", "#61AA43", "#00544E", "#ff5a27"))


})

output$completion <- renderBillboarder({

  plot <- higher_ed %>%
    select(instnm, completion_rate) %>%
    arrange(desc(completion_rate)) %>% 
    mutate(grp = instnm) %>% 
    select(instnm, completion_rate, grp) 
  
  

  billboarder() %>%
    bb_barchart(data = plot, mapping = bbaes(x = instnm, y = completion_rate*100, group = grp), 
                stacked = T,
                rotated = T)  %>%
    bb_legend(show = FALSE) %>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_x_axis(tick = list(width = 250)) %>%
    bb_colors_manual("Guilford College" ="#026bc1",
                     "Bennett College" = "#026bc1",
                     "Greensboro College" ="#026bc1",
                     "Guilford Technical Community College" = "#026bc1",
                     "High Point University" = "#026bc1",
                     "North Carolina A & T State University" ="#026bc1",
  "University of North Carolina at Greensboro" = "#026bc1",
  "NC State Avg: Public, 4-year or above" ="#f9eed7",
  "NC State Avg: Public, 2-year" = "#f9eed7",
  "National Avg: Public, 4-year or above" = "#f9eed7",
  "National Avg: Public, 2-year" = "#f9eed7")



})

output$retention <- renderBillboarder({
  
  plot <- higher_ed %>% 
    select(instnm, full_time_retention_rate) %>% 
    mutate(dbt_earn_avg = case_when(str_detect(instnm, "year")~ full_time_retention_rate), 
           dbt_earn_coll= case_when(!str_detect(instnm, "year")~ full_time_retention_rate 
           )) %>% 
    arrange(full_time_retention_rate)

  
    billboarder() %>% 
    bb_lollipop(data = plot,
                mapping = bbaes(y = dbt_earn_coll, x = instnm), rotated = F,
                point_color = "#61aa43", line_color = "#61aa43") %>% 
    bb_scatterplot(data = plot, mapping = bbaes(y = dbt_earn_avg, x = instnm), rotated = F) %>% 
    bb_legend(show = FALSE) %>%
    bb_y_axis(tick = list(text = "", format = suffix("%"))) %>%
    bb_axis(x =list(label = "",height = 75), 
            y = list(label = ""), 
            y2 = list(show = FALSE)) 
  


})

output$debt <- renderBillboarder({
  
  plot <- higher_ed %>%
    select(instnm, debt_to_earnings_ratio) %>%
    arrange(desc(debt_to_earnings_ratio)) %>% 
    mutate(grp = instnm) %>% 
    select(instnm, debt_to_earnings_ratio, grp) 

  billboarder() %>%
    bb_barchart(data = plot, mapping = bbaes(x = instnm, y = debt_to_earnings_ratio*100, group = grp), 
                stacked = T,
                rotated = T)  %>%
    bb_legend(show = FALSE) %>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_color(palette = c("#ff5a27"))   %>%
    bb_x_axis(tick = list(width = 250)) %>% 
    bb_colors_manual("Guilford College" ="#ff5a27",
                     "Bennett College" = "#ff5a27",
                     "Greensboro College" ="#ff5a27",
                     "Guilford Technical Community College" = "#ff5a27",
                     "High Point University" = "#ff5a27",
                     "North Carolina A & T State University" ="#ff5a27",
                     "University of North Carolina at Greensboro" = "#ff5a27",
                     "NC State Avg: Public, 4-year or above" ="#f9eed7",
                     "NC State Avg: Public, 2-year" = "#f9eed7",
                     "National Avg: Public, 4-year or above" = "#f9eed7",
                     "National Avg: Public, 2-year" = "#f9eed7")

})

output$schools_map <- renderLeaflet({

  schools %>%
    group_by(school, metric) %>%
    filter(!str_detect(label_metric, "Mean")) %>%
    filter(!is.na(value)) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(popup = paste0(label_metric, ": ", scales::percent(value), " (", label_school_year, ")")) %>%
    select(school, lat, lng, metric, popup) %>%
    spread(key = metric, value = popup) %>%
    unite(col = popup, kdib, grd3_read, act, hsgrad, post_hs_enrolled, sep = "<BR>", na.rm = T) %>%
    mutate(popup = paste0("<B>", school, "</B><BR><BR>", popup)) %>%
    leaflet() %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    addAwesomeMarkers(lat = ~lat,
               lng = ~lng,
               icon = scicon,
               popup = ~popup,
               label = ~school,
               layerId = ~school,
               clusterOptions = markerClusterOptions())

})

filtered_school <- reactive({

  val <- input$schools_map_marker_click$id
  schools %>%
    filter(school == val)

  })



output$schools_details <- renderBillboarder({
  validate(
    need(input$schools_map_marker_click$id!= "", "Please click on a school to view performance metrics"),
    errorClass = "errorText"
  )

  bb_data <- filtered_school()%>%
    arrange(year, metric) %>%
    dplyr::select(label_metric, value, label_school_year)


  billboarder() %>%
    bb_barchart(data = bb_data,
                mapping = bbaes(label_school_year, value*100, group= label_metric)) %>%
    bb_bar(padding = 2) %>%
    bb_y_axis(tick = list(format = suffix("%"))) %>%
    bb_colors_manual("College Attendance" ="#B42025",
                     "Mean College Attendance" = "#e2c3c7",
                     "Grade 11 ACT Proficiency" = "#026BC1",
                     "Mean Grade 11 ACT Proficiency" = "#c3d8e5",
                     "Graduation Rate" = "#FFC91D",
                     "Mean Graduation Rate" = "#f9eed7",
                     "DIBELS At or Above Benchmark" = "#FF5A27",
                     "Mean DIBELS At or Above Benchmark" = "#f9dad4",
                     "End of Grade 3 Reading Proficiency" = "#00544E",
                     "Mean End of Grade 3 Reading Proficiency" = "#c5d3d1")
})



output$learn_resources <- renderUI({
  vec <- vector()

  for(i in 1:nrow(learn_resources)){
    ln <- learn_resources[i,1]
    ln1 <- paste(i, ": ", ln)
    vec <- append(vec, ln1)
  }
  HTML(paste(vec, collapse = "<br>"))
})


# ENGAGE Tab ----



output$resources <- renderUI({

  vec <- vector()

  for(i in 1:nrow(resources)){
    ln <- resources[i,1]
    ln1 <- paste(i, ": ", ln)
    vec <- append(vec, ln1)
  }
  HTML(paste(vec, collapse = "<br>"))

})
output$voters_gp <- renderBillboarder({
  voters_gp <- active_voters %>%
    group_by(gender_code, party_cd) %>%
    summarise(count = n()) %>%
    mutate(denom = sum(count)) %>%
    mutate(perc = round(count/denom*100, 0) ) %>%
    select(gender_code, party_cd, perc) %>%
    filter(perc!=0) %>%
    filter(!is.na(perc), !is.na(gender_code)) %>%
    spread(party_cd, perc)

  billboarder() %>%
    bb_barchart(data = voters_gp) %>%
    bb_bar(padding = 2) %>%
    bb_color(palette = c("#b42025", "#ffc91d", "#00544e", "#50d1d8")) %>%
    bb_y_axis(tick = list(format = suffix("%")))


})



output$voters_rp <- renderBillboarder({
  voters_rp <- active_voters %>%
    group_by(party_cd, race_code) %>%
    summarise(count = n()) %>%
    mutate(denom = sum(count)) %>%
    mutate(perc = round(count/denom*100, 0) ) %>%
    select(party_cd, race_code, perc) %>%
    filter(perc!=0) %>%
    filter(!is.na(perc)) %>%
    spread(race_code, perc)


  billboarder() %>%
    bb_barchart(data = voters_rp, rotated = F) %>%
    bb_bar(padding = 2) %>%
    bb_color(palette = c("#B42025", "#50D1D8", "#026BC1", "#FFC91D", "#61AA43", "#FF5A27", "#00544E"))%>%
    bb_y_axis(tick = list(format = suffix("%")))  %>%
    bb_axis(x =list(height = 50))

})



output$engage_resources <- renderUI({
  vec <- vector()

  for(i in 1:nrow(engage_resources)){
    ln <- engage_resources[i,1]
    ln1 <- paste(i, ": ", ln)
    vec <- append(vec, ln1)
  }
  HTML(paste(vec, collapse = "<br>"))
})




# EXPLORE Tab ----

#Explore Map ----

output$explore_map <- renderLeaflet({
  exploremap <- leaflet()

  layer <- input$radioinput

  walk(explore_acsdata, function(layer) {



    #Build palette
    palette <- colorNumeric(
      palette = "viridis",
      domain = layer$est2017
    )

    group <- layer %>% slice(1) %>% pull(short_title)

    layer <- layer %>% unite(layerid, c("short_title", "geoid"), sep = ":", remove = F)

    as_percent <- layer %>% slice(1) %>% pull(as_percent) #Do we need to format as percent?
    
    if(!as_percent) {
      popup <- layer %>%
        transmute(popup = glue("<B>{concept}</B><BR><BR>
                           {format(est2017, big.mark = ',')}")) %>%
        pull(popup)
    } else {
      popup <- layer %>%
        transmute(popup = glue("<B>{concept}</B><BR><BR>
                           {scales::percent(est2017)}")) %>%
        pull(popup)
    }


    exploremap <<- exploremap %>%
      addPolygons(data = layer,
                  group = group,
                  layerId = ~layerid,
                  stroke = F,
                  fillColor = ~palette(est2017),
                  fillOpacity = 0.7,
                  popup = popup
      )



  })

  exploremap <- exploremap %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    addLayersControl(baseGroups = explore_acsdata %>% map_chr(~.x %>% slice(1) %>% pluck("short_title")) %>% unname(),
                     overlayGroups = c("Schools", "Parks", "Food Stores"),
                     position = "bottomright",
                     options = layersControlOptions(collapsed = F)) %>%
    hideGroup(c("Schools", "Parks", "Food Stores")) %>%
    addLegend(pal = colorNumeric(palette = "viridis", domain = c(0,1)),
              values = c(0, 1),
              position = "bottomleft",
              title = "Scale"
    )

  exploremap <- exploremap %>%
    addAwesomeMarkers(data = food_stores,
                      lat = ~lat, lng = ~lon, popup = ~name,
                      icon = fdicon,
                      clusterOptions = markerClusterOptions(),
                      group = "Food Stores") %>%
    addAwesomeMarkers(data = schools,
                      icon = scicon,
                      lat = ~lat, lng = ~lng, popup = ~school,
                      clusterOptions = markerClusterOptions(),
                      group = "Schools") %>%
    addAwesomeMarkers(data = parks,
                      icon = parkicon,
                      lat = ~lat, lng = ~lon, popup = ~name,
                      clusterOptions = markerClusterOptions(),
                      group = "Parks")

  exploremap
})


# Compare Map----

shiny_selected_year1 <- reactive({
  input$yr[1]
})
shiny_selected_year2 <- reactive({
  input$yr[2]
})


output$compare <- renderLeaflet({
  comparemap <- leaflet()

  walk(explore_acsdata, function(layer) {

    est_column_year1 <- sym(paste0("est", shiny_selected_year1()))
    est_column_year2 <- sym(paste0("est", shiny_selected_year2()))

    layer <- layer %>%
      mutate(estimate = !!est_column_year2 - !!est_column_year1)

    layer <- layer %>% unite(layerid, c("short_title", "geoid"), sep = ":", remove = F)

    #Build palette
    palette <- colorNumeric(
      palette = "RdYlGn",
      domain = layer$estimate
    )

    group <- layer %>% slice(1) %>% pull(short_title)

    layerid <- layer %>% slice(1) %>% pull(short_title)
    
    as_percent <- layer %>% slice(1) %>% pull(as_percent) #Do we need to format as percent?
    
    if(!as_percent) {
      popup <- layer %>%
        transmute(popup = glue("<B>{concept}</B><BR><BR>
                           Change from {shiny_selected_year1()} to {shiny_selected_year2()}: {format(estimate, big.mark = ',')}")) %>%
        pull(popup)
    } else {
      popup <- layer %>%
        transmute(popup = glue("<B>{concept}</B><BR><BR>
                           Change from {shiny_selected_year1()} to {shiny_selected_year2()}: {scales::percent(estimate)}")) %>%
        pull(popup)
    }

    


    comparemap <<- comparemap %>%
      addPolygons(data = layer,
                  layerId = ~layerid,
                  group = group,
                  stroke = F,
                  fillColor = ~palette(estimate),
                  fillOpacity = 0.7,
                  popup = popup
      )



  })

  comparemap <- comparemap %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    addLayersControl(baseGroups = explore_acsdata %>% map_chr(~.x %>% slice(1) %>% pluck("short_title")) %>% unname(),
                     overlayGroups = c("Schools", "Parks", "Food Stores"),
                     position = "bottomright",
                     options = layersControlOptions(collapsed = F)) %>%
    hideGroup(c("Schools", "Parks", "Food Stores")) %>%
    addLegend(pal = colorNumeric(palette = "RdYlGn", domain = c(-1,1)),
              values = c(-1, 1),
              position = "bottomleft",
              title = "Scale"
    )

  comparemap



  comparemap <- comparemap %>%
    addAwesomeMarkers(data = food_stores,
                      lat = ~lat, lng = ~lon, popup = ~name,
                      icon = fdicon,
                      clusterOptions = markerClusterOptions(),
                      group = "Food Stores") %>%
    addAwesomeMarkers(data = schools,
                      icon = scicon,
               lat = ~lat, lng = ~lng, popup = ~school,
               clusterOptions = markerClusterOptions(),
               group = "Schools") %>%
    addAwesomeMarkers(data = parks,
                      icon = parkicon,
               lat = ~lat, lng = ~lon, popup = ~name,
               clusterOptions = markerClusterOptions(),
               group = "Parks")

  comparemap
})

# Line Chart

long_explore_acsdata <- explore_acsdata %>%
  map(function(table) {
    table %>%
      as_tibble() %>%
      select(-geometry) %>%
      gather(key = year, value = estimate, matches("est\\d{4}")) %>%
      mutate(year = parse_number(year))
  }) %>% bind_rows()



explore_filtered <- reactive({
  shapeid <- str_split(input$compare_shape_click$id, ":") %>% unlist()

  long_explore_acsdata %>%
    filter(short_title %in% shapeid,
           geoid %in% shapeid) %>%
    #filter(year>=shiny_selected_year1(),year<=shiny_selected_year2()) %>%
    arrange(year) %>%
    mutate(year = as.character(year)) %>%
    select(year, estimate, short_title)
})



output$exploreline <- renderPlotly({

  validate(
    need(input$compare_shape_click$id!= "", "Please click on a the map to get the trend of that location over time"),
    errorClass = "errorText"
  )


  title = explore_filtered() %>% slice(1) %>%  pull(short_title)

  p <- plot_ly(explore_filtered(), x = ~year, y = ~estimate, type = "scatter", mode = 'lines+markers',
          line= list(color= '#61aa43', width =2.5),
          marker = list(color= '#61aa43', width =3))%>%
    layout(yaxis = list(rangemode = "tozero", title = title))

  p



})



}


# Run the app -------------------------------------------------------------
shinyApp(ui, server)
