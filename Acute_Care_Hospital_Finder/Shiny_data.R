#
# The purpose of this program is to generate a dataframe of the top rated hospitals per county with lat/longitude data which will be used to create the Shiny application 'app.R'
# IMPORTANT: This program does not need to be run unless new Hospital information has been downloaded from CMS
# This program takes ~30 minutes to execute
#

# Uncomment and run to install the following packages if running this code for the first time
# install.packages("tidyverse")
# install.packages("tinygeocoder")
# install.packages("usmap")

# Uncomment and update the packages to avoid any loading issues
# update.packages("tidyverse")
# update.packages("tinygeocoder")
# update.packages("usmap")
library(tidyverse)
library(tinygeocoder)
library(usmap)

# Read in hospital general information and get state data using the 'us_map' function
# Data is located in the 'data' folder within 'Acute_Care_Hospital_Finder'
hospital_data <- read_csv("data/Hospital_General_Information.csv") 
states <- us_map(region = "states")

# Create a new dataframe, selecting general hospital information and the overall star rating
hospitals <- hospital_data %>%
  dplyr::filter(`Hospital Type` == "Acute Care Hospitals") %>%
  mutate(star_rating = as.numeric(`Hospital overall rating`)) %>%
  rename(County = `County Name`) %>%
  rename(ZIP = `ZIP Code`) %>%
  filter(`Hospital Type` == "Acute Care Hospitals") %>%
  select(c("Facility ID", "Facility Name", "Address", "City", "State", "ZIP", "County", "star_rating")) %>%
  filter(State %in% states$abbr)

# Create a new dataframe, grouping by state and county to get hospitals with the highest star rating per county
hospitals_best <- hospitals %>%
  group_by(State, County) %>%
  filter(star_rating == max(star_rating))

# Use the function 'geocode' to get the lat and long data for each hospital
hospitals_best <- geocode(hospitals_best, street = Address, city = City, county = County, state = State, postalcode = ZIP, method = 'osm')

# Save the dataframes to the 'data' folder - These will be used to build the Shiny app
write_csv(hospitals_best, "Acute_Care_Hospital_Finder/data/locations_temp.csv")
write_csv(hospitals, "Acute_Care_Hospital_Finder/data/hospitals.csv")