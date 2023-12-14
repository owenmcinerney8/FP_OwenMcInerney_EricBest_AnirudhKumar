#---------------------
#-Data Visualizations-
#---------------------
# Please see Q0Notes_and_Ideas.txt for additional data questions and project documentation
# Question 1: How common are charging stations? 

# Package and Filepath Configuration ---------------------------
# Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"

# 'here' for using a relative filepath
# 'leaflet' will display the map of charging stations
# 'dplyr' is brought in for tidying data
pkgs=c('here', 'leaflet', 'dplyr')
groundhog.library(pkgs, groundhog.day)

# Using 'here' for a relative filepath
csv_path <- here("EVfuelstations.csv")
EVfuelstations <- read.csv(csv_path)

# Data Wrangling ---------------------------
# This dataset includes a column called "Fuel Type Code" that can stand for electric, CNG, etc
# We want to only consider the fuel stations that are ELEC, in Pennsylvania, and for public use
# Private and government charging facilities off limits to the public are not uncommon
# Case Definition: Charging station with relevant attributes: 
# Fuel.Type.Code, State, Groups.With.Access.Code,
# Latitude, Longitude

filtered_stations <- EVfuelstations %>% 
  filter(Fuel.Type.Code == "ELEC") %>%
  filter(State == "PA") %>% 
  filter(Groups.With.Access.Code == 'Public')

# Printing the number of stations in Pennsylvania
num_stations <- nrow(filtered_stations)
print("Number of Public EV Charging Stations in PA:")
print(num_stations)

# The station names and coordinates are easily selected
stationNames <- filtered_stations$Station.Name
latitude <- filtered_stations$Latitude
longitude <- filtered_stations$Longitude

# Create a custom data frame with station coordinates and names
# Leaflet is sensitive to large datasets
station_data <- data.frame(
  Name <- stationNames,
  Latitude <- latitude,
  Longitude <- longitude
)

# Plot data ---------------------------
# Create the leaflet map
# The setView coordinates center the map on PA
EVchargemap <- leaflet(station_data) %>%
  setView(lng = -77.8124, lat = 40.86833, zoom = 6) %>%  
  addTiles() %>%  
  addMarkers(lat = ~Latitude, lng = ~Longitude, popup = ~Name)  

# Display the map
EVchargemap
