#Raw R Code for our data exploration questions:

#----------------------------
#-Data Exploration Questions-
#----------------------------
#1. How common are charging stations? 
#   Answer: We will use the study in (4) to create a map of charging stations and then analyze the data

#2. What vehicle has the longest excursion time/length on a full-charge? 
#   Answer: We will use the study in (3) to analyze this data. Note on electric vehicles:
#   Weather can affect their performance, especially when it's cold.

#3. Which vehicles are the most cost-efficient when compared with traditional ICE cars? 
#   Answer: We must find a dataset which discusses these economic factors.


#---------------------
#-Data Visualizations-
#---------------------
##1. How common are charging stations? 
#    

## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"

## We need 'here' for using a relative filepath
## 'leaflet' will display the map of charging stations
## 'dplyr' is brought in for tidying data
pkgs=c('here', 'leaflet', 'dplyr', 'geosphere')
groundhog.library(pkgs, groundhog.day)

## Using 'here' for a relative filepath
csv_path <- here("EVfuelstations.csv")
EVfuelstations <- read.csv(csv_path)

## Data Tidying
## This dataset includes a column called "Fuel Type Code" that can stand for electric, CNG, etc
## We want to only consider the fuel stations that are ELEC
## Then we only want to look at charging stations in Pennsylvania
filtered_stations <- EVfuelstations %>% filter(Fuel.Type.Code == "ELEC") %>%
  filter(State == "PA")

num_stations = nrow(filtered_stations)

print("Number of EV Charging Stations in PA:")
print(num_stations)

## The station names and coordinates are easily selected
stationNames <- filtered_stations$Station.Name
latitude <- filtered_stations$Latitude
longitude <- filtered_stations$Longitude

## Create a custom data frame with station coordinates and names
## Leaflet is sensitive to large datasets
station_data <- data.frame(
  Name = stationNames,
  Latitude = latitude,
  Longitude = longitude
)

# Create the leaflet map
EVchargemap <- leaflet(station_data) %>%
  setView(lng = -77.8124, lat = 40.86833, zoom = 6) %>%  # Center the map around State College, PA
  addTiles() %>%  # Add map tiles as the base layer
  addMarkers(lat = ~Latitude, lng = ~Longitude, popup = ~Name)  

# Display the map
EVchargemap
