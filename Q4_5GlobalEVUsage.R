### Load Packages
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"
pkgs=c('here', 'dplyr', 'dbplyr', 'ggplot2')
groundhog.library(pkgs, groundhog.day)


### Load Data
## Using 'here' for a relative filepath
options(scipen = 999) # Ensures scienfic notation is not used
csv_path <- here("IEA_EV_SalesHistoricalCars.csv")
EV_Global_Historical_raw <- read.csv(csv_path)


### Wrangle Data
## tidy data to cases of groups of cars with attributes: year, region, count
EV_production_region_year <- EV_Global_Historical_raw %>%
  filter(unit == 'Vehicles', mode == 'Cars', region != "World") %>%
  select(region, year, value) %>%
  group_by(region, year) %>%
  summarise(count = sum(value))

## wrangled data to cases of groups of cars with attributes: year, count
EV_production_global_year <- EV_production_region_year %>%
  group_by(year) %>%
  summarise(count = sum(count)) 


### CREATE Bar Chart of Global Electric Car Ownership by Year
global_ev_ownership_by_year_bar_chart <- ggplot(EV_production_global_year, aes(x = year, y = count, fill = 'red')) +
  geom_col() + 
  scale_y_continuous(n.breaks = 8, limits = c(0, max(EV_production_global_year$count))) +
  scale_x_continuous(n.breaks = 11) +
  labs(title = "Global Electirc Car Ownership Since 2010",
       x = "Year",
       y = "Cars") +
  theme_classic() +
  theme(legend.position = "none") # removes legend

global_ev_ownership_by_year_bar_chart # to view bar chart

### Car bar chart of 
# Wrangle Data to get total sum of car ownership by region between 2019 and 2022
EV_usage_region_2019_2022 <- EV_production_region_year %>%
  filter(year >= 2019) %>%
  group_by(region) %>%
  summarise(count = sum(count))

# use y = reorder(region, +count) to order countries by car ownership
regional_ev_ownership_bar_chart <- ggplot(EV_ownership_order_by_country, aes(x = count, y = reorder(region, +count), fill = 'red')) +
  geom_col() + 
  scale_x_continuous(limits = c(0, max(EV_ownership_order_by_country$count)), n.breaks = 9) +
  labs(title = "Electric Car Ownership By Region",
       subtitle = "Total Cars Owned from 2019 to 2022 for each Region",
       x = "Cars",
       y = "Region") +
  theme_classic() +
  theme(legend.position = "none") # removes legend

regional_ev_ownership_bar_chart # to view bar chart

### View Charts
global_ev_ownership_by_year_bar_chart
regional_ev_ownership_bar_chart
