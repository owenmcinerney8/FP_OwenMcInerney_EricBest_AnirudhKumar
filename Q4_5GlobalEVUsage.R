### Load Packages
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"

pkgs=c('here', 'dplyr', 'dbplyr', 'ggplot2')
groundhog.library(pkgs, groundhog.day)

### Load Data
## Using 'here' for a relative filepath
options(scipen = 999)

csv_path <- here("IEA_EV_SalesHistoricalCars.csv")
EV_Global_Historical_raw <- read.csv(csv_path)




### Wrangle Data
## wrangled data to cases of groups of cars with attributes: year, region, count
EV_production_region_year <- EV_Global_Historical_raw %>%
  filter(unit == 'Vehicles', mode == 'Cars', region != "World") %>%
  select(region, year, value) %>%
  group_by(region, year) %>%
  summarise(count = sum(value))

## wrangled data to cases of groups of cars with attributes: year, count
EV_production_global_year <- EV_production_region_year %>%
  group_by(year) %>%
  summarise(count = sum(count))


### CREATE HISTOGRAM
ggplot(EV_production_global_year, aes(x = year)) +
  geom_histogram(bins = 13)

ggplot(EV_production_global_year, aes(x = year, y = count, fill = 'red')) +
  geom_col() + 
  labs(title = "Global Electirc Cars in Use Since 2010",
       x = "Year",
       y = "Cars") +
  theme_classic() +
  theme(legend.position = "none")

### Create Summary Table of Top 5 Leading Countries in 
EV_usage_region_2019_2022 <- EV_production_region_year %>%
  filter(year >= 2019) %>%
  group_by(region) %>%
  summarise(count = sum(count))

EV_ownership_order_by_country <- EV_usage_region_2019_2022[order(EV_usage_region_2019_2022$count, decreasing = TRUE),]

ggplot(EV_ownership_order_by_country, aes(x = region, y = count, fill = 'red')) +
  geom_col() + 
  labs(title = "Electric Car Ownership By Country",
       x = "Country",
       y = "Cars") +
  theme_classic() +
  theme(legend.position = "none")
