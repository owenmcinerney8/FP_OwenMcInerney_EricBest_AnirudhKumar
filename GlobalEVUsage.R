## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"

pkgs=c('here', 'dplyr', 'dbplyr', 'ggplot2')
groundhog.library(pkgs, groundhog.day)

## Using 'here' for a relative filepath
csv_path <- here("IEA_EV_SalesHistoricalCars.csv")
EV_Global_Historical_raw <- read.csv(csv_path)

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

## NEXT STEP :: CREATE HISTOGRAM