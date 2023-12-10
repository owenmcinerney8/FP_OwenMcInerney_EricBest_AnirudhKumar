### Load Packages --------------------------------------------------------------
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day <- "2023-11-20"
pkgs <- c("here", "dplyr", "dbplyr", "ggplot2")
groundhog.library(pkgs, groundhog.day)


### Load Data ------------------------------------------------------------------
## Using 'here' for a relative file path
options(scipen = 999) # Ensures scientific notation is not used
csv_path <- here("IEA_EV_SalesHistoricalCars.csv")
EV_Global_Historical_raw <- read.csv(csv_path)


### Wrangle Data ---------------------------------------------------------------
## wrangle data to include columns for: year, region, count of EVs
EV_ownership_region_year_raw <- EV_Global_Historical_raw %>%
  filter(unit == "Vehicles", region != "World") %>%
  select(region, year, value) %>%
  group_by(region, year) %>%
  summarise(count = sum(value))

## wrangled data to cases of years with attributes: year, count of EVs
EV_ownership_global_year <- EV_ownership_region_year_raw %>%
  group_by(year) %>%
  summarise(count = sum(count))

### CREATE Bar Chart of Global EV Ownership by Year ----------------------------
global_ev_ownership_by_year_bar_chart <- ggplot(EV_ownership_global_year, aes(x = year, y = count, fill = "red")) +
  geom_col() +
  scale_y_continuous(n.breaks = 8, limits = c(0, max(EV_production_global_year$count))) +
  scale_x_continuous(n.breaks = 11) +
  labs(
    title = "Global EV Ownership since 2010",
    x = "Year",
    y = "Count of Electric Vehicles"
  ) +
  theme_classic() +
  theme(legend.position = "none") # removes legend

global_ev_ownership_by_year_bar_chart # to view bar chart

### Create bar chart of EV Ownership by Country -----------------------------------
# Wrangle Data to get cases of regions from 2019-2022 with attributes: region, count
EV_ownership_region_2019_2022 <- EV_ownership_region_year_raw %>%
  filter(year >= 2019) %>%
  group_by(region) %>%
  summarise(count = sum(count))

# use y = reorder(region, +count) to order countries by car ownership
regional_ev_ownership_bar_chart <- ggplot(EV_ownership_region_2019_2022, aes(x = count, y = reorder(region, +count), fill = "red")) +
  geom_col() +
  scale_x_continuous(limits = c(0, max(EV_ownership_order_by_country$count)), n.breaks = 6) +
  labs(
    title = "EV Ownership By Region",
    subtitle = "Total EVs Owned from 2019 to 2022 for each Region",
    x = "Count of Electric Vehicles",
    y = "Region"
  ) +
  theme_classic() +
  theme(legend.position = "none") # removes legend

regional_ev_ownership_bar_chart # to view bar chart

### Q5 Expansion: What percentage of EVs do the top 3 owning EV countries own?

## Wrangle Data to top 3 (China, Europe, EU) vs everyone else
EV_ownership_region_2019_2022_order_count <- EV_ownership_region_2019_2022[order(EV_ownership_region_2019_2022$count, decreasing = TRUE), ]

top_3_count <- (sum(EV_ownership_region_2019_2022_order_count[1:3, 2]) / sum(EV_ownership_region_2019_2022_order_count$count)) * 100
bottom_count <- sum(EV_ownership_region_2019_2022_order_count[4:nrow(EV_ownership_region_2019_2022_order_count), 2]) / sum(EV_ownership_region_2019_2022_order_count$count) * 100

top_3_count
bottom_count

top_vs_bottom_df <- data.frame(Top3 = c("Yes", "No"), percent = c(top_3_count, bottom_count)) # create data frame

## Create Bar Chart of Top 3 vs Bottom 3
top_vs_bottom_percent <- top_vs_bottom_df %>%
  ggplot(aes(x = Top3, y = percent, fill = Top3)) +
  geom_col() +
  geom_text(aes(label = paste(as.character(round(percent, digits = 2)), "%", sep = ""), y = percent + 2), size = 3) +
  labs(
    title = "Percent of World's EVs Owned", subtitle = "Leading 3 Regions Vs The Rest of the World",
    x = "Top 3", y = "Percentage"
  ) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 100)) +
  theme_bw()

### View Charts ----------------------------------------------------------------
global_ev_ownership_by_year_bar_chart
regional_ev_ownership_bar_chart
top_vs_bottom_percent
