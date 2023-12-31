#---------------------
#-Data Visualizations-
#---------------------
# Please see Q0Notes_and_Ideas.txt for additional data questions and project documentation
# Question 7: How do EV's and ICE cars compare in terms of miles driven per dollar for fuel/electricity?

# Load Packages --------------------------------------------------------------
# Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day <- "2023-11-20"
pkgs <- c("readxl", "dplyr", "dbplyr", "ggplot2", "tidyverse", "knitr", "kableExtra")
groundhog.library(pkgs, groundhog.day)

# Read Data --------------------------------------------------------------------
gas_prices <- read_excel("bls_gas_prices.xlsx", range = "B11:M11", col_names = FALSE)
electricty_prices <- read_excel("bls_electricity_prices.xlsx", range = "B11:M11", col_names = FALSE)
EV_GAS_data_raw <- read_excel("all_alpha_24.xlsx")

# Wrangle Data -----------------------------------------------------------------
electricty_prices[1:12] <- electricty_prices[1:12] * 33.7 # converts from kWh to kWh equivalent of a gallon
fuel_df <- full_join(gas_prices, electricty_prices)

# Currently have each month as a given column: need to label columns
colnames(fuel_df)[1] <- "jan"
colnames(fuel_df)[2] <- "feb"
colnames(fuel_df)[3] <- "mar"
colnames(fuel_df)[4] <- "apr"
colnames(fuel_df)[5] <- "may"
colnames(fuel_df)[6] <- "jun"
colnames(fuel_df)[7] <- "jul"
colnames(fuel_df)[8] <- "aug"
colnames(fuel_df)[9] <- "sep"
colnames(fuel_df)[10] <- "oct"
colnames(fuel_df)[11] <- "nov"
colnames(fuel_df)[12] <- "dec"

# Wrangle data to include Model, fuel, veh_class, air pollution score, city mpg, highway mpg, greenhouse gas score
# Change names of columns with spaces
colnames(EV_GAS_data_raw)[11] <- "veh_class"
colnames(EV_GAS_data_raw)[12] <- "air_pollution_score"
colnames(EV_GAS_data_raw)[13] <- "city_mpge"
colnames(EV_GAS_data_raw)[14] <- "highway_mpge"
colnames(EV_GAS_data_raw)[15] <- "cmb_mpge"
colnames(EV_GAS_data_raw)[16] <- "greenhouse_gas_score"

# Cases are cars with attributes: Model, fuel, vehicle class, air pollution score, city mpg, highway mpg, greenhouse gas score
EV_GAS_data <- EV_GAS_data_raw %>%
  drop_na(city_mpge) %>%
  filter(Fuel == "Gasoline" | Fuel == "Electricity") %>%
  select(Model, Fuel, veh_class, air_pollution_score, city_mpge, highway_mpge, cmb_mpge, greenhouse_gas_score) %>%
  mutate(
    city_mpge = as.numeric(city_mpge),
    highway_mpge = as.numeric(highway_mpge),
    cmb_mpge = as.numeric(cmb_mpge),
    greenhouse_gas_score = as.numeric(greenhouse_gas_score)
  )


# To find average mpge for gas and electric
mpge_avg_fuel <- EV_GAS_data %>%
  group_by(Fuel) %>%
  summarise(mpge_avg = mean(cmb_mpge))

electricity_price_per_mile <- mpge_avg_fuel %>%
  filter(Fuel == "Electricity") %>%
  select(mpge_avg)

gas_price_per_mile <- mpge_avg_fuel %>%
  filter(Fuel == "Gasoline") %>%
  select(mpge_avg)

# Make sure they are numeric
electricity_avg_mpge <- as.numeric(electricity_price_per_mile)
gas_avg_mpge <- as.numeric(gas_price_per_mile)

# Add fuel type column to data frame
fuel_df$type <- c("gas", "electricity")

# Need to change cases to months
fuel_df <- fuel_df %>%
  pivot_longer(
    cols = c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec),
    names_to = "month",
    values_to = "avg_gallon_equaivalent_price"
  )

# Making actual dates instead of character dates
dates <- append(
  seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-01"), by = "month"),
  seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-01"), by = "month")
)

# Add dates column to fuel_df
fuel_df$date <- dates

# do not need month column anymore, want to mutate from avg price per gallon to miles per dollar
fuel_df <- fuel_df %>%
  select(type, avg_gallon_equaivalent_price, date) %>%
  mutate(miles_per_dollar = case_when(
    type == "gas" ~ gas_avg_mpge / avg_gallon_equaivalent_price,
    type == "electricity" ~ electricity_avg_mpge / avg_gallon_equaivalent_price
  )) %>%
  select(date, type, miles_per_dollar)

# Make Line Graph ------------------------------------------------------------
miles_per_dollar_line_graph <- fuel_df %>%
  ggplot(aes(x = date, y = miles_per_dollar, color = type)) +
  geom_line() +
  scale_y_continuous(n.breaks = 10, limits = c(0, 20)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Miles per Dollar by Month: Electricity Vs Gas",
    subtitle = "Using 2024 vehicles and 2022 monthly fuel prices",
    x = "Month", y = "Miles per Dollar", color = "Fuel Type"
  ) +
  theme_bw()

# View Miles per Dollar Line Graph
miles_per_dollar_line_graph

