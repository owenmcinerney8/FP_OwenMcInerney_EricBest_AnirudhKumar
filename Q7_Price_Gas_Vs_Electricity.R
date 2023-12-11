### Load Packages --------------------------------------------------------------
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day <- "2023-11-20"
pkgs <- c("readxl", "dplyr", "dbplyr", "ggplot2", "tidyverse", "knitr", "kableExtra")
groundhog.library(pkgs, groundhog.day)

### read in data ---------------------------------------------------------------
gas_prices <- read_excel("bls_gas_prices.xlsx", range = "B11:M11", col_names = FALSE)
electricty_prices <- read_excel("bls_electricity_prices.xlsx", range = "B11:M11", col_names = FALSE)
EV_GAS_data_raw <- read_excel("all_alpha_24.xlsx")

### Wrangle Data
# Change names of columns with spaces
colnames(EV_GAS_data_raw)[11] <- "veh_class"
colnames(EV_GAS_data_raw)[12] <- "air_pollution_score"
colnames(EV_GAS_data_raw)[13] <- "city_mpge"
colnames(EV_GAS_data_raw)[14] <- "highway_mpge"
colnames(EV_GAS_data_raw)[15] <- "cmb_mpge"
colnames(EV_GAS_data_raw)[16] <- "greenhouse_gas_score"

# Select to Model, fuel, veh_class, air pollution score, city mpg, highway mpg, greenhouse gas score
EV_GAS_data <- EV_GAS_data_raw %>%
  drop_na(cmb_mpge) %>%
  filter(Fuel == "Gasoline" | Fuel == "Electricity") %>%
  select(Model, Fuel, veh_class, air_pollution_score, city_mpge, highway_mpge, cmb_mpge, greenhouse_gas_score) %>%
  mutate(
    city_mpge = as.numeric(city_mpge),
    highway_mpge = as.numeric(highway_mpge),
    cmb_mpge = as.numeric(cmb_mpge),
    greenhouse_gas_score = as.numeric(greenhouse_gas_score)
  )

# case: fuel with attributes each month, type of fuel, average mpge of a 2024 car
electricty_prices[1:12] = electricty_prices[1:12] * 33.7 # converts from kWh to kWh equivalent of a gallon
fuel_df = full_join(gas_prices, electricty_prices)

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

## To find average mpge 
mpge_avg_fuel <- EV_GAS_data %>%
  group_by(Fuel) %>%
  summarise(mpge_avg = mean(cmb_mpge))

electricity_price_per_mile <- mpge_avg_fuel %>%
  filter(Fuel == "Electricity") %>%
  select(mpge_avg)

gas_price_per_mile <- mpge_avg_fuel %>%
  filter(Fuel == "Gasoline") %>%
  select(mpge_avg)

electricity_avg_mpge <- as.numeric(electricity_price_per_mile)
gas_avg_mpge <- as.numeric(gas_price_per_mile)
mpge_avg_fuel

electricity_price_per_mile

fuel_df$type = c("gas", "electricity")

fuel_df <- fuel_df %>%
  pivot_longer(cols = c(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec), 
               names_to = "month", 
               values_to = "avg_gallon_equaivalent_price") %>%
  pivot_wider(id_cols = month, names_from = type, values_from = avg_gallon_equaivalent_price)

dates <- append(seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-01"), by = 'month'), seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-01"), by = 'month'))

fuel_df$date = dates

fuel_df_final <- fuel_df %>%
  reframe(date = date, miles_per_dollar_gas = gas_avg_mpge/gas, miles_per_dollar_electricity = electricity_avg_mpge/electricity)

fuel_df_final <- fuel_df_final %>%
  pivot_longer(cols = c(miles_per_dollar_electricity, miles_per_dollar_gas), names_to = 'fuel_type', names_prefix = 'miles_per_dollar_', values_to = 'miles_per_dollar') 

typeof(fuel_df_final$date)

View(fuel_df_final)

### Plot data
fuel_plot <- fuel_df_final %>% 
  ggplot(aes(x = date, y = miles_per_dollar, color = fuel_type)) +
  geom_line() +
  scale_y_continuous(n.breaks = 10, limits = c(0, 20)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  labs(title = "Miles per Dollar by Month: Electricity Vs Gas", subtitle = "Using 2024 vehicles and 2022 monthly fuel prices", x = "Month", y = "Miles per Dollar", color = "Fuel Type") +
  theme_bw()

fuel_plot

