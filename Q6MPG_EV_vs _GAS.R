### Load Packages
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"
pkgs=c('readxl', 'dplyr', 'dbplyr', 'ggplot2')
groundhog.library(pkgs, groundhog.day)

### read in data
EV_GAS_data_raw <- read_excel("all_alpha_24.xlsx")

### Wrangle data to include Model, fuel, veh_class, air pollution score, city mpg, highway mpg, greenhouse gas score

# Change names of columns with spaces
colnames(EV_GAS_data_raw)[11] = "veh_class"
colnames(EV_GAS_data_raw)[12] = "air_pollution_score"
colnames(EV_GAS_data_raw)[13] = "city_mpg"
colnames(EV_GAS_data_raw)[14] = "highway_mpg"
colnames(EV_GAS_data_raw)[16] = "greenhouse_gas_score"

# Select to Model, fuel, veh_class, air pollution score, city mpg, highway mpg, greenhouse gas score
EV_GAS_data <- EV_GAS_data_raw %>%
  filter(Fuel == "Gasoline" | Fuel == "Electricity") %>%
  select(Model, Fuel, veh_class, air_pollution_score, city_mpg, highway_mpg, greenhouse_gas_score)

### Make Box Plot
library(esquisse)
esquisser(data = EV_GAS_data, viewer = getOption(x = "esquisse.viewer", default = "dialog"))
esquisser()

ggplot(EV_GAS_data) +
  aes(Fuel, city_mpg) +
  geom_boxplot(fill = "red") +
  theme_minimal()
