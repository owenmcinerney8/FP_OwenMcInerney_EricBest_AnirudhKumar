#---------------------
#-Data Visualizations-
#---------------------
# Please see Q0Notes_and_Ideas.txt for additional data questions and project documentation
# Question 6: How do EV MPG compare to Gas MPG?

# Load Packages --------------------------------------------------------------
# Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day <- "2023-11-20"
pkgs <- c("readxl", "dplyr", "dbplyr", "ggplot2", "tidyverse", "knitr", "kableExtra")
groundhog.library(pkgs, groundhog.day)

# Load Data ---------------------------------------------------------------
EV_GAS_data_raw <- read_excel("all_alpha_24.xlsx")

# Wrangle Data ---------------------------------------------------------------
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

# Make Box Plots / Greenhouse Summary Table ----------------------------------
# City MPG Box Plot
city_mpge_box_plot <- EV_GAS_data %>%
  group_by(Fuel) %>%
  ggplot(aes(x = Fuel, y = city_mpge, group = Fuel, fill = Fuel)) +
  labs(title = "City MPGe EV vs Gas Box Plot", subtitle = "MPGe is Miles per Gallon equivalent", x = "Fuel", y = "MPGe") +
  scale_y_continuous(n.breaks = 10, limits = c(0, max(EV_GAS_data$city_mpge))) +
  geom_boxplot(
    outlier.colour = "black", outlier.shape = 16,
    outlier.size = 2, notch = FALSE
  ) +
  theme_minimal()

# Highway MPG Box Plot
highway_mpge_box_plot <- EV_GAS_data %>%
  group_by(Fuel) %>%
  ggplot(aes(x = Fuel, y = highway_mpge, group = Fuel, fill = Fuel)) +
  labs(title = "Highway MPGe EV vs Gas Box Plot", subtitle = "MPGe is Miles per Gallon equivalent", x = "Fuel", y = "MPGe") +
  scale_y_continuous(n.breaks = 10, limits = c(0, max(EV_GAS_data$highway_mpge))) +
  geom_boxplot(
    outlier.colour = "black", outlier.shape = 16,
    outlier.size = 2, notch = FALSE
  ) +
  theme_minimal()

# Greenhouse Gas Score Summary Table
funx <- list(
  min = ~ min(.x, na.rm = TRUE),
  Q1 = ~ quantile(.x, probs = 0.20, na.rm = TRUE),
  Q2 = ~ quantile(.x, probs = 0.40, na.rm = TRUE),
  median = ~ median(.x, na.rm = TRUE),
  Q3 = ~ quantile(.x, probs = 0.60, na.rm = TRUE),
  Q4 = ~ quantile(.x, probs = 0.80, na.rm = TRUE),
  max = ~ max(.x, na.rm = TRUE),
  mean = ~ mean(.x, na.rm = TRUE),
  std = ~ sd(.x, na.rm = TRUE)
)

green_house_gas_data <- EV_GAS_data %>%
  group_by(Fuel) %>%
  select(Fuel, greenhouse_gas_score) %>%
  summarize(
    across(
      .cols = where(is.numeric),
      .fns = funx
    ),
    count = n()
  )

greenhouse_gas_score_summary_table <- green_house_gas_data %>%
  kable() %>%
  kable_classic() %>%
  add_header_above(c("EV Highway Economy, in MPGe" = 11))%>%
  add_footnote("MPGe: Miles Per Gallon equivalent. This measures an EV's efficiency against gasoline vehicles")

### View Data Visualizations ---------------------------------------------------
city_mpge_box_plot
highway_mpge_box_plot
greenhouse_gas_score_summary_table

