#---------------------
#-Data Visualizations-
#---------------------
# Please see Q0Notes_and_Ideas.txt for additional data questions and project documentation
# Question 2: What vehicle has the longest excursion time/length on a full-charge? 

# Package and Filepath Configuration ---------------------------
library(groundhog)
groundhog.day="2023-11-20"

pkgs=c('dplyr', 'ggplot2', 'knitr', 'here', 'leaflet', 'kableExtra')
groundhog.library(pkgs, groundhog.day)

csv_path <- here("EVcars.csv")
EVcars <- read.csv(csv_path)

# Data Wrangling ---------------------------
EVcars_tidy <- EVcars %>%
  filter(Fuel.ID == 41) %>%
  select(Automobile.ID, Fuel, Manufacturer.ID, Manufacturer, Model, 
         Model.Year,Category.ID, Engine.Type, Engine.Size, Transmission.Type,
         Category, Engine.Size, City.Fuel.Economy.Units, City.Fuel.Economy, 
         Highway.Fuel.Economy.Units, Highway.Fuel.Economy) 

# Filters to cases where fuel economy is measured in mpge and is a sedan/wagon  
EVcars_mpge <- EVcars_tidy %>%
  filter(City.Fuel.Economy.Units == 'mpge') 

# Table Formatting ---------------------------
funx <- list(
  min = ~min(.x, na.rm = TRUE),
  Q1 = ~quantile(.x, probs = 0.20, na.rm = TRUE),
  Q2 = ~quantile(.x, probs = 0.40, na.rm = TRUE),
  median = ~median(.x, na.rm = TRUE),
  Q3 = ~quantile(.x, probs = 0.60, na.rm = TRUE),
  Q4 = ~quantile(.x, probs = 0.80, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE),
  mean = ~mean(.x, na.rm = TRUE),
  std = ~sd(.x, na.rm = TRUE)
)

EVcars_mpge_CityHighway_FiveNumSum <- EVcars_mpge %>%
  group_by(Category) %>%
  select(Category, City.Fuel.Economy, Highway.Fuel.Economy) %>%
  summarize(
    across(
      .cols=where(is.numeric),
      .fns = funx),
    count =n()
  )
EVcars_mpge_CityHighway_FiveNumSum <- setNames(EVcars_mpge_CityHighway_FiveNumSum, 
                                               c("Category", "Minimum", "1st Quartile", "2nd Quartile",
                                                 "Median", "3rd Quartile", "4th Quartile",
                                                 "Max", "Mean", "Standard Deviation", "# of Vehicles"))
# Display Table ---------------------------
EVcars_mpge_CityHighway_FiveNumSum %>%
  kable() %>%
  kableExtra::kable_classic() %>%
  add_header_above(c("EV Highway Economy, in MPGe" = 11))%>%
  add_footnote("MPGe: Miles Per Gallon equivalent. This measures an EV's efficiency against gasoline vehicles")

  