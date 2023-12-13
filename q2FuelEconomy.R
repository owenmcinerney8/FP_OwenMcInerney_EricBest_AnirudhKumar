#---------------------
#-Data Visualizations-
#---------------------
# Please see Q0Notes_and_Ideas.txt for additional data questions and project documentation
# Question 2: What vehicle has the longest excursion time/length on a full-charge? 

# Package and Filepath Configuration ---------------------------
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(here)

csv_path <- here("EVcars.csv")
EVcars <- read.csv(csv_path)

# Including 'here' functionality for portable code
# csv_path <- here("EVcars.csv")
# EV_range_value_General <- read.csv(csv_path)

# Data Wrangling ---------------------------
EVcars_tidy <- EVcars %>%
  filter(Fuel.ID == 41) %>%
  select(Automobile.ID, Fuel, Manufacturer.ID, Manufacturer, Model, 
         Model.Year,Category.ID, Engine.Type, Engine.Size, Transmission.Type,
         Category, Engine.Size, City.Fuel.Economy.Units, City.Fuel.Economy, 
         Highway.Fuel.Economy.Units, Highway.Fuel.Economy) 

# filters to cases where fuel economy is measured in mpge and is a sedan/wagon  
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

# Display Table ---------------------------
EVcars_mpge_CityHighway_FiveNumSum %>%
  kable() %>%
  kableExtra::kable_classic()

esquisser(data = EVcars_mpge, viewer = getOption(x, default = "dialog"))

esquisser