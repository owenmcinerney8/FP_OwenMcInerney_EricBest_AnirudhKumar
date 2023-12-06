#2. What vehicle has the longest excursion time/length on a full-charge? 
#   Answer: We will use the study in (3) to analyze this data. Note on electric vehicles:
#   Weather can affect their performance, especially when it's cold.

##2. 

library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

csv_path <- here("EVcars.csv")
EVcars <- read.csv(csv_path)

EVcars_tidy <- EVcars %>%
  filter(Fuel.ID == 41) %>%
  select(Automobile.ID, Fuel, Manufacturer.ID, Manufacturer, Model, Model.Year,Category.ID, Engine.Type, Engine.Size, Transmission.Type, Category, Engine.Size, City.Fuel.Economy.Units, City.Fuel.Economy, Highway.Fuel.Economy.Units, Highway.Fuel.Economy) # select necessary components
  

EVcars_mpge <- EVcars_tidy %>%
  filter(City.Fuel.Economy.Units == 'mpge') # filters to cases where fuel economy is measured in mpge and is a sedan/wagon

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

EVcars_mpge_CityHighway_FiveNumSum %>%
  kable() %>%
  kableExtra::kable_classic()

esquisser(data = EVcars_mpge, viewer = getOption(x, default = "dialog"))

esquisser