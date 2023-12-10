### Load Packages --------------------------------------------------------------
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day <- "2023-11-20"
pkgs <- c("readxl", "dplyr", "dbplyr", "ggplot2", "tidyverse", "knitr", "kableExtra")
groundhog.library(pkgs, groundhog.day)

### read in data ---------------------------------------------------------------
gas_prices <- read_excel("bls_gas_prices", range = A11:M11)
electricty_prices <- read_excel("bls_electricty_prices", range = A11:M11)

### Wrangle data to include Model, fuel, veh_class, air pollution score, city mpg, highway mpg, greenhouse gas score