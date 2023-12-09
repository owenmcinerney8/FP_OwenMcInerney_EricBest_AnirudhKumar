### Load Packages
## Use groundhog to make sure the code runs mostly everywhere
library(groundhog)
groundhog.day="2023-11-20"
pkgs=c('here', 'dplyr', 'dbplyr', 'ggplot2')
groundhog.library(pkgs, groundhog.day)

### Load Data
## Using 'here' for a relative file path
csv_path <- here("IEA_EV_SalesHistoricalCars.csv")
EV_Global_Historical_raw <- read.csv(csv_path)