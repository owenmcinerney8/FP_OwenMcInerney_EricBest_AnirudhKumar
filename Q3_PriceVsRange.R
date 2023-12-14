#---------------------
#-Data Visualizations-
#---------------------
## Please see Q0Notes_and_Ideas.txt for additional data questions and project documentation
## Question 3: How do EV manufacturers compare in excursion range and vehicle price?

# Load Packages ----------------------------------------------------------------
library(groundhog)
groundhog.day <- "2023-11-20"
pkgs <- c("here", "dplyr", "readr", "ggplot2", "tidyverse")
groundhog.library(pkgs, groundhog.day)

# Load Data --------------------------------------------------------------------
# Using 'here' for a relative file path
# Ensures scientific notation is not used
options(scipen = 999) 
csv_path <- here("EVrange_value_General.csv")
EV_range_value_General <- read.csv(csv_path)

# Data Wrangling ---------------------------------------------------------------

# Need to change name of EPA range attribute to access
colnames(EV_range_value_General)[5] <- "EPA_range_mi"

# Wangling Data to have case: make and attribute: make and avg epa range
EV_range_value_df <- EV_range_value_General %>%
  mutate(Price = as.numeric(gsub(",", "", Price))) %>%
  select(Price, EPA_range_mi, Make) %>%
  transform(Price = as.numeric(Price)) %>%
  group_by(Make) %>%
  summarise(Price_avg = mean(Price), EPA_range_avg = mean(EPA_range_mi))

# Create a scatter plot
EPA_range_by_make_linear_scatter_plot <- ggplot(EV_range_value_df, aes(x = EPA_range_avg, y = Price_avg)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(
    title = "Scatter Plot of Price by EPA Range",
    x = "EPA Range",
    y = "Price"
  ) +
  theme_minimal()

EPA_range_by_make_linear_scatter_plot
