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
options(scipen = 999) # Ensures scientific notation is not used
options(digits = 2) # Two Decimal Places
csv_path <- here("EVrange_value_General.csv")
EV_range_value_General <- read.csv(csv_path)

# Data Wrangling ---------------------------------------------------------------

# Need to change name of EPA range attribute to access
colnames(EV_range_value_General)[5] <- "EPA_range_mi"

# Wangling Data to have case: make and attribute: make and avg epa range
EV_range_value_df <- EV_range_value_General %>%
  mutate(Price = as.numeric(gsub(",", "", Price))) %>%
  select(Price, EPA_range_mi, Make)

# Create a scatter plot
EPA_range_by_make_linear_scatter_plot <- ggplot(EV_range_value_df, aes(x = EPA_range_mi, y = Price)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Scatter Plot of Price vs EPA Range",
    subtitle = paste("Correlation Coefficient: ", round(cor(EV_range_value_df$Price, EV_range_value_df$EPA_range_mi), 2)),
    x = "EPA Range",
    y = "Price"
  ) +
  theme_minimal()

EPA_range_by_make_linear_scatter_plot
