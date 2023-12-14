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
csv_path <- here("EV_range_value_General.csv")
EV_range_value_General <- read.csv(csv_path)

# Data Wrangling ---------------------------------------------------------------

# Need to change name of EPA range attribute to access
colnames(EV_range_value_General)[5] <- "EPA_range_mi"

# Wangling Data to have case: make and attribute: make and avg epa range
EV_range_value_df <- EV_range_value_General %>%
  select(EPA_range_mi, Make) %>%
  group_by(Make) %>%
  summarise(EPA_range_avg = mean(EPA_range_mi)) %>%
  head()

# Create a bar chart
EPA_range_by_make_bar_chart <- ggplot(EV_range_value_df, aes(x = reorder(Make, +EPA_range_avg), y = EPA_range_avg, fill = "red")) +
  geom_col() +
  labs(
    title = "Bar Chart of Average EPA Range by Make",
    x = "Make",
    y = "AVG EPA Range (mi)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # removes legend

EPA_range_by_make_bar_chart
