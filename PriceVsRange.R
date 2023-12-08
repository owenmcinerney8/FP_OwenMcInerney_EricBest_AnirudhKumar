library(readr)
EV_range_value_General <- read_csv("~/Downloads/EV_range_value_General.csv")
library(ggplot2)

# Read the data from the CSV file

# View the structure of the data to verify column names and types
str(EV_range_value_General)

# Create a scatter plot
ggplot(EV_range_value_General, aes(x = `EPA Range (mi)`, y = Price, color = Make)) +
  geom_point() +
  labs(title = "Scatter Plot of Price vs EPA Range",
       x = "EPA Range (mi)",
       y = "Price") +
  theme_minimal()