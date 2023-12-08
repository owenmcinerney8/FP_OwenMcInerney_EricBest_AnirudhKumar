library(here)
library(readr)
library(ggplot2)

# Read the data from the CSV file - traditional way
#EV_range_value_General <- read_csv("~/Downloads/EV_range_value_General.csv")

# Including 'here' functionality for portable code
# 'here' needs the file to be in the current directory, in our case, in the Github repository. It needs to be uploaded
#csv_path <- here("EV_range_value_General.csv")
#EV_range_value_General <- read.csv(csv_path)

# View the structure of the data to verify column names and types
str(EV_range_value_General)

# Create a scatter plot
ggplot(EV_range_value_General, aes(x = `EPA Range (mi)`, y = Price, color = Make)) +
  geom_point() +
  labs(title = "Scatter Plot of Price vs EPA Range",
       x = "EPA Range (mi)",
       y = "Price") +
  theme_minimal()