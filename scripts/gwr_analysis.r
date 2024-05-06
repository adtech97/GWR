library(spatialEco)
library(spgwr)
library(ggplot2)
library(leaflet)
library(readr)
library(dplyr)
library(maps)
library(mapdata)

cleaned_data <- read_csv("C:/Users/Lenovo/OneDrive/Desktop/GWR-main/data/processed_data.csv")
india_map <- map_data("world", region = "India")

# Convert 'Deceased' column to numeric
cleaned_data$Deceased <- as.numeric(cleaned_data$Deceased)

# Step - 1: Look at the spatial distribution of the data

ggplot() +
  geom_polygon(data = india_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = cleaned_data, aes(x = longitude, y = latitude, color = Deceased), size = 3) +
  scale_color_gradient(low = "green", high = "red", name = "Number of Deaths") +  # Adjust color scale and add legend
  coord_fixed(ratio = 1.2) +  # Adjust ratio for better map representation
  labs(title = "Spatial Distribution of Deaths in India During Pandemic", 
       x = "Longitude", y = "Latitude", caption = "Data Source: GWR-main") +  # Add labels and caption
  theme_minimal()  # Use a minimal theme for cleaner look

# Convert 'Month' to a Date object
cleaned_data$Month <- as.Date(paste0(cleaned_data$Month, "-01"), format = "%Y-%m-%d")

# Aggregate the data by month
monthly_deaths <- cleaned_data %>%
  group_by(Month) %>%
  summarise(Total_Deaths = sum(Deceased, na.rm = TRUE))

# Plot the line graph
ggplot(monthly_deaths, aes(x = Month, y = Total_Deaths)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Monthly Deaths During Pandemic",
       x = "Month",
       y = "Total Deaths")

