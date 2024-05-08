library(ggplot2)
library(leaflet)
library(readr)
library(dplyr)
library(maps)
library(mapdata)
library(sf)
library(sp)
library(GWmodel)
cleaned_data <- read_csv("data/processed_data.csv")

# read map shape file
indian_state_map = read_sf("assets/gadm36_IND_1.shp")

# Convert 'Deceased' column to numeric
cleaned_data$Deceased <- as.numeric(cleaned_data$Deceased)

# Aggregating deceased counts by state
deceased_summary <- aggregate(Deceased ~ State, data = cleaned_data, sum)



# Filter out "India" region from india_map


indian_state_map$NAME_1 <- toupper(indian_state_map$NAME_1)
# Merge the filtered india_map with deceased_summary
map_data_merged <- merge(indian_state_map, deceased_summary, by.x = "NAME_1", by.y = "State")

# Plot the heatmap
# Plot the map with adjusted fill scale and color scale
ggplot() +
  geom_sf(data = map_data_merged, aes(fill = Deceased), color = "black") +
  scale_fill_gradient(name = "Number of Deceased",
                      low = "lightblue", high = "red",
                      trans = "log", na.value = "grey50") +
  labs(title = "Heatmap of Deceased Counts in Indian States",
       caption = "Data Source: Your Source") +
  theme_minimal()



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
  labs(title = "Monthly mortality rates during pandemic in India",
       x = "Month",
       y = "Total Deaths")


