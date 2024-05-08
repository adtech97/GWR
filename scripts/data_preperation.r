library(readr)
library(dplyr)

censor_data <- function(df) {
  df <- mutate(df, 
               population = pmin(population, 10000000)  # Cap deaths at 1000
  )
  return(df)
}

# Read raw data
raw_data <- read_csv("data/states.csv")

# Convert Date column to date format
raw_data$Date <- as.Date(raw_data$Date, format = "%d-%m-%Y")

# Create a new column for month
raw_data$Month <- format(raw_data$Date, "%Y-%m")
raw_data_filtered <- raw_data %>%
  filter(State != "India")


# Aggregate data by month and State, summing the numbers
aggregated_data <- raw_data_filtered %>%
  group_by(Month, State) %>%
  summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
            Recovered = sum(Recovered, na.rm = TRUE),
            Deceased = sum(Deceased, na.rm = TRUE),
            Other = sum(Other, na.rm = TRUE),
            Tested = sum(Tested, na.rm = TRUE),
            .groups = "drop")

# Apply censoring (if needed)

state_coords <- read_csv("data/poptable.csv")
# Standardize state names in aggregated_data
aggregated_data$State <- toupper(gsub(" Islands", "", aggregated_data$State))

# Standardize state names in state_coords
state_coords$State.Name <- toupper(state_coords$State.Name)

# Merge datasets based on state names, ignoring case
merged_data <- merge(aggregated_data, state_coords, by.x = "State", by.y = "State.Name", ignore.case = TRUE)

# Print the merged dataset
#print(merged_data)

processed_data <- censor_data(merged_data)




processed_data <- processed_data %>% filter(State != "ANDAMAN AND NICOBAR")





state_wise_aqi <- read_csv("data/state-wise-aqi.csv")
state_wise_aqi$State <- toupper(state_wise_aqi$State)
final_merged_data <- merge(processed_data, state_wise_aqi, by = "State")
keeps <- c("State"	,"Month","Tested", "Confirmed",	"Deceased"	,"latitude"	,"longitude"	,"population"	,"Status"	,"AQIUS","PM25"	,"PM10"	,"Temp"	,"Humid")

write_csv(final_merged_data[keeps], "data/processed_data.csv")