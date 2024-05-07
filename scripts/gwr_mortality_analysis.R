library(GWmodel)      ### GW models
#library(sf)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         # plot
library(gridExtra)    # Multiple plot
library(ggplot2) 
library(readr)
library(dplyr)
library(sp)
#read processed data

covid_19_data <- read_csv('data/processed_data.csv')
state_map <- shapefile('assets/gadm36_IND_1.shp')
state_map$NAME_1 <- toupper(state_map$NAME_1)
# Aggregate covid_19_data by State
# Aggregate covid_19_data by State
aggregated_data <- covid_19_data %>%
  group_by(State) %>%
  summarize(
    across(
      .cols = !c(Month, Status),  # Exclude Month and Status columns
      .fns = sum
    )
  )


# Merge aggregated_data with state_map
CSPDF <- merge(state_map, aggregated_data, by.x = "NAME_1", by.y = "State", all = FALSE)


head(CSPDF)

# Calculate the bandwidth
coords = cbind(CSPDF$latitude, CSPDF$longitude)
# Create a data frame with the required columns


# Create the SpatialPointsDataFrame

CSPDF_df <- as.data.frame(CSPDF)

dm = gw.dist(dp.locat=coords)
coords_sp <- SpatialPoints(coords)
CSPDF_sp <- SpatialPointsDataFrame(coords_sp, data=CSPDF_df)
bw.gwr <- bw.ggwr(Deceased ~ population + PM10 + Temp + Humid,
                  data=CSPDF,
                  family="poisson",
                  approach="AICc",
                  kernel="bisquare",
                  adaptive=TRUE,
                  dMat=dm)



bgwr.res <- ggwr.basic(formula=Deceased ~ population + PM10 + Temp + Humid,
                       data=CSPDF,
                       family="poisson",
                       bw=bw.gwr,
                       kernel="bisquare",
                       adaptive=TRUE,
                       dMat=dm)

print(bgwr.res)

