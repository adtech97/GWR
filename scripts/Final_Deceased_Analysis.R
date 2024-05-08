library(GWmodel)
library(sp)
library(raster)
library(sf)
library(GWmodel)
library(sp)
library(raster)
library(sf)
library(ggplot2)
library(tmap)


# Load the CSV data
data <- read.csv("data/aggregated_data.csv")

# Define dependent and independent variables
dependent_var <- "Deceased"
independent_vars <- c("Recovered", "Confirmed", "Tested", "AQIUS", "PM25", "PM10", "Temp", "Humid", "population")

# Load India shapefile
india_map <- "assets/gadm36_IND_filtered.shp"

# Convert data to SpatialPointsDataFrame
coordinates(data) <- c("longitude", "latitude")  # Assuming your CSV has longitude and latitude columns
proj4string(data) <- CRS("+proj=longlat +datum=WGS84")  # Assuming WGS84 coordinate system

# Define the equation for GWR
eq <- paste(dependent_var, "~", paste(independent_vars, collapse = "+"))

# Bandwidth selection
bw <- bw.gwr(eq, 
             data = data, 
             approach = "AIC", 
             kernel = "gaussian",
             adaptive = TRUE, 
             p = 2,
             parallel.method = "omp",
             parallel.arg = "omp")

# Run GWR
gwr_result <- gwr.basic(eq, 
                        data = data, 
                        bw = bw, 
                        kernel = "gaussian", 
                        adaptive = TRUE, 
                        p = 2, 
                        F123.test = FALSE,
                        cv = FALSE,
                        parallel.method = "omp",
                        parallel.arg = "omp")

gwr_result



# Extract GWR results
#gwr_sf <- st_as_sf(gwr_result$SDF)
#gwr_sf


# Load necessary libraries
library(ggplot2)
library(sf)

# Load the shapefile of India
india <- st_read("assets/gadm36_IND_filtered.shp")

# Convert the data to an sf object
gwr_sf <- st_as_sf(gwr_sf, coords = c("X", "Y"), crs = 4326)  # Assuming X and Y are columns representing longitude and latitude

# Plot the points on the map of India
ggplot() +
  geom_sf(data = india, fill = "lightblue") +
  geom_sf(data = gwr_sf, aes(color = "GWR"), size = 1) +  # Add color aesthetic and specify legend label
  scale_color_manual(values = "red", name = "Legend", labels = c("GWR")) +  # Add manual color scale and legend
  theme_minimal()





tmap_mode("view")



tmap_mode("view")
tm_shape(india) +
  tm_polygons() +
  tm_shape(gwr_sf) +
  tm_dots(size = "Recovered", col = "Recovered") +
  tm_layout(main.title = "GWR Analysis of COVID-19 Confirmed Cases in India")



# Define data folder
dataFolder <- "assets/"

# Load the India shapefile
india <- st_read("assets/gadm36_IND_filtered.shp")

# Extract GWR results
india$y <- gwr_result$SDF$y
india$yhat <- gwr_result$SDF$yhat
india$residual <- gwr_result$SDF$residual
rsd <- sd(india$residual)
india$stdRes <- india$residual / rsd
india$LLN <- india$yhat - 1.645 * rsd
india$ULN <- india$yhat + 1.645 * rsd

# Intercept
india$Intercept <- gwr_result$SDF$Intercept
india$est_Recovered <- gwr_result$SDF$Recovered
india$est_Confirmed <- gwr_result$SDF$Confirmed
india$est_Tested <- gwr_result$SDF$Tested
india$est_AQIUS <- gwr_result$SDF$AQIUS
india$est_PM25 <- gwr_result$SDF$PM25
india$est_PM10 <- gwr_result$SDF$PM10
india$est_Temp <- gwr_result$SDF$Temp
india$est_Humid <- gwr_result$SDF$Humid
india$est_population <- gwr_result$SDF$population


# T-values
india$t_Intercept <- gwr_result$SDF$Intercept_TV
india$t_Recovered <- gwr_result$SDF$Recovered_TV
india$t_Confirmed <- gwr_result$SDF$Confirmed
india$t_Tested <- gwr_result$SDF$Tested_TV
india$t_AQIUS <- gwr_result$SDF$AQIUS_TV
india$t_PM25 <- gwr_result$SDF$PM25_TV
india$t_PM10 <- gwr_result$SDF$PM10_TV
india$t_Temp <- gwr_result$SDF$Temp_TV
india$t_Humid <- gwr_result$SDF$Humid_TV
india$t_population <- gwr_result$SDF$population_TV


# Calculate pseudo-t values
india$p_Recovered <- 2 * pt(-abs(gwr_result$SDF$Recovered_TV), df = 3103)
india$p_Confirmed <- 2 * pt(-abs(gwr_result$SDF$Confirmed_TV), df = 3103)
india$p_Tested <- 2 * pt(-abs(gwr_result$SDF$Tested_TV), df = 3103)
india$p_AQIUS <- 2 * pt(-abs(gwr_result$SDF$AQIUS_TV), df = 3103)
india$p_PM25 <- 2 * pt(-abs(gwr_result$SDF$PM25_TV), df = 3103)
india$p_PM10 <- 2 * pt(-abs(gwr_result$SDF$PM10_TV), df = 3103)
india$p_Temp <- 2 * pt(-abs(gwr_result$SDF$Temp_TV), df = 3103)
india$p_Humid <- 2 * pt(-abs(gwr_result$SDF$Humid_TV), df = 3103)
india$p_population <- 2 * pt(-abs(gwr_result$SDF$population_TV), df = 3103)

# Significance indicators
india$sig_Recovered <- ifelse(india$est_Recovered > 0 & india$p_Recovered <= 0.05, 1, 0)
india$sig_Confirmed <- ifelse(india$est_Confirmed > 0 & india$p_Confirmed <= 0.05, 1, 0)
india$sig_Tested <- ifelse(india$est_Tested > 0 & india$p_Tested <= 0.05, 1, 0)
india$sig_AQIUS <- ifelse(india$est_AQIUS > 0 & india$p_AQIUS <= 0.05, 1, 0)
india$sig_PM25 <- ifelse(india$est_PM25 > 0 & india$p_PM25 <= 0.05, 1, 0)
india$sig_PM10 <- ifelse(india$est_PM10 > 0 & india$p_PM10 <= 0.05, 1, 0)
india$sig_Temp <- ifelse(india$est_Temp > 0 & india$p_Temp <= 0.05, 1, 0)
india$sig_Humid <- ifelse(india$est_Humid > 0 & india$p_Humid <= 0.05, 1, 0)
india$sig_population <- ifelse(india$est_population > 0 & india$p_population <= 0.05, 1, 0)





library(sp)
library(sf)

# Convert the 'sf' object to 'Spatial' class
india_sp <- as(india, "Spatial")

# Convert the 'Spatial' object to 'SpatialLines'
india_lines <- as(india_sp, "SpatialLines")

# Create 'polys' list
polys <- list("sp.lines", india_lines, col = "grey", lwd = 0.8, lty = 1)







col.palette <- colorRampPalette(c("lightcyan", "cyan", "cyan1", "cyan2", "cyan3", "cyan4", "darkblue"), space = "rgb", interpolate = "linear")





library(ggplot2)



library(gridExtra)




library(ggplot2)

# Plotting for est_Recovered
est_recovered <- ggplot() +
  geom_sf(data = india, aes(fill = est_Recovered)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "Recovered") +
  theme_minimal()

# Plotting for est_Confirmed
est_Confirmed <- ggplot() +
  geom_sf(data = india, aes(fill = est_Confirmed)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "Confirmed") +
  theme_minimal()

# Plotting for est_Tested
est_tested <- ggplot() +
  geom_sf(data = india, aes(fill = est_Tested)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "Tested") +
  theme_minimal()

# Plotting for est_AQIUS
est_aqius <- ggplot() +
  geom_sf(data = india, aes(fill = est_AQIUS)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "AQIUS") +
  theme_minimal()

# Plotting for est_PM25
est_pm25 <- ggplot() +
  geom_sf(data = india, aes(fill = est_PM25)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "PM25") +
  theme_minimal()

# Plotting for est_PM10
est_pm10 <- ggplot() +
  geom_sf(data = india, aes(fill = est_PM10)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "PM10") +
  theme_minimal()

# Plotting for est_Temp
est_temp <- ggplot() +
  geom_sf(data = india, aes(fill = est_Temp)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "Temp") +
  theme_minimal()

# Plotting for est_Humid
est_humid <- ggplot() +
  geom_sf(data = india, aes(fill = est_Humid)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "Humid") +
  theme_minimal()

# Plotting for est_population
est_population <- ggplot() +
  geom_sf(data = india, aes(fill = est_population)) +
  scale_fill_gradientn(colors = col.palette(100)) +
  labs(title = "Population") +
  theme_minimal()

library(grid)
library(gridExtra)

# Define the title as a grob
title_grob <- textGrob("Local Estimates", gp = gpar(fontsize = 25))

# Arrange the plots with the title
grid.arrange(est_recovered, est_Confirmed, est_tested, est_aqius, est_pm25, est_pm10, est_temp, est_humid, est_population,
             ncol = 3, top = title_grob)



# Define color palette for t-values
col.palette.t <- colorRampPalette(c("blue", "sky blue", "green", "yellow", "pink", "red"), space = "rgb", interpolate = "linear")

# Plotting for t_Recovered
t_recovered <- ggplot() +
  geom_sf(data = india, aes(fill = t_Recovered)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "Recovered") +
  theme_minimal()

# Plotting for t_Deceased
t_Confirmed <- ggplot() +
  geom_sf(data = india, aes(fill = t_Confirmed)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "Deceased") +
  theme_minimal()

# Plotting for t_Tested
t_tested <- ggplot() +
  geom_sf(data = india, aes(fill = t_Tested)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "Tested") +
  theme_minimal()

# Plotting for t_AQIUS
t_aqius <- ggplot() +
  geom_sf(data = india, aes(fill = t_AQIUS)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "AQIUS") +
  theme_minimal()

# Plotting for t_PM25
t_pm25 <- ggplot() +
  geom_sf(data = india, aes(fill = t_PM25)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "PM25") +
  theme_minimal()

# Plotting for t_PM10
t_pm10 <- ggplot() +
  geom_sf(data = india, aes(fill = t_PM10)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "PM10") +
  theme_minimal()

# Plotting for t_Temp
t_temp <- ggplot() +
  geom_sf(data = india, aes(fill = t_Temp)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "Temp") +
  theme_minimal()

# Plotting for t_Humid
t_humid <- ggplot() +
  geom_sf(data = india, aes(fill = t_Humid)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "Humid") +
  theme_minimal()

# Plotting for t_population
t_population <- ggplot() +
  geom_sf(data = india, aes(fill = t_population)) +
  scale_fill_gradientn(colors = col.palette.t(100)) +
  labs(title = "Population") +
  theme_minimal()

# Arrange the plots for t-values
grid.arrange(t_recovered, t_Confirmed, t_tested, t_aqius, t_pm25, t_pm10, t_temp, t_humid, t_population,
             ncol = 3, top = textGrob("Local t-values", gp = gpar(fontsize = 25)))






library(sf)

# Define color palette
myPaletteRes <- colorRampPalette(c("lightseagreen", "lightsteelblue1", "moccasin", "hotpink", "red"))

# Extract the stdRes column
std_res <- india$stdRes

# Get unique values in std_res
unique_values <- unique(std_res)

# Determine the length of the color vector
num_unique_values <- length(unique_values)
colors <- myPaletteRes(num_unique_values)

# Plot the sf object with specified colors
plot(india["stdRes"], col = colors, main = "GWRP Std. Residuals")

# Add legend
legend("left", legend = unique_values, fill = colors, title = "Std. Residuals", cex = 0.8)







library(sp)
library(sf)

# Convert the sf object to SpatialPolygonsDataFrame
india_spdf <- as(india, "Spatial")

# Now you can use spplot with india_spdf
std_res <- spplot(india_spdf, "stdRes", main = "GWRP Std. Residuals",
                  sp.layout = list(polys),
                  col = "transparent",
                  col.regions = myPaletteRes(100))

# Print the plot
print(std_res)


