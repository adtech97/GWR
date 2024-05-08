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

CSPDF$`AQIUS` <- scale(CSPDF$`AQIUS`)
CSPDF$PM10<- scale(CSPDF$PM10)
CSPDF$Temp <- scale(CSPDF$Temp)
CSPDF$Humid <- scale(CSPDF$Humid)
CSPDF$PM25<- scale(CSPDF$PM25)
CSPDF$population<-scale(CSPDF$population)
CSPDF$Confirmed<-scale(CSPDF$Confirmed)
CSPDF$Tested<-scale(CSPDF$Tested)
bw.gwr <- bw.ggwr(Deceased ~ Confirmed+Tested+population+PM25+Temp+Humid,
                  data=CSPDF,
                  family="poisson",
                  approach="AICc",
                  kernel="bisquare",
                  adaptive=TRUE,
                  dMat=dm)



bgwr.res <- ggwr.basic(formula=Deceased ~ Confirmed+Tested+population+PM25+Temp+Humid,
                       data=CSPDF,
                       family="poisson",
                       bw=bw.gwr,
                       kernel="bisquare",
                       adaptive=TRUE,
                       dMat=dm)

print(bgwr.res)


#Presentations layer vizualizations for inference.

state_map_filtered <- shapefile("assets/gadm36_IND_filtered.shp")
state_map_filtered <- state_map_filtered[-nrow(state_map_filtered), ]
#Extract Gwr data

state_map_filtered@data$y <- bgwr.res$SDF$y
state_map_filtered@data$yhat <- bgwr.res$SDF$yhat
state_map_filtered@data$residual <- bgwr.res$SDF$residual
rsd = sd(state_map_filtered@data$residual)
state_map_filtered@data$stdRes <- (state_map_filtered@data$residual) / sd(state_map_filtered@data$residual)

state_map_filtered@data$LLN <- bgwr.res$SDF$yhat - 1.645 * rsd

state_map_filtered@data$ULN <- bgwr.res$SDF$yhat+1.645*rsd



#Extract Intercept
state_map_filtered@data$Intercept<-bgwr.res$SDF$Intercept
state_map_filtered@data$est_population<-bgwr.res$SDF$population
state_map_filtered@data$est_PM25<-bgwr.res$SDF$PM25
state_map_filtered@data$est_PM10<-bgwr.res$SDF$PM10
state_map_filtered@data$est_Temp<-bgwr.res$SDF$Temp
state_map_filtered@data$est_Humid<-bgwr.res$SDF$Humid


#Exract T-Values
state_map_filtered@data$Intercept_tv<-bgwr.res$SDF$Intercept_TV
state_map_filtered@data$population_tv<-bgwr.res$SDF$population_TV
state_map_filtered@data$PM25_tv<-bgwr.res$SDF$PM25_TV
state_map_filtered@data$PM10_tv<-bgwr.res$SDF$PM10_TV
state_map_filtered@data$Temp_tv<-bgwr.res$SDF$Temp_TV
state_map_filtered@data$Humid_tv<-bgwr.res$SDF$Humid_TV

#Calcilate pseudo T value

state_map_filtered@data$p_population <- 2*pt(-abs(bgwr.res$SDF$population_TV), df=3103)
state_map_filtered@data$p_PM25 <- 2*pt(-abs(bgwr.res$SDF$PM25_TV), df=3103)
state_map_filtered@data$p_PM10 <- 2*pt(-abs(bgwr.res$SDF$PM10_TV), df=3103)
state_map_filtered@data$p_Humid <- 2*pt(-abs(bgwr.res$SDF$Humid_TV), df=3103)
state_map_filtered@data$p_Temp <- 2*pt(-abs(bgwr.res$SDF$Temp_TV), df=3103)


print(names(state_map_filtered))

polys<- list("sp.lines", as(state_map_filtered, "SpatialLines"), col="grey", lwd=.8,lty=1)
col.palette<-colorRampPalette(c("blue",  "sky blue", "green","yellow", "red"),space="rgb",interpolate = "linear")

col.palette<-colorRampPalette(c("lightcyan","cyan","cyan1", "cyan2","cyan3","cyan4", "darkblue"),space="rgb",interpolate = "linear") 


est_pop<-spplot(state_map_filtered,"est_population", main = "population", 
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=col.palette(100))
est_pm25<-spplot(state_map_filtered,"est_PM25", main = "PM25", 
                sp.layout=list(polys),
                col="transparent",
                col.regions=col.palette(100))
est_pm10<-spplot(state_map_filtered,"est_PM10", main = "PM10", 
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=col.palette(100))

est_hum<-spplot(state_map_filtered,"est_Humid", main = "humidity", 
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=col.palette(100))
est_temp<-spplot(state_map_filtered,"est_Temp", main = "Temprature", 
                sp.layout=list(polys),
                col="transparent",
                col.regions=col.palette(100))
grid.arrange(est_pop,est_pm25,est_pm10, est_hum, est_temp,ncol= 3, top = textGrob("Local Estimates",gp=gpar(fontsize=25)))



col.palette.t<-colorRampPalette(c("blue",  "sky blue", "green","yellow","pink", "red"),space="rgb",interpolate = "linear") 

t_pop <- spplot(state_map_filtered, "population_tv", main="Population",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))
t_pm25 <- spplot(state_map_filtered, "PM25_tv", main="PM25",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))
t_pm10 <- spplot(state_map_filtered, "PM10_tv", main="PM10",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))
t_hum <- spplot(state_map_filtered, "Humid_tv", main="Humidity",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))
t_Temp <- spplot(state_map_filtered, "Temp_tv", main="Temprature",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))

grid.arrange(t_pop, t_pm25, t_pm10, t_hum, t_Temp, ncol=3,
             top=textGrob("Local t-values", gp=gpar(fontsize=25)))



myPaletteRes <- colorRampPalette(c("lightseagreen","lightsteelblue1", "moccasin","hotpink", "red"))
std_res<-spplot(state_map_filtered,"stdRes", main = "GWRP Std. Residuals", 
                sp.layout=list(polys),
                col="transparent",
                col.regions=myPaletteRes(100))

print(std_res)
