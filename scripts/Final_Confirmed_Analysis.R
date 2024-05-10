library(GWmodel)      ## GW models
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(gridExtra)    #  Multiple plot
library(grid)
library(ggplot2)      # plot
library(latticeExtra) # advance ploting function
library(RStoolbox)    # advance raster ploting function
#library(SpatialML)    # Geographicall Weigted Random Forest
library(dplyr)
library(sp)

covid_19_data <- read.csv('data/processed_data.csv')
state_map <- shapefile('assets/gadm36_IND_filtered.shp')

summary(covid_19_data)
print(state_map)
aggregated_data <- covid_19_data %>%
  group_by(State) %>%
  summarize(
    across(
      .cols = !c(Month, Status),  # Exclude Month and Status columns
      .fns = sum
    )
  )


xy <- aggregated_data[ , c(5:6)]


SPDF <- SpatialPointsDataFrame(coords=xy, data = aggregated_data)

SPDF$Confirmed <- scale(SPDF$Confirmed)
SPDF$PM25 <- scale(SPDF$PM25)
SPDF$population <- scale(SPDF$population)
SPDF$Humid <- scale(SPDF$Humid)
SPDF$Tested <- scale(SPDF$Tested)
SPDF$Temp <- scale(SPDF$Temp)
SPDF$PM10 <- scale(SPDF$PM10)
SPDF$Deceased <- scale(SPDF$Deceased)
SPDF$AQIUS <- scale(SPDF$AQIUS)

gwr.bw <- bw.gwr(Confirmed ~ PM25+population+Humid+Tested+Temp+PM10+Deceased+AQIUS ,
                 data = SPDF,
                 approach = "AICc",
                 kernel='bisquare',
                 adaptive=TRUE)

gwr.bw


gwr.res <-gwr.basic(Confirmed ~ PM25+population+Humid+Tested+Temp+PM10+Deceased+AQIUS,
                    data=SPDF,
                    bw=gwr.bw,
                    kernel='bisquare',
                    adaptive=TRUE)
gwr.res


#Extract results for visualizations




state_map@data$y <- gwr.res$SDF$y
state_map@data$yhat <- gwr.res$SDF$yhat
state_map@data$residual <- gwr.res$SDF$residual
rsd=sd(state_map@data$residual)
state_map@data$stdRes <- (state_map@data$residual) / rsd
state_map@data$LLN <- state_map@data$yhat-1.645*rsd
state_map@data$ULN <- state_map@data$yhat+1.654*rsd

#Intercept
state_map@data$Intercept <- gwr.res$SDF$Intercept
state_map@data$est_PM25 <- gwr.res$SDF$PM25
state_map@data$est_population <- gwr.res$SDF$population
state_map@data$est_Humid <- gwr.res$SDF$Humid
state_map@data$est_Tested <- gwr.res$SDF$Tested
state_map@data$est_Temp <- gwr.res$SDF$Temp
state_map@data$est_PM10 <- gwr.res$SDF$PM10
state_map@data$est_Deceased <- gwr.res$SDF$Deceased


#T-values
state_map@data$t_Intercept <- gwr.res$SDF$Intercept_TV
state_map@data$t_PM25 <- gwr.res$SDF$PM25_TV
state_map@data$t_population <- gwr.res$SDF$population_TV
state_map@data$t_Humid <- gwr.res$SDF$Humid_TV
state_map@data$t_Tested <- gwr.res$SDF$Tested_TV
state_map@data$t_Temp <- gwr.res$SDF$Temp_TV
state_map@data$t_PM10 <- gwr.res$SDF$PM10_TV
state_map@data$t_Deceased <- gwr.res$SDF$Deceased_TV


#Vizualizations :
polys<- list("sp.lines", as(state_map, "SpatialLines"), col="grey", lwd=.8,lty=1)
col.palette<-colorRampPalette(c("blue",  "sky blue", "green","yellow", "red"),space="rgb",interpolate = "linear")
col.palette<-colorRampPalette(c("lightcyan","cyan","cyan1", "cyan2","cyan3","cyan4", "darkblue"),space="rgb",interpolate = "linear") 


est_pop<-spplot(state_map,"est_population", main = "population", 
                sp.layout=list(polys),
                col="transparent",
                col.regions=col.palette(100))
est_pm25<-spplot(state_map,"est_PM25", main = "PM25", 
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=col.palette(100))
est_pm10<-spplot(state_map,"est_PM10", main = "PM10", 
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=col.palette(100))

est_hum<-spplot(state_map,"est_Humid", main = "humidity", 
                sp.layout=list(polys),
                col="transparent",
                col.regions=col.palette(100))
est_temp<-spplot(state_map,"est_Temp", main = "Temprature", 
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=col.palette(100))
est_Deceased <- spplot(state_map, "est_Deceased", main = "Deceased cases",
                   sp.layout=list(polys),
                   col="transparent",
                   col.regions=col.palette(100))

est_test <- spplot(state_map, "est_Tested", main = "Tested cases",
                   sp.layout=list(polys),
                   col="transparent",
                   col.regions=col.palette(100))

grid.arrange(est_pop,est_pm25,est_pm10, est_hum, est_temp, est_Deceased, est_test,ncol= 3, top = textGrob("Local Estimates",gp=gpar(fontsize=25)))

col.palette.t<-colorRampPalette(c("blue",  "sky blue", "green","yellow","pink", "red"),space="rgb",interpolate = "linear") 

t_pop <- spplot(state_map, "t_population", main="Population",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))
t_pm25 <- spplot(state_map, "t_PM25", main="PM25",
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=rev(col.palette.t(100)))
t_pm10 <- spplot(state_map, "t_PM10", main="PM10",
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=rev(col.palette.t(100)))
t_hum <- spplot(state_map, "t_Humid", main="Humidity",
                sp.layout=list(polys),
                col="transparent",
                col.regions=rev(col.palette.t(100)))
t_Temp <- spplot(state_map, "t_Temp", main="Temprature",
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=rev(col.palette.t(100)))
t_Deceased <- spplot(state_map, "t_Deceased", main="Deceased Cases",
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=rev(col.palette.t(100)))
t_test <- spplot(state_map, "t_Tested", main="Tested cases",
                 sp.layout=list(polys),
                 col="transparent",
                 col.regions=rev(col.palette.t(100)))

grid.arrange(t_pop, t_pm25, t_pm10, t_hum, t_Temp, t_Deceased, t_test,ncol=3,
             top=textGrob("Local t-values", gp=gpar(fontsize=25)))





myPaletteRes <- colorRampPalette(c("lightseagreen","lightsteelblue1", "moccasin","hotpink", "red"))
std_res<-spplot(state_map,"stdRes", main = "GWRP Std. Residuals", 
                sp.layout=list(polys),
                col="transparent",
                col.regions=myPaletteRes(100))

print(std_res)