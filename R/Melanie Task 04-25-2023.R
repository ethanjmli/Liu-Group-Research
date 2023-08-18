library(MODIStsp) #For getting MODIS data
library(ggplot2) #For plotting maps
library(raster) #For reading rasters
#library(rgdal) #readOGR()
library(sf) #For working with sf objects 
library(terra)
#library(devtools)
library(stars)
library(stringr) #For working with strings
#library(rasterVis)
library(tidyverse)
library(dplyr)
library(lubridate) #For working with dates
library(viridis)
library(ggnewscale)
library(patchwork)
library(rgeos)
library(tidycensus)
library(tmap)
library(RColorBrewer)
library(gridExtra)
library(ggmap)
library(osmdata)
library(gstat)
tempcolorvector <- inferno(300)
tmap_options(check.and.fix = TRUE)

maparea <- get_acs(
  geography = "county", 
  variables = "C02003_004",
  state = "GA",
  year = 2021,
  geometry = TRUE
)

maparea <- maparea %>% dplyr::select(GEOID, NAME, geometry)
maparea$NAME <- str_sub(maparea$NAME,1,-17)

tm_shape(maparea)+
  tm_borders()+
  tm_text("NAME",size=0.3)


crop_parameters_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = st_crs(maparea))
cropLongLat_5c <- st_make_valid(cropLongLat_5c)
cropLongLat5c_small <- cropLongLat_5c[,9]
rm(crop_parameters_5c)
rm(cropLongLat_5c)

maparea.sub <- st_crop(maparea,cropLongLat5c_small)
rownames(maparea.sub)<-NULL
ymod <- c(0,0,0,0,0,-1,0,0,0,-4,0,0,0,0,0,0,0,0,0,0,0,0)
xmod <- c(0,0,0,0,0,0,2,0,0,-3,0,0,0,0,0,0,0,0,0,0,0,0)
maparea.sub$xmod <- xmod
maparea.sub$ymod <- ymod
bbox<- st_bbox(cropLongLat5c_small)
bbox
bbox[1] <- -84.76
bbox[2] <- 33.53
bbox[3] <- -84.04
bbox[4] <- 34.13

maparea.sub <- st_crop(maparea.sub,bbox)

available_features()
available_tags(feature = "highway")
highways <- osmdata_sf(add_osm_feature(opq(bbox), 
                                           key = "highway", value = "motorway"))
highways <- highways$osm_lines
st_crs(highways)
highways <- st_transform(highways, st_crs(bbox))
highways <- st_crop(highways,bbox)
highways <- highways[highways$highway == "motorway",]

maparea.sub <- st_cast(maparea.sub)
rownames(maparea.sub)<-NULL
maparea.sub$xmod[9] <- 1
maparea.sub$ymod[14] <- -1
maparea.sub$NAME[1]<-NA

tm_shape(maparea.sub)+
  tm_borders()+
  tm_text("NAME",size=1,ymod = "ymod",xmod="xmod")+
tm_shape(highways)+
  tm_lines(lwd=3,col="red")+
tm_shape(highways)+
  tm_lines(lwd=2)+
tm_layout(frame=F)


setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
airtempfiles <- list.files()[c(grep("2020-08",list.files(),fixed=TRUE),
                               grep("2020-07",list.files(),fixed=TRUE),
                               grep("2020-06",list.files(),fixed=TRUE))]

airtemplist <- list()
for(i in 1:length(airtempfiles)){
  airtemplist[[i]] <- readRDS(airtempfiles[i])
}

airtemplist <- bind_rows(airtemplist)
airtemplist <- airtemplist %>% group_by(Latitude,Longitude) %>% summarize(maxtemp = max(tmax))

coords <- st_as_sf(airtemplist[,c(1,2)], coords = c("Longitude","Latitude"), crs=st_crs(bbox))
coords <- st_crop(coords, bbox)
coords <- st_coordinates(coords)
coords <- as.data.frame(coords)
names(coords) <- c("Longitude","Latitude")
airtemplist <- merge(airtemplist, coords, by = c("Longitude","Latitude"))

library(Metrics)
suppressMessages(library(spatstat))
obs_window <- owin(xrange=bbox[c(1,3)],yrange=bbox[c(2,4)])

ppp.t <- ppp(airtemplist$Longitude,airtemplist$Latitude,
              marks = airtemplist$maxtemp,window=obs_window)

powers <- seq(7.0,7.41,0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp.t, power=power, at="points")
  mse_result <- c(mse_result,Metrics::mse(ppp.t$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

airtempidw<- idw(ppp.t, power=7.31, at="pixels")
airtempidw <- raster(airtempidw)
plot(airtempidw)
values(airtempidw) <- values(airtempidw)*(9/5)+32
values(airtempidw)
plot(airtempidw)


crs(airtempidw) <- 4269

map1 <-tm_shape(airtempidw)+
  tm_raster(col="layer",palette=rev(tempcolorvector),
            breaks = c(seq(min(values(airtempidw),na.rm=T),max(values(airtempidw),na.rm=T),0.5),
                       max(values(airtempidw),na.rm=T)),
            labels = c("Low","","","","","","","","High"),
            style = "cont",title = "Temp °F",legend.reverse=T)+
tm_shape(maparea.sub)+
  tm_borders(col="white",lwd=3,lty="dashed")+
  tm_text("NAME",size=1,ymod = "ymod",xmod="xmod",col="white",shadow=T)+
tm_shape(highways)+
  tm_lines(lwd=3,col="white")+
tm_shape(highways)+
  tm_lines(lwd=2)+
  tm_layout(frame=F,
            main.title=paste0("Maximum Temperatures, June-August 2020"),
            main.title.size=1.4,
            main.title.position=0.07,
            legend.outside=T,
            legend.text.size=1,
            legend.title.size = 1.5
            )
  
