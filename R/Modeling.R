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
#library(ggnewscale)
#library(patchwork)
library(rgeos)
library(tidycensus)
library(tmap)
library(RColorBrewer)
#library(gridExtra)
#library(ggmap)
library(osmdata)
library(hydroTSM)
#library(gifski)
library(readxl)
library(seas)
library(here)
library(ncdf4)
library(tigris)
tempcolorvector <- colorRampPalette(brewer.pal(11,"Spectral"))(1000)
airpolcolorvector <- c(inferno(1000))

NDVIcolorvector <- colorRampPalette(bias=0.8, 
                                    c("#A50026","#D73027","#F46D43","#FDAE61","#FEE08B", 
                                      "#FFFFBF","#D9EF8B","#A6D96A","#66BD63", 
                                      "#1A9850","#006837"))(1000)

humiditycolorvector <- colorRampPalette(brewer.pal(11,"BrBG"))(1000)
tmap_options(check.and.fix = TRUE)
#####MODIS GUI#####
MODIStsp() #Load GUI for downloading specific areas/times of satellite data
#Note: MOD11a1 v6 day/night LST data is in units of Kelvin, scale factor = *50 
#multiply by 0.02 for real value(s) in Kelvin
#-273 for values in Celsius
#Yes R Rasterstack
#Yes Reprocess
#Projection = set by user: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

#####Get shapefiles and boundaries#####
# crop_parameters_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
# crop_parameters_5c <- st_read("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
crop_parameters_5c <- st_read(here::here("Data","ATL_shps_for_April","five_counties.shp"))
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
cropLongLat_5c <- st_make_valid(cropLongLat_5c)
cropLongLat5c_small <- cropLongLat_5c[,9]
ymod <- c(0,0,-4,0,0)
xmod <- c(0,0,-3,0,0)
cropLongLat_5c <- cbind(cropLongLat_5c,ymod, xmod)
rm(crop_parameters_5c)
st_bbox(cropLongLat_5c)

#crop_parameters_zip <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/expandedzip.shp")
crop_parameters_zip <- st_read(here::here("Data","ATL_shps_for_April","expandedzip.shp"))
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
xmod <- c(1,0,0,1,-0.4,1)
cropLongLat_zip <- cbind(cropLongLat_zip,xmod)
cropLongLatzip_small <- cropLongLat_zip[,3]
colnames(cropLongLat_zip)[1]<-"ZIP Code"
rm(crop_parameters_zip)

zipcrop <- st_bbox(cropLongLat_zip)
zipcrop[2] <- 33.615
zipcrop[3] <- -84.34
zipcrop


####NOAA####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
list.files()
NOAAdaily <- read.csv("Atlanta Daily Weather 2.csv")
NOAAdaily <- read_excel("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Atlanta Daily Weather 1.xlsx",guess_max=1000000)
table(NOAAdaily$NAME)

NOAAdaily.locations <- NOAAdaily[,c(1,2,3,4)]
NOAAdaily.locations <- distinct(NOAAdaily.locations)
NOAAdaily.locations <- st_as_sf(NOAAdaily.locations,coords = c("LONGITUDE","LATITUDE"), crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
st_relate(NOAAdaily.locations,pattern = "T*F**FFF*",sparse=F)
tm_shape(NOAAdaily.locations)+
  tm_dots(size=0.01, col="red")+
  tm_text("NAME",size=0.4)+
tm_shape(cropLongLat5c_small)+
  tm_borders()


colnames(NOAAdaily)
NOAAdaily$STATION
NOAAdaily.temps <- NOAAdaily %>% dplyr::select(STATION,NAME,LATITUDE,LONGITUDE,DATE,TAVG,TMAX,TMIN)
NOAAdaily.temps <- NOAAdaily.temps %>% dplyr::filter(!is.na(TAVG)|(!is.na(TMAX) & !is.na(TMIN)))
NOAAdaily.temps <- NOAAdaily.temps %>% mutate(year = year(DATE))

NOAAdaily.temps.summary <- NOAAdaily.temps %>% group_by(year,NAME) %>% summarize(available = length(unique(DATE)),
                                                                                 year = mean(year)
                                                                                  )
NOAAdaily.temps.summary <- NOAAdaily.temps.summary %>% mutate(missing = year.length(year)-available)

NOAAdaily.temps.summary2 <- NOAAdaily.temps.summary %>% summarize(     
                                      mean.available = mean(available),
                                      max.available = max(available),
                                      mean.missing = mean(missing),
                                      percent.missing = (mean(missing)/year.length(mean(year)))*100,
                                      sites = length(unique(NAME)),
                                      site.names = paste(unique(NAME),collapse= ", ")
                                      )


NOAAdaily.temps.sf <- st_as_sf(NOAAdaily.temps,coords = c("LONGITUDE","LATITUDE"),crs =st_crs("+proj=longlat +datum=WGS84 +no_defs") )
NOAAdaily.temps.sf <- NOAAdaily.temps.sf %>% filter(!duplicated(geometry))
NOAAdaily.temps.sf$Agency <- "NOAA"


#######CDO####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
CobbClaytondaily <- read.csv("Cobb+Clayton Daily.csv")
Dekalbdaily <- read.csv("Dekalb Daily.csv")
Fultondaily <- read.csv("Fulton Daily.csv")
Gwinnettdaily <- read.csv("Gwinnett Daily.csv")
CDOdaily <- bind_rows(CobbClaytondaily,Dekalbdaily,Fultondaily,Gwinnettdaily)
write.csv(CDOdaily, "Five Counties NOAA CDO Daily Weather.csv")
CDOdaily <- read.csv("Five Counties NOAA CDO Daily Weather.csv")

unique(NOAAdaily$NAME) %in% unique(CDOdaily$NAME)

CDOdaily.locations <- CDOdaily[,c(1,2,3,4)]
CDOdaily.locations <- distinct(CDOdaily.locations)
which(table(CDOdaily.locations$NAME)>1)
CDOdaily.locations <- CDOdaily.locations[!duplicated(CDOdaily.locations$NAME),]
CDOdaily.locations <- st_as_sf(CDOdaily.locations,coords = c("LONGITUDE","LATITUDE"), crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
# CDOidentical <- as.data.frame(st_relate(CDOdaily.locations,pattern = "T*F**FFF*",sparse=F))
# CDOidentical[CDOidentical == FALSE] <- NA

tm_shape(cropLongLat5c_small)+
  tm_borders()+
tm_shape(CDOdaily.locations)+
  tm_dots(size=0.01, col="red")
  #tm_text("NAME",size=0.4)+


colnames(CDOdaily)
CDOdaily$STATION
CDOdaily.temps <- CDOdaily %>% dplyr::select(STATION,NAME,LATITUDE,LONGITUDE,DATE,TAVG,TMAX,TMIN)
#which(table(distinct(CDOdaily.temps)$NAME) > 2)
#CDOdaily.temps <- CDOdaily.temps %>% dplyr::filter(!is.na(TAVG)|(!is.na(TMAX) & !is.na(TMIN)))
unique(CDOdaily.temps$NAME)
CDOdaily.temps <- CDOdaily.temps %>% mutate(year = year(DATE))
sum(!is.na(CDOdaily.temps$TAVG) | !is.na(CDOdaily.temps$TMAX) | !is.na(CDOdaily.temps$TMIN))

CDOdaily.temps.summary <- CDOdaily.temps %>% group_by(year,STATION) %>% summarize(available = sum(!is.na(TAVG) | !is.na(TMAX) | !is.na(TMIN)),
                                                                                 year = mean(year)
                                                                               )

length(unique(CDOdaily$STATION))
length(unique(CDOdaily.temps.summary$STATION))
CDOdaily.temps.summary <- CDOdaily.temps.summary %>% mutate(missing = year.length(year)-available)

CDOdaily.temps.summary2 <- CDOdaily.temps.summary %>% summarize(     
  mean.available = mean(available),
  max.available = max(available),
  mean.missing = mean(missing),
  percent.missing = (mean(missing)/year.length(mean(year)))*100,
  sites = length(unique(STATION)),
  site.names = paste(unique(STATION),collapse= ", ")
)


CDOdaily.temps.sf <- st_as_sf(CDOdaily.temps,coords = c("LONGITUDE","LATITUDE"),crs =st_crs("+proj=longlat +datum=WGS84 +no_defs") )
CDOdaily.temps.sf <- CDOdaily.temps.sf %>% filter(!duplicated(geometry))
CDOdaily.temps.sf$Agency <- "NOAA CDO"




CDOdaily.temps.available <- CDOdaily.temps %>% dplyr::filter(!is.na(TAVG)|(!is.na(TMAX) & !is.na(TMIN)))

unique(CDOdaily.temps.available$NAME)
CDOdaily.temps.available <- CDOdaily.temps.available %>% mutate(year = year(DATE))
sum(!is.na(CDOdaily.temps.available$TAVG) | !is.na(CDOdaily.temps.available$TMAX) | !is.na(CDOdaily.temps.available$TMIN))

CDOdaily.temps.available.summary <- CDOdaily.temps.available %>% group_by(year,STATION) %>% summarize(available = sum(!is.na(TAVG) | !is.na(TMAX) | !is.na(TMIN)),
                                                                                  year = mean(year)
)

length(unique(CDOdaily$STATION))
length(unique(CDOdaily.temps.available.summary$STATION))
CDOdaily.temps.available.summary <- CDOdaily.temps.available.summary %>% mutate(missing = year.length(year)-available)

CDOdaily.temps.available.summary2 <- CDOdaily.temps.available.summary %>% summarize(     
  mean.available = mean(available),
  max.available = max(available),
  mean.missing = mean(missing),
  percent.missing = (mean(missing)/year.length(mean(year)))*100,
  sites = length(unique(STATION)),
  site.names = paste(unique(STATION),collapse= ", ")
)


CDOdaily.temps.available.sf <- st_as_sf(CDOdaily.temps.available,coords = c("LONGITUDE","LATITUDE"),crs =st_crs("+proj=longlat +datum=WGS84 +no_defs") )
CDOdaily.temps.available.sf <- CDOdaily.temps.available.sf %>% filter(!duplicated(geometry))
CDOdaily.temps.available.sf$Agency <- "NOAA CDO"


#test2 <- test[,c(2,3,4,6,29)]
#test2 <- st_as_sf(test2,coords = c("LONGITUDE","LATITUDE"), crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
#test.list <- split(test2,test2$NAME)
#test3 <- test.list[[1]]
#test4 <- test2
#test4$DATE <- lubridate::year(test4$DATE)
#test.list2 <- split(test4, test4$DATE)
#test.list2[[1]]$NAME
#i<-1

#for(i in 1:length(test.list2)){
#  print(unique(test.list2[[i]]$DATE))
#  #print(unique(test.list2[[i]]$NAME))
#  print(length(unique(test.list2[[i]]$NAME)))
#}

# is.na(AvgNAYear$TAVG)
# AvgNAYear <- test.list2[[1]]
# AvgNAYear2 <- AvgNAYear %>% group_by(NAME) %>% summarise(missing = sum(is.na(TAVG)))
# mean(AvgNAYear2$missing)
# 
# for(i in 1:length(test.list2)){
#   print(unique(test.list2[[i]]$DATE))
#   print(length(unique(test.list2[[i]]$NAME)))
#   AvgNAYear2 <- test.list2[[i]] %>% group_by(NAME) %>% summarise(missing = sum(is.na(TAVG)))
#   print(length(unique(AvgNAYear2$NAME)))
#   print(mean(AvgNAYear2$missing))
# }
# 
# i<-1
# for(i in 1:length(test.list2)){
#   print(unique(test.list2[[i]]$DATE))
#   print(length(unique(test.list2[[i]]$NAME)))
#   AvgNAYear3 <- test.list2[[i]] %>% group_by(NAME) %>% summarise(available = sum(!is.na(TAVG)))
#   print(length(unique(AvgNAYear2$NAME)))
#   print(max(AvgNAYear3$available))
# }
# 
# view(test.list2[[4]])


#####EPA TEMPERATURE#####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/EPA Daily Weather Data/TEMPERATURE")
list.filenames <- list.files(pattern = "^daily.*.csv$")
list.filenames
rm(i)
i<-1
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailytemp <- read.csv(list.filenames[i])
  EPA.dailytemp <- subset(EPA.dailytemp,State.Name == "Georgia")
  EPA.dailytemp <- st_as_sf(EPA.dailytemp, coords = c("Longitude","Latitude"),crs = 4269)
  EPA.dailytemp <- st_transform(EPA.dailytemp,crs="+proj=longlat +datum=WGS84 +no_defs")
  st_crs(EPA.dailytemp)
  st_crs(cropLongLat5c_small)
  EPA.dailytemp <- st_crop(EPA.dailytemp,cropLongLat5c_small)
  #st_coordinates(EPA.dailytemp.2022)
  #EPA.dailytemp.2022 <- cbind(as.data.frame(EPA.dailytemp.2022),st_coordinates(EPA.dailytemp.2022))
  st_write(EPA.dailytemp,paste0("cropped_",str_sub(list.filenames[i],1,-5),".csv"),layer_options = "GEOMETRY=AS_XY")
}

#test <- read.csv("cropped_daily_TEMP_2013.csv")
#sum(is.na(test$Arithmetic.Mean))
#unique(test$Site.Num)
#table(test$Local.Site.Name)

#test$year <- year(test$Date.Local)
#class(test$year)

list.files(pattern = regex("^cropped.*.TEMP.*.csv$"))
list.filenames <- list.files(pattern = regex("^cropped.*.TEMP.*.csv$"))
list.filenames
i<-1
EPA.dailytemp.check.summarized <- NULL
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailytemp.check <- read.csv(list.filenames[i])
  EPA.dailytemp.check$year <- year(EPA.dailytemp.check$Date.Local)
  EPA.dailytemp.check <- EPA.dailytemp.check %>% group_by(Local.Site.Name) %>% summarise(available = length(unique(Date.Local)),year=mean(year))
  EPA.dailytemp.check <- EPA.dailytemp.check %>% mutate(missing = year.length(year) - available)
  EPA.dailytemp.check.summarized <- rbind(EPA.dailytemp.check.summarized,EPA.dailytemp.check)
}

EPA.dailytemp.check.summarized.2 <- EPA.dailytemp.check.summarized %>% group_by(year) %>% 
                                      summarise(mean.available = mean(available),
                                                max.available = max(available),
                                                mean.missing = mean(missing),
                                                percent.missing = (mean(missing)/year.length(mean(year)))*100,
                                                sites=length(Local.Site.Name),
                                                site.names=paste(unique(Local.Site.Name),collapse=", "))

list.files(pattern = regex("^cropped.*.TEMP.*.csv$"))
list.filenames <- list.files(pattern = regex("^cropped.*.TEMP.*.csv$"))
list.filenames
EPA.dailytemp.list <- list()
for(i in 1:length(list.filenames)){
  EPA.dailytemp.list[[i]]<-read.csv(list.filenames[i])
}
names(EPA.dailytemp.list) <- list.filenames
EPA.dailytemp.sf <- bind_rows(EPA.dailytemp.list)
EPA.dailytemp.sf <- st_as_sf(EPA.dailytemp.sf,coords=c("X","Y"),crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
EPA.dailytemp.sf <- EPA.dailytemp.sf %>% filter(!duplicated(geometry))


#####EPA RH#####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/EPA Daily Weather Data/RELATIVE HUMIDITY")
list.files()
list.filenames <- list.files(pattern = "^daily.*.RH_DP.*.csv$")
list.filenames
rm(i)
i<-3
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailyRH <- read.csv(list.filenames[i])
  EPA.dailyRH <- subset(EPA.dailyRH,State.Name == "Georgia")
  EPA.dailyRH <- st_as_sf(EPA.dailyRH, coords = c("Longitude","Latitude"),crs = 4269)
  EPA.dailyRH <- st_transform(EPA.dailyRH,crs="+proj=longlat +datum=WGS84 +no_defs")
  st_crs(EPA.dailyRH)
  st_crs(cropLongLat5c_small)
  EPA.dailyRH <- st_crop(EPA.dailyRH,cropLongLat5c_small)
  #st_coordinates(EPA.dailyRH)
  #EPA.dailyRH <- cbind(as.data.frame(EPA.dailyRH),st_coordinates(EPA.dailyRH))
  st_write(EPA.dailyRH,paste0("cropped_",str_sub(list.filenames[i],1,-5),".csv"),layer_options = "GEOMETRY=AS_XY")
}

test <- read.csv("cropped_daily_RH_DP_2011.csv")
table(test$Local.Site.Name)


list.files(pattern = regex("^cropped.*.RH_DP.*.csv$"))
list.filenames <- list.files(pattern = regex("^cropped.*.RH_DP.*.csv$"))
list.filenames
i<-1
EPA.dailyRH.check.summarized <- NULL
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailyRH.check <- read.csv(list.filenames[i])
  EPA.dailyRH.check$year <- year(EPA.dailyRH.check$Date.Local)
  EPA.dailyRH.check <- EPA.dailyRH.check %>% group_by(Local.Site.Name) %>% summarise(available = length(unique(Date.Local)),numberobs = n(),year=mean(year))
  EPA.dailyRH.check.summarized <- rbind(EPA.dailyRH.check.summarized,EPA.dailyRH.check)
}

EPA.dailyRH.check.summarized.2 <- EPA.dailytemp.check.summarized %>% 
                                    group_by(year) %>% 
                                    summarise(available = mean(available),sites=length(Local.Site.Name),
                                              site.names=paste(unique(Local.Site.Name),collapse=", "))

EPA.dailyRH.check.summarized.2 == EPA.dailytemp.check.summarized.2


####EPA PRESSURE####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/EPA Daily Weather Data/BAROMETRIC PRESSURE")
list.files()
list.filenames <- list.files(pattern = "^daily.*.PRESS.*.csv$")
list.filenames
rm(i)
i<-3
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailyPRESS <- read.csv(list.filenames[i])
  EPA.dailyPRESS <- subset(EPA.dailyPRESS,State.Name == "Georgia")
  EPA.dailyPRESS <- st_as_sf(EPA.dailyPRESS, coords = c("Longitude","Latitude"),crs = 4269)
  EPA.dailyPRESS <- st_transform(EPA.dailyPRESS,crs="+proj=longlat +datum=WGS84 +no_defs")
  st_crs(EPA.dailyPRESS)
  st_crs(cropLongLat5c_small)
  EPA.dailyPRESS <- st_crop(EPA.dailyPRESS,cropLongLat5c_small)
  #st_coordinates(EPA.dailyPRESS)
  #EPA.dailyPRESS <- cbind(as.data.frame(EPA.dailyPRESS),st_coordinates(EPA.dailyPRESS))
  st_write(EPA.dailyPRESS,paste0("cropped_",str_sub(list.filenames[i],1,-5),".csv"),layer_options = "GEOMETRY=AS_XY")
}

test <- read.csv("cropped_daily_PRESS_2008.csv")
table(test$Local.Site.Name)

list.files(pattern = regex("^cropped.*.PRESS.*.csv$"))
list.filenames <- list.files(pattern = regex("^cropped.*.PRESS.*.csv$"))
list.filenames
i<-1
EPA.dailypress.check.summarized <- NULL
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailypress.check <- read.csv(list.filenames[i])
  EPA.dailypress.check$year <- year(EPA.dailypress.check$Date.Local)
  EPA.dailypress.check <- EPA.dailypress.check %>% group_by(Local.Site.Name) %>% summarise(available = length(unique(Date.Local)),numberobs = n(),year=mean(year))
  EPA.dailypress.check.summarized <- rbind(EPA.dailypress.check.summarized,EPA.dailypress.check)
}

EPA.dailypress.check.summarized.2 <- EPA.dailypress.check.summarized %>% group_by(year) %>% 
  summarise(available = mean(available),sites=length(Local.Site.Name),
            site.names=paste(unique(Local.Site.Name),collapse=", "))

####EPA WIND####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/EPA Daily Weather Data/WIND")
list.files()
list.filenames <- list.files(pattern = "^daily.*.WIND.*.csv$")
list.filenames
rm(i)
i<-3
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailyWIND <- read.csv(list.filenames[i])
  EPA.dailyWIND <- subset(EPA.dailyWIND,State.Name == "Georgia")
  EPA.dailyWIND <- st_as_sf(EPA.dailyWIND, coords = c("Longitude","Latitude"),crs = 4269)
  EPA.dailyWIND <- st_transform(EPA.dailyWIND,crs="+proj=longlat +datum=WGS84 +no_defs")
  st_crs(EPA.dailyWIND)
  st_crs(cropLongLat5c_small)
  EPA.dailyWIND <- st_crop(EPA.dailyWIND,cropLongLat5c_small)
  #st_coordinates(EPA.dailyWIND)
  #EPA.dailyWIND <- cbind(as.data.frame(EPA.dailyWIND),st_coordinates(EPA.dailyWIND))
  st_write(EPA.dailyWIND,paste0("cropped_",str_sub(list.filenames[i],1,-5),".csv"),layer_options = "GEOMETRY=AS_XY")
}

test <- read.csv("cropped_daily_WIND_2015.csv")
table(test$Local.Site.Name)


list.files(pattern = regex("^cropped.*.WIND.*.csv$"))
list.filenames <- list.files(pattern = regex("^cropped.*.WIND.*.csv$"))
list.filenames
i<-1
EPA.dailywind.check.summarized <- NULL
for(i in 1:length(list.filenames)){
  print(list.filenames[i])
  EPA.dailywind.check <- read.csv(list.filenames[i])
  EPA.dailywind.check$year <- year(EPA.dailywind.check$Date.Local)
  EPA.dailywind.check <- EPA.dailywind.check %>% group_by(Local.Site.Name) %>% summarise(available = length(unique(Date.Local)),numberobs = n(),year=mean(year))
  EPA.dailywind.check.summarized <- rbind(EPA.dailywind.check.summarized,EPA.dailywind.check)
}

EPA.dailywind.check.summarized.2 <- EPA.dailywind.check.summarized %>% group_by(year) %>% 
  summarise(available = mean(available),sites=length(Local.Site.Name),
            site.names=paste(unique(Local.Site.Name),collapse=", "))

####MAP NOAA + EPA####
tm_shape(cropLongLat5c_small)+
  tm_borders()+
tm_shape(EPA.dailytemp.sf)+
  tm_dots(col="Method.Name",palette="red",title="EPA Sites",label = "",size = 0.08)+
tm_shape(CDOdaily.temps.sf)+
  tm_dots(col="Agency",palette = "blue",title = "NOAA Sites",label = "",size = 0.08)


tm_shape(cropLongLat5c_small)+
  tm_borders()+
tm_shape(EPA.dailytemp.sf)+
  tm_dots(col="Method.Name",palette="red",title="EPA Sites",label = "",size = 0.08)+
tm_shape(CDOdaily.temps.available.sf)+
  tm_dots(col="Agency",palette = "blue",title = "NOAA Sites",label = "",size = 0.08)

####Combined NOAA+CDO+EPA Availability####
colnames(NOAAdaily.temps.summary)
colnames(CDOdaily.temps.summary)
colnames(EPA.dailytemp.check.summarized)
colnames(CDOdaily.temps.available.summary)
#EPA.dailytemp.check.summarized <- EPA.dailytemp.check.summarized %>% dplyr::select(-(numberobs))
colnames(EPA.dailytemp.check.summarized) <- c("STATION","available","year","missing")
CombinedDaily.temps.summary <- bind_rows(EPA.dailytemp.check.summarized,CDOdaily.temps.summary)

CombinedDaily.temps.summary.2 <- CombinedDaily.temps.summary %>% group_by(year) %>% 
                                      summarise(mean.available = mean(available),
                                      max.available = max(available),
                                      mean.missing = mean(missing),
                                      percent.missing = (mean(missing)/year.length(mean(year)))*100,
                                      sites=length(STATION),
                                      site.names=paste(unique(STATION),collapse=", "))

#CombinedDaily.temps.summary.2$sites-2 == CDOdaily.temps.summary2$sites
CombinedDaily.temps.available.summary <- bind_rows(EPA.dailytemp.check.summarized,CDOdaily.temps.available.summary)

CombinedDaily.temps.available.summary.2 <- CombinedDaily.temps.available.summary %>% group_by(year) %>% 
  summarise(mean.available = mean(available),
            max.available = max(available),
            mean.missing = mean(missing),
            percent.missing = (mean(missing)/year.length(mean(year)))*100,
            sites=length(STATION),
            site.names=paste(unique(STATION),collapse=", "))
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research")
getwd()
write.csv(CombinedDaily.temps.available.summary.2, "Station Summary (Filtered).csv")
write.csv(CombinedDaily.temps.summary.2,"Station Summary (Raw).csv")


####Compare DayMet vs Ground: ATLANTA HARTSFIELD JACKSON INTERNATIONAL AIRPORT, GA US####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
CDOdaily <- read.csv("Five Counties NOAA CDO Daily Weather.csv")
table(CDOdaily$NAME)
HartsfieldDaily <- CDOdaily %>% filter(NAME == "ATLANTA HARTSFIELD JACKSON INTERNATIONAL AIRPORT, GA US")
HartsfieldDaily.sf <- st_as_sf(HartsfieldDaily, coords = c("LONGITUDE","LATITUDE"),crs=4269)
st_crs(HartsfieldDaily.sf)

HartsfieldDaily <- HartsfieldDaily %>% filter(!(DATE %in% c("2004-12-31","2008-12-31","2012-12-31","2016-12-31","2020-12-31")))

setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
#list.files(pattern="^daymet.*\\.RDS$")
#list.filenames <- list.files(pattern="^daymet.*\\.RDS$")
list.files(pattern=regex('[2][0][0][3-9]|[2][0][1][0-9]|[2][0][2][0-2]'))
list.filenames <- list.files(pattern=regex('[2][0][0][3-9]|[2][0][1][0-9]|[2][0][2][0-2]'))


HartfieldDaymet.compare <- matrix(data=NA,nrow=0,ncol=4)
HartfieldDaymet.compare <- as.data.frame(HartfieldDaymet.compare)
colnames(HartfieldDaymet.compare) <- c("Ground","DayMet","Ground Date","DayMet Date")


for(i in 1:length(list.filenames)){
daymettest <- readRDS(list.filenames[i])
daymettest <- as.data.frame(daymettest)
#print(i)
daymetdate <- str_sub(list.filenames[i],8,17)
#print(i)
#daymettest.sf <- st_as_sf(daymettest, coords = c("Longitude","Latitude"),crs = 4269)
#print(i)
# tm_shape(daymettest.sf)+
#   tm_dots(col = "tmean")
Grounddate <- HartsfieldDaily[i,"DATE"]
#print(i)
#Grounddate == daymetdate

#nearestpointtest <- st_nearest_feature(HartsfieldDaily.sf[i,],daymettest.sf)
#print(i)
#nearestpointtest
#print(i)
#print(i)
# daymettest.sf[6826,9]

newdata <- c(HartsfieldDaily[i,"TAVG"],daymettest[6826,"tmean"],Grounddate,daymetdate)
#print(i)
HartfieldDaymet.compare<-rbind(HartfieldDaymet.compare,newdata)
print(i)
}

HartfieldDaymet.compare$Ground <- as.numeric(HartfieldDaymet.compare$Ground)
HartfieldDaymet.compare$DayMet <- as.numeric(HartfieldDaymet.compare$DayMet)
colnames(HartfieldDaymet.compare) <- c("Ground","DayMet","Ground Date","DayMet Date")
# HartfieldDaymet.compare <- HartfieldDaymet.compare %>% mutate(Ground.date = as.Date(`Ground Date`, origin = as.Date("1960-01-01")), 
#                                                               DayMet.date = as.Date(`DayMet Date`, origin = as.Date("1960-01-01")))
setwd("/Users/ethan_j_li/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
write.csv(HartfieldDaymet.compare,"Comparison of DayMet vs Hartsfield Jackson Obs.csv")



library(Hmisc)
plot(HartfieldDaymet.compare$Ground,HartfieldDaymet.compare$DayMet)
cor(HartfieldDaymet.compare$Ground,HartfieldDaymet.compare$DayMet)

rcorr(HartfieldDaymet.compare$Ground,HartfieldDaymet.compare$DayMet)
sum(is.na(HartfieldDaymet.compare$Ground))
sum(is.na(HartsfieldDaily$TAVG))
6570-2798
HartfieldDaymet.compare.noNA <- HartfieldDaymet.compare %>% filter(!(is.na(Ground)))

class(HartfieldDaymet.compare.noNA$Ground)


model <- lm(HartfieldDaymet.compare.noNA$Ground~HartfieldDaymet.compare.noNA$DayMet,data=HartfieldDaymet.compare.noNA)
summary(model)



####Compare DayMet vs Ground: ATLANTA DEKALB PEACHTREE AIRPORT, GA US####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
CDOdaily <- read.csv("Five Counties NOAA CDO Daily Weather.csv")
table(CDOdaily$NAME)
PeachtreeDaily <- CDOdaily %>% filter(NAME == "ATLANTA DEKALB PEACHTREE AIRPORT, GA US")
PeachtreeDaily.sf <- st_as_sf(PeachtreeDaily, coords = c("LONGITUDE","LATITUDE"),crs=4269)
st_crs(PeachtreeDaily.sf)

PeachtreeDaily <- PeachtreeDaily %>% filter(!(DATE %in% c("2004-12-31","2008-12-31","2012-12-31","2016-12-31","2020-12-31")))
PeachtreeDaily.replacement <- as.data.frame(cbind(PeachtreeDaily$TMIN,PeachtreeDaily$TMAX))
colnames(PeachtreeDaily.replacement) <- c("TMIN","TMAX")
PeachtreeDaily.replacement$TMEAN <- rowMeans(PeachtreeDaily.replacement)
PeachtreeDaily$REPLACEMENT <- PeachtreeDaily.replacement$TMEAN

for(i in 1:nrow(PeachtreeDaily)){
  PeachtreeDaily[i,"TAVG"] <- ifelse(is.na(PeachtreeDaily[i,"TAVG"]),PeachtreeDaily[i,"REPLACEMENT"],PeachtreeDaily[i,"TAVG"])
}

setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
#list.files(pattern="^daymet.*\\.RDS$")
#list.filenames <- list.files(pattern="^daymet.*\\.RDS$")
list.files(pattern=regex('[2][0][0][3-9]|[2][0][1][0-9]|[2][0][2][0-2]'))
list.filenames <- list.files(pattern=regex('[2][0][0][3-9]|[2][0][1][0-9]|[2][0][2][0-2]'))
list.filenames
list.filenames <- list.filenames[!list.filenames==("daymet.2012-02-28.RDS")]

PeachtreeDaymet.compare <- matrix(data=NA,nrow=0,ncol=4)
PeachtreeDaymet.compare <- as.data.frame(PeachtreeDaymet.compare)
colnames(PeachtreeDaymet.compare) <- c("Ground","DayMet","Ground Date","DayMet Date")

i<-1
daymettest <- readRDS(list.filenames[i])
daymettest <- as.data.frame(daymettest)
daymettest.sf <- st_as_sf(daymettest, coords = c("Longitude","Latitude"),crs = 4269)
st_crs(daymettest.sf)
nearestpointtest <- st_nearest_feature(PeachtreeDaily.sf[i,],daymettest.sf)
nearestpointtest #3666


rm(i)

for(i in 1:length(list.filenames)){
  daymettest <- readRDS(list.filenames[i])
  daymettest <- as.data.frame(daymettest)
  #print(i)
  daymetdate <- str_sub(list.filenames[i],8,17)
  #print(i)
  #daymettest.sf <- st_as_sf(daymettest, coords = c("Longitude","Latitude"),crs = 4269)
  #print(i)
  # tm_shape(daymettest.sf)+
  #   tm_dots(col = "tmean")
  Grounddate <- PeachtreeDaily[i,"DATE"]
  #print(i)
  #Grounddate == daymetdate
  
  #nearestpointtest <- st_nearest_feature(HartsfieldDaily.sf[i,],daymettest.sf)
  #print(i)
  #nearestpointtest
  #print(i)
  #print(i)
  # daymettest.sf[6826,9]
  
  newdata <- c(PeachtreeDaily[i,"TAVG"],daymettest[3666,"tmean"],Grounddate,daymetdate)
  #print(i)
  PeachtreeDaymet.compare<-rbind(PeachtreeDaymet.compare,newdata)
  print(i)
}

colnames(PeachtreeDaymet.compare) <- c("Ground","DayMet","Ground Date","DayMet Date")
PeachtreeDaymet.compare$Ground <- as.numeric(PeachtreeDaymet.compare$Ground)
PeachtreeDaymet.compare$DayMet <- as.numeric(PeachtreeDaymet.compare$DayMet)

table(PeachtreeDaymet.compare$`Ground Date`==PeachtreeDaymet.compare$`DayMet Date`)

# HartfieldDaymet.compare <- HartfieldDaymet.compare %>% mutate(Ground.date = as.Date(`Ground Date`, origin = as.Date("1960-01-01")), 
#                                                               DayMet.date = as.Date(`DayMet Date`, origin = as.Date("1960-01-01")))
setwd("/Users/ethan_j_li/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
write.csv(PeachtreeDaymet.compare,"Comparison of DayMet vs Peachtree Airport Obs.csv")



library(Hmisc)
plot(PeachtreeDaymet.compare$Ground,PeachtreeDaymet.compare$DayMet)
cor(PeachtreeDaymet.compare$Ground,PeachtreeDaymet.compare$DayMet)

rcorr(PeachtreeDaymet.compare$Ground,PeachtreeDaymet.compare$DayMet)
sum(is.na(PeachtreeDaymet.compare$Ground))
sum(is.na(PeachtreeDaily$TAVG))
6570-2798
PeachtreeDaymet.compare.noNA <- PeachtreeDaymet.compare %>% filter(!(is.na(Ground)))

class(PeachtreeDaymet.compare.noNA$Ground)


model <- lm(PeachtreeDaymet.compare.noNA$Ground~PeachtreeDaymet.compare.noNA$DayMet,data=PeachtreeDaymet.compare.noNA)
summary(model)




####Compare DayMet vs Ground: South DeKalb EPA####
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/NOAA Daily Weather Data")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/EPA Daily Weather Data/TEMPERATURE")
EPAdaily <- NULL
list.files(pattern=regex('^cropped.*.[2][0][0][4-9].*\\.csv|^cropped.*.[2][0][1][0-9].*\\.csv|^cropped.*.[2][0][2][0-2].*\\.csv'))
list.filenames <- list.files(pattern=regex('^cropped.*.[2][0][0][4-9].*\\.csv|^cropped.*.[2][0][1][0-9].*\\.csv|^cropped.*.[2][0][2][0-2].*\\.csv'))
list.filenames
for(i in 1:length(list.filenames)){
  EPAdaily <- rbind(EPAdaily,read.csv(list.filenames[i]))
}
#EPAdaily <- read.csv("cropped_daily_TEMP_2004.csv")

SouthDeKalbdaily <- EPAdaily %>% filter(Local.Site.Name == "South DeKalb")
SouthDeKalbdaily.sf <- st_as_sf(SouthDeKalbdaily, coords = c("X","Y"),crs=4269)
st_crs(SouthDeKalbdaily.sf)

SouthDeKalbdaily <- SouthDeKalbdaily %>% filter(!(Date.Local %in% c("2004-12-31","2008-12-31","2012-12-31","2016-12-31","2020-12-31")))
#JonesboroDaily.replacement <- as.data.frame(cbind(JonesboroDaily$TMIN,JonesboroDaily$TMAX))
#colnames(JonesboroDaily.replacement) <- c("TMIN","TMAX")
#JonesboroDaily.replacement$TMEAN <- rowMeans(JonesboroDaily.replacement)
#JonesboroDaily$REPLACEMENT <- JonesboroDaily.replacement$TMEAN

# for(i in 1:nrow(JonesboroDaily)){
#   JonesboroDaily[i,"TAVG"] <- ifelse(is.na(JonesboroDaily[i,"TAVG"]),JonesboroDaily[i,"REPLACEMENT"],JonesboroDaily[i,"TAVG"])
# }

setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
#list.files(pattern="^daymet.*\\.RDS$")
#list.filenames <- list.files(pattern="^daymet.*\\.RDS$")
list.files(pattern=regex('[2][0][0][3-9]|[2][0][1][0-9]|[2][0][2][0-2]'))
list.filenames <- list.files(pattern=regex('[2][0][0][3-9]|[2][0][1][0-9]|[2][0][2][0-2]'))
list.filenames[str_sub(list.filenames,8,17)%in% SouthDeKalbdaily$Date.Local]
list.filenames <- list.filenames[str_sub(list.filenames,8,17)%in% SouthDeKalbdaily$Date.Local]


i<-1
daymettest <- readRDS(list.filenames[i])
daymettest <- as.data.frame(daymettest)
daymettest.sf <- st_as_sf(daymettest, coords = c("Longitude","Latitude"),crs = 4269)
st_crs(daymettest.sf)
nearestpointtest <- st_nearest_feature(SouthDeKalbdaily.sf[i,],daymettest.sf)
nearestpointtest #5853


rm(i)


SouthDeKalbDaymet.compare <- matrix(data=NA,nrow=0,ncol=4)
SouthDeKalbDaymet.compare <- as.data.frame(SouthDeKalbDaymet.compare)
colnames(SouthDeKalbDaymet.compare) <- c("Ground","DayMet","Ground Date","DayMet Date")


for(i in 1:length(list.filenames)){
  daymettest <- readRDS(list.filenames[i])
  daymettest <- as.data.frame(daymettest)
  #print(i)
  daymetdate <- str_sub(list.filenames[i],8,17)
  #print(i)
  #daymettest.sf <- st_as_sf(daymettest, coords = c("Longitude","Latitude"),crs = 4269)
  #print(i)
  # tm_shape(daymettest.sf)+
  #   tm_dots(col = "tmean")
  Grounddate <- SouthDeKalbdaily[i,"Date.Local"]
  #print(i)
  #Grounddate == daymetdate
  
  #nearestpointtest <- st_nearest_feature(HartsfieldDaily.sf[i,],daymettest.sf)
  #print(i)
  #nearestpointtest
  #print(i)
  #print(i)
  # daymettest.sf[6826,9]
  
  newdata <- c(SouthDeKalbdaily[i,"Arithmetic.Mean"],daymettest[5853,"tmean"],Grounddate,daymetdate)
  #print(i)
  SouthDeKalbDaymet.compare<-rbind(SouthDeKalbDaymet.compare,newdata)
  print(i)
}

colnames(SouthDeKalbDaymet.compare) <- c("Ground","DayMet","Ground Date","DayMet Date")
SouthDeKalbDaymet.compare$Ground <- as.numeric(SouthDeKalbDaymet.compare$Ground)
SouthDeKalbDaymet.compare$DayMet <- as.numeric(SouthDeKalbDaymet.compare$DayMet)
SouthDeKalbDaymet.compare$Ground <- (SouthDeKalbDaymet.compare$Ground-32)*5/9

table(SouthDeKalbDaymet.compare$`Ground Date`==SouthDeKalbDaymet.compare$`DayMet Date`)

# HartfieldDaymet.compare <- HartfieldDaymet.compare %>% mutate(Ground.date = as.Date(`Ground Date`, origin = as.Date("1960-01-01")), 
#                                                               DayMet.date = as.Date(`DayMet Date`, origin = as.Date("1960-01-01")))
setwd("/Users/ethan_j_li/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
write.csv(SouthDeKalbDaymet.compare,"Comparison of DayMet vs South DeKalb Obs.csv")



library(Hmisc)
plot(SouthDeKalbDaymet.compare$Ground,SouthDeKalbDaymet.compare$DayMet)
cor(SouthDeKalbDaymet.compare$Ground,SouthDeKalbDaymet.compare$DayMet)

rcorr(SouthDeKalbDaymet.compare$Ground,SouthDeKalbDaymet.compare$DayMet)
sum(is.na(SouthDeKalbDaymet.compare$Ground))
sum(is.na(SouthDeKalbdaily$Arithmetic.Mean))
6570-2798
SouthDeKalbDaymet.compare.noNA <- SouthDeKalbDaymet.compare %>% filter(!(is.na(Ground)))

class(SouthDeKalbDaymet.compare.noNA$Ground)


model <- lm(SouthDeKalbDaymet.compare.noNA$Ground~SouthDeKalbDaymet.compare.noNA$DayMet,data=SouthDeKalbDaymet.compare.noNA)
summary(model)



####USGS Elevation####
setwd("/Users/ethan_j_li/Library/CloudStorage/OneDrive-EmoryUniversity/Liu Group Research/USGS Elevation Data")
list.files()
Elevation <- raster("30n090w_20101117_gmted_mea150.tif")
crs(Elevation)
Elevation <- crop(Elevation,cropLongLat5c_small)
plot(Elevation)
Elevation <- raster::mask(Elevation,cropLongLat5c_small)
plot(Elevation)

maxElevation <-raster("30n090w_20101117_gmted_max150.tif")
crs(maxElevation)
maxElevation <- crop(maxElevation,cropLongLat5c_small)
plot(maxElevation)
maxElevation <- raster::mask(maxElevation,cropLongLat5c_small)
plot(maxElevation) 

setwd("/Users/ethan_j_li/Library/CloudStorage/OneDrive-EmoryUniversity/Liu Group Research/USGS Elevation Data/GMTED2010N30W090_300")
list.files()
Elevation1km <- raster("30n090w_20101117_gmted_mea300.tif")
res(Elevation1km)
res(Elevation)


####Process MCD12A1 Land Use Data####

setwd(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"))
list.files()
list.filenames <- list.files(pattern = regex("^MCD12Q1.*\\.hdf$"))
list.filenames
list.filenames.dates <- paste0(substr(list.filenames,10,13),"_",substr(list.filenames,14,16), "_",substr(list.filenames,18,20))
list.filenames.dates
list.filenames.dates <- substr(list.filenames.dates,1,8)
table(table(list.filenames.dates))
table(list.filenames.dates)
which(table(list.filenames.dates)==1)
which(table(list.filenames.dates)==3)
which(table(list.filenames.dates)==4)


#landusecrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
landusecrs <- "PROJCS[\"Sinusoidal\", GEOGCS[\"GCS_unnamed ellipse\",
                            DATUM[\"D_unknown\", SPHEROID[\"Unknown\",6371007.181,0]],
                            PRIMEM[\"Greenwich\",0], UNIT[\"Degree\",0.017453292519943295]],
       PROJECTION[\"Sinusoidal\"], PARAMETER[\"central_meridian\",0],
       PARAMETER[\"false_easting\",0], PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]"
croplanduse <- st_transform(cropLongLat5c_small,crs = landusecrs)
#croplanduse <- croplanduse[,2]
crs(croplanduse)



for(index in seq(1,length(list.filenames),2)){
  a <- sds(list.filenames[index])
  b<- sds(list.filenames[index+1])
  a <- a[1]
  b <- b[1]
  # a <- mean(a,na.rm=TRUE)
  # b<-mean(b,na.rm=TRUE)
  ab<-merge(a,b)
  writeRaster(ab,paste0(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"),"/Merged ",substr(list.filenames[index],10,16)," 1km MCD12Q1 Land Use.tif"))
}


list.filenames <- list.files(pattern = regex("^Merged.*.Land Use.tif$"))
plot(raster(list.filenames[1]))
reclassoriginal <- c(1,2,3,4,5,6,7,8,9,10,12,14,11,17,13,16,15)
reclassnew <- c(rep(1,12),rep(2,2),3,4,5) #1 = Vegetaion, 2= Water, 3 Urban, 4=Barren, 5=Snow/Ice
reclassmatrix <- as.matrix(cbind(reclassoriginal,reclassnew))

for(index in 1:length(list.filenames)){
  originalraster <- raster(list.filenames[index])
  reclassraster <- reclassify(originalraster,reclassmatrix)
  writeRaster(reclassraster,paste0(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"),"/Reclassified ",list.filenames[index]))
}

landtypes <- c("Vegetation","Water","Urban","Barren","Snow/Ice")
Nratio <- as.integer(1000/463.3127)
list.filenames <- list.files(pattern = regex("^Reclassified.*.Land Use.tif$"))

for(index in 1:length(list.filenames)){
  originalraster <- raster(list.filenames[index])
  
  for(i in min(values(originalraster)):max(values(originalraster))){
    
    landpercent <- raster::aggregate(originalraster, Nratio, fun=function(x, na.rm=T) {as.numeric(mean(x==i, na.rm=T)*100)})
    writeRaster(landpercent,paste0(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"),"/Percentage ",
                                   landtypes[i]," ", substr(list.filenames[index],21,27)," 1km MCD12Q1 Land Use.tif"))
    print(landtypes[i])
    
  }
  
 # ab <- project(ab,"+proj=longlat +datum=WGS84 +no_defs")
 # writeRaster(ab,paste0(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"),"/Cropped ",substr(list.filenames[i],10,16)," 1km MCD12Q1 Land Use.tif"))
  print(index)
}

veg <- raster("Percentage Vegetation 2003001 1km MCD12Q1 Land Use.tif")
water <- raster("Percentage Water 2003001 1km MCD12Q1 Land Use.tif")
urban <- raster("Percentage Urban 2003001 1km MCD12Q1 Land Use.tif")
barren <- raster("Percentage Barren 2003001 1km MCD12Q1 Land Use.tif")


ATL_1km_grid <- raster(resolution=c(1000,1000),crs=proj4string(veg),ext=extent(veg))
list.filenames <- list.files(pattern = regex("^Percentage.*.Land Use.tif$"))

for(index in 1:length(list.filenames)){
  originalraster <- raster(list.filenames[index])
  originalraster.1km <- resample(originalraster,ATL_1km_grid,method = "ngb")
  originalraster.1km <- crop(originalraster.1km,croplanduse)
  writeRaster(originalraster.1km,paste0(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"),"/Cropped ", list.filenames[index]))
}

veg <- raster("Cropped Percentage Vegetation 2003001 1km MCD12Q1 Land Use.tif")
urban <- raster("Cropped Percentage Urban 2018001 1km MCD12Q1 Land Use.tif")
  
####AOD####
setwd("H:/Raw Atlanta AOD")
list.files()
a <-sds(list.files()[1])
a[2]
crs(a[2])


#setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research")
list.files()
AODfilenames <- list.files()
AODfilenames
AODfilenames.dates <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames.dates
AODfilenames.dates <- substr(AODfilenames,1,8)
table(table(AODfilenames.dates))
table(AODfilenames.dates)
which(table(AODfilenames.dates)==1)
which(table(AODfilenames.dates)==3)
which(table(AODfilenames.dates)==4)



aodcrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"Sinusoidal\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
cropaod <- st_read(here::here("Data","ATL_shps_for_April","five_counties.shp"))
#st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Data/ATL_shps_for_April/five_counties.shp")
cropaod <- st_transform(cropaod,crs = aodcrs)
cropaod <- cropaod[,9]
crs(cropaod)

rm(i)

for(i in seq(1,length(AODfilenames),2)){
  a <- sds(AODfilenames[i])
  b<- sds(AODfilenames[i+1])
  a <- a[2]
  b <- b[2]
  a <- mean(a,na.rm=TRUE)
  b<-mean(b,na.rm=TRUE)
  ab<-merge(a,b)
  ab <- crop(ab,cropaod)
  writeRaster(ab,paste0(here::here("Data","Cropped Atlanta AOD"),"/Cropped ",substr(AODfilenames[i],10,16)," Atlanta AOD.tif"))
  print(i)
}

originalraster <- raster(here::here("Data","Cropped Atlanta AOD","Cropped 2013174 Atlanta AOD.tif"))
originalraster
crs(originalraster)


setwd(here::here("Data","Cropped Atlanta AOD"))
ATL_1km_grid <- raster(resolution=c(1000,1000),crs=proj4string(originalraster),ext=extent(originalraster))
list.filenames <- list.files(pattern = regex("^Cropped.*.AOD.tif$"))

for(index in 1:length(list.filenames)){
  originalraster <- raster(list.filenames[index])
  originalraster.1km <- resample(originalraster,ATL_1km_grid,method = "ngb")
  writeRaster(originalraster.1km,paste0(here::here("Data","1km Cropped Atlanta AOD"),"/1km Cropped ", list.filenames[index]))
  print(index)
}


#Check extent of AOD and Land Use
AOD <- raster(here::here("Data","1km Cropped Atlanta AOD","1km Cropped Cropped 2002002 Atlanta AOD.tif"))
Landuse <- raster(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31","Cropped Percentage Urban 2013001 1km MCD12Q1 Land Use.tif"))

ext(AOD)
ext(Landuse)
res(AOD)
res(Landuse)


####Speciation Data####

speciation.2021 <- read.csv(here::here("Data","PM2.5 Speciation","daily_SPEC_2021.csv"))
table(speciation.2021$Parameter.Name)
table(speciation.2021$Local.Site.Name)
table(speciation.2021$State.Name)
table(speciation.2021$Units.of.Measure)
speciation.2021 <- speciation.2021 %>% filter(Units.of.Measure == "Micrograms/cubic meter (LC)")
table(speciation.2021$Parameter.Name)

speciation.2021.sf <- st_as_sf(speciation.2021,coords=c("Longitude","Latitude"),crs=4269)
speciation.2021.sf.locations <- speciation.2021.sf %>% filter(!duplicated(geometry))
tm_shape(speciation.2021.sf.locations)+
  tm_dots()


speciation.2021$Arithmetic.Mean <- ifelse(speciation.2021$Arithmetic.Mean<0,NA,speciation.2021$Arithmetic.Mean)
sum(is.na(speciation.2021$Arithmetic.Mean))
speciation.2021.availability.1 <- speciation.2021 %>% group_by(Site.Num,Date.Local) %>% summarize(missing=sum(is.na(Arithmetic.Mean)))


speciation.2021.availability.2 <- speciation.2021 %>% mutate(year = year(Date.Local))
#sum(!is.na(CDOdaily.temps.available$TAVG) | !is.na(CDOdaily.temps.available$TMAX) | !is.na(CDOdaily.temps.available$TMIN))

#speciation.2021.availability.2 <- speciation.2021.availability.2 %>% group_by(year,STATION) %>% summarize(available = sum(!is.na(TAVG) | !is.na(TMAX) | !is.na(TMIN)),
#                                                                                                      year = mean(year)
#)

speciation.2021.availability.2 <- speciation.2021.availability.2 %>% group_by(Site.Num,Parameter.Name) %>% summarize(available = sum(!is.na(Arithmetic.Mean)),year=mean(year))

speciation.2021.availability.2 <- speciation.2021.availability.2 %>% mutate(missing = year.length(year)-available)

speciation.2021.availability.3 <- speciation.2021.availability.2 %>% summarize(     
  mean.available = mean(available),
  #max.available = max(available),
  mean.missing = mean(missing),
  percent.missing = (mean(missing)/year.length(mean(year)))*100,
  sites = length(unique(Site.Num)),
  year = mean(year)
 # site.names = paste(unique(STATION),collapse= ", ")
)


#CDOdaily.temps.available.sf <- st_as_sf(CDOdaily.temps.available,coords = c("LONGITUDE","LATITUDE"),crs =st_crs("+proj=longlat +datum=WGS84 +no_defs") )
#CDOdaily.temps.available.sf <- CDOdaily.temps.available.sf %>% filter(!duplicated(geometry))
#CDOdaily.temps.available.sf$Agency <- "NOAA CDO"


#####MISR#####
#install.packages("ncdf4")
#library(ncdf4)
us_states <- states()
st_crs(us_states)
us_states.CONUS <- us_states %>% dplyr::filter(!(NAME %in% c("United States Virgin Islands","Commonwealth of the Northern Mariana Islands",
                                                      "American Samoa","Guam","Hawaii","Puerto Rico","Alaska")))%>% dplyr::select(NAME,geometry)

tm_shape(us_states.CONUS)+
  tm_borders()

st_crs(us_states.CONUS)
st_bbox(us_states.CONUS)  #xmin       ymin       xmax       ymax 
                          #-124.84897   24.39631  -66.88544   49.38448 

us_states.HI <- us_states %>% dplyr::filter(NAME %in% c("Hawaii"))%>% dplyr::select(NAME,geometry)
st_bbox(us_states.HI) #xmin       ymin       xmax       ymax 
                      #-178.44359   18.86546 -154.75579   28.51727 

us_states.AK <- us_states %>% dplyr::filter(NAME %in% c("Alaska"))%>% dplyr::select(NAME,geometry)
tm_shape(us_states.AK)+
  tm_borders()
st_bbox(us_states.AK) # xmin       ymin       xmax       ymax 
                      #-179.23109   51.17509  179.85968   71.43979


us_states.PR <- us_states %>% dplyr::filter(NAME %in% c("Puerto Rico"))%>% dplyr::select(NAME,geometry)
st_bbox(us_states.PR)  # xmin      ymin      xmax      ymax 
                        #-67.99875  17.83151 -65.16850  18.56800 

#CONUS BB
# CONUS <- st_read(here::here("Data","ATL_shps_for_April","US States Shapefile","US States Shapefile.shp"))
# plot(CONUS)
# st_bbox(CONUS)
# tm_shape(CONUS)+
#   tm_borders()


#Path 18
P18.dates <- seq.Date(as.Date("2003-01-01"),as.Date("2022-02-22"),by=16)

MISR.P18 <- nc_open(here::here("Data","MISR Data","MISR_AM1_AS_AEROSOL_P044_O118079_F13_0023.nc"))
{
    sink('P_018_O117990.txt')
  print(MISR.P18)
    sink()
}

names(MISR.P18)
MISR.P18[[8]]
MISR.P18

names(MISR.P18$var)

MISR.P18.variables <- MISR.P18$var
MISR.P18.variables$`4.4_KM_PRODUCTS/Year`

library(mapchina)
china <- mapchina::china
test_grid <- read.csv(here::here("Python","own code","test_grid_mean_compare.csv"))
test_grid.sf <- st_as_sf(test_grid, coords=c("Lon","Lat"),crs=4326)
tm_shape(china)+
  tm_borders()+
tm_shape(test_grid.sf)+
  tm_dots()

plot(test_grid.sf["aod"])


#Test grid
own_test_grid <- read.csv(here::here("Python","own code","Test Output","2022227P037_O118064MISR Grid IDs.csv"))
own_test_grid.sf <- st_as_sf(own_test_grid,coords = c("Lon","Lat"),crs = 4269)
tm_shape(own_test_grid.sf)+
  tm_dots()+
tm_shape(us_states.CONUS)+
  tm_borders()

#Test AOD
own_test_AOD <- read.csv(here::here("Python","own code","Test Output","2022227P037_O118064MISR AOD.csv"))
own_test_AOD.sf <- st_as_sf(own_test_AOD, coords = c("Lon","Lat"),crs=4269)
tm_shape(us_states.CONUS)+
  tm_borders()+
tm_shape(own_test_AOD.sf)+
  tm_dots(col="aod_cal")


#Test merging AOD
list.files(here::here("Python","own code","Test Output"))
test.list.filenames <- list.files(here::here("Python","own code","Test Output"))

list.MISR.files <- list()

#read.csv(here::here("Python","own code","Test Output",test.list.filenames[1]))

for(i in 1:length(test.list.filenames)){
  list.MISR.files[[i]] <- read.csv(here::here("Python","own code","Test Output",test.list.filenames[i]))
} 

list.MISR.files[[1]]

MISR.Jan.2020 <-bind_rows(list.MISR.files)
MISR.Jan.2020 <- MISR.Jan.2020 %>%mutate(sum_components = aod_1+aod_2+aod_3+aod_6+aod_8+aod_14+aod_19+aod_21,match_check = aod_cal - sum_components)  
min(MISR.Jan.2020$match_check)
max(MISR.Jan.2020$match_check)  
MISR.Jan.2020 <- MISR.Jan.2020 %>% group_by(Lat,Lon) %>% summarise(across(everything(), list(mean)))

MISR.Jan.2020.sf <- st_as_sf(MISR.Jan.2020,coords=c("Lon","Lat"),crs = 4269)
tm_shape(us_states.CONUS)+
  tm_borders()+
tm_shape(MISR.Jan.2020.sf)+
  tm_dots(col="aod_cal_1")
st_crs(us_states.CONUS)

test <- read.csv(here::here("Python","own code","Test Output","2020.1.22.P044_O106895MISR AOD.csv"))
test.sf <- st_as_sf(test,coords = c("Lon","Lat"))
tm_shape(us_states.CONUS)+
  tm_borders()+
tm_shape(MISR.Jan.2020.sf)+
  tm_dots(col="aod_cal",breaks = quantile(MISR.Jan.2020.sf$aod_cal_1,probs=seq(0,1,0.01)))

####Build 0.01 degree x 0.01 degree Lat/Lon Grid for Continental US####
#0.0174532925199433/0.01

st_bbox(us_states.CONUS)    #  xmin       ymin       xmax       ymax 
                           #-124.84897   24.39631  -66.88544   49.38448 
st_crs(us_states.CONUS)
us_states.CONUS.ext <- ext(us_states.CONUS)
us_states.CONUS.grid<- raster(xmn = -124.85,xmx = -66.88,ymn=24.39,ymx = 49.39,crs=4269,res=c(0.01,0.01))

res(us_states.CONUS.grid)
us_states.CONUS.grid

st_crs(us_states.CONUS.grid)
us_states.CONUS.grid <- as.data.frame(rasterToPoints(us_states.CONUS.grid))
us_states.CONUS.grid$index <- rownames(us_states.CONUS.grid)
write.csv(us_states.CONUS.grid,here::here("Data","CONUS Grid.csv"))

us_states.CONUS.grid.sf <- st_as_sf(us_states.CONUS.grid,coords = c("x","y"),crs=4269)
tm_shape(us_states.CONUS.grid.sf)+
  tm_dots()+
tm_shape(us_states.CONUS)+
  tm_borders()


####Build 0.01 degree x 0.01 degree Lat/Lon Grid for Alaska####
#0.0174532925199433/0.01

st_bbox(us_states.AK)     #xmin       ymin       xmax       ymax 
                    #-179.23109   51.17509    -129.75   71.43979 
st_crs(us_states.AK)
us_states.AK.ext <- ext(us_states.AK)
us_states.AK.grid<- raster(xmn = -179.24,xmx = -129.75,ymn=51.17,ymx = 71.44,crs=4269,res=c(0.01,0.01))

res(us_states.AK.grid)
us_states.AK.grid

st_crs(us_states.AK.grid)
us_states.AK.grid <- as.data.frame(rasterToPoints(us_states.AK.grid))
us_states.AK.grid$index <- rownames(us_states.AK.grid)
write.csv(us_states.AK.grid,here::here("Data","AK Grid.csv"))

us_states.AK.grid.sf <- st_as_sf(us_states.AK.grid,coords = c("x","y"),crs=4269)
tm_shape(us_states.AK.grid.sf)+
  tm_dots()+
  tm_shape(us_states.CONUS)+
  tm_borders()

####Build 0.01 degree x 0.01 degree Lat/Lon Grid for Hawaii####
#0.0174532925199433/0.01

st_bbox(us_states.HI)           #xmin       ymin       xmax       ymax 
                          #-178.44359   18.86546 -154.75579   28.51727 
st_crs(us_states.HI)
us_states.HI.ext <- ext(us_states.HI)
us_states.HI.grid<- raster(xmn = -178.45,xmx = -154.75,ymn=18.86,ymx = 28.52,crs=4269,res=c(0.01,0.01))

res(us_states.HI.grid)
us_states.HI.grid

st_crs(us_states.HI.grid)
us_states.HI.grid <- as.data.frame(rasterToPoints(us_states.HI.grid))
us_states.HI.grid$index <- rownames(us_states.HI.grid)
write.csv(us_states.HI.grid,here::here("Data","HI Grid.csv"))

us_states.HI.grid.sf <- st_as_sf(us_states.HI.grid,coords = c("x","y"),crs=4269)
tm_shape(us_states.HI.grid.sf)+
  tm_dots()+
  tm_shape(us_states.CONUS)+
  tm_borders()

####Build 0.01 degree x 0.01 degree Lat/Lon Grid for PR####
#0.0174532925199433/0.01

st_bbox(us_states.PR)     #xmin      ymin      xmax      ymax 
                      #-67.99875  17.83151 -65.16850  18.56800 
st_crs(us_states.PR)
us_states.PR.ext <- ext(us_states.PR)
us_states.PR.grid<- raster(xmn = -68.00,xmx = -65.16,ymn=17.83,ymx = 18.57,crs=4269,res=c(0.01,0.01))

res(us_states.PR.grid)
us_states.PR.grid

st_crs(us_states.PR.grid)
us_states.PR.grid <- as.data.frame(rasterToPoints(us_states.PR.grid))
us_states.PR.grid$index <- rownames(us_states.PR.grid)
write.csv(us_states.PR.grid,here::here("Data","PR Grid.csv"))

us_states.PR.grid.sf <- st_as_sf(us_states.PR.grid,coords = c("x","y"),crs=4269)
tm_shape(us_states.PR.grid.sf)+
  tm_dots()+
  tm_shape(us_states.CONUS)+
  tm_borders()


#####Map EPA Data#####
#######Create combined EPA Speciation Data file#####
#PM_speciation <- read.csv(here::here("Data","PM2.5 Speciation",list.filenames[[file]]))

list.files(here::here("Data","PM2.5 Speciation"))[grepl("^daily_SPEC.*..csv$",list.files(here::here("Data","PM2.5 Speciation")))]
list.filenames <- list.files(here::here("Data","PM2.5 Speciation"))[grepl("^daily_SPEC.*..csv$",list.files(here::here("Data","PM2.5 Speciation")))]
PM_speciation_list <- list()

for(file in 1:length(list.filenames)){
  PM_speciation <- read.csv(here::here("Data","PM2.5 Speciation",list.filenames[[file]]))
  PM_speciation <- PM_speciation %>% dplyr::select(Latitude,Longitude,Parameter.Name,Date.Local,Units.of.Measure,Arithmetic.Mean,Local.Site.Name,Address,State.Name)
  PM_speciation_list[[file]] <- PM_speciation
  print(file)
}

PM_speciation_reduced <- bind_rows(PM_speciation_list)
table(PM_speciation_reduced$Parameter.Name)
PM_speciation_reduced <- PM_speciation_reduced %>% 
  filter(Parameter.Name %in% c("Total Nitrate PM2.5 LC",
                               "Sulfate PM2.5 LC",
                               "OC PM2.5 LC TOR",
                               "EC PM2.5 LC TOR",
                               "Reconstructed Mass PM2.5 LC")) %>%
  mutate(Arithmetic.Mean = case_when(Arithmetic.Mean < 0 ~ 0,
                                     Arithmetic.Mean >=0 ~ Arithmetic.Mean))

saveRDS(object = PM_speciation_reduced,file = here::here("Data","PM2.5 Speciation","Aggregated Speciation 2000-2022.rds"))


#######Mapping#######

list.files(here::here("Data","PM2.5 Speciation"))
file.exists(here::here("Data","PM2.5 Speciation","Aggregated Speciation 2000-2022.rds"))
speciation_file <- here::here("Data","PM2.5 Speciation","Aggregated Speciation 2000-2022.rds")
PM_speciation_reduced_new <- readRDS(speciation_file)
#all.equal(PM_speciation_reduced,PM_speciation_reduced_new)


PM_speciation_reduced_new <- PM_speciation_reduced_new %>% 
  mutate(Season = hydroTSM::time2season(as.Date(Date.Local),out.fmt="seasons"))
PM_speciation_reduced_new$Date.Local <- as.Date(PM_speciation_reduced_new$Date.Local)
summary(PM_speciation_reduced_new)
table(PM_speciation_reduced_new$Parameter.Name)

#lubridate::month(PM_speciation_reduced_new$Date.Local,label=TRUE)
#lubridate::year(PM_speciation_reduced_new$Date.Local)

year_and_season <- function(Date,Season){
  month <- lubridate::month(Date,label=TRUE)
  year <- lubridate::year(Date)
  year <- ifelse(month %in% c("Jan","Feb"),year-1,year)
  season <- paste(year,Season)
  return(season)
}

#year_and_season(PM_speciation_reduced_new$Date.Local,PM_speciation_reduced_new$Season)
PM_speciation_reduced_new <- PM_speciation_reduced_new %>% mutate(Season.Year = year_and_season(Date.Local,Season))
PM_speciation_reduced_new <- PM_speciation_reduced_new %>% group_by(Latitude,Longitude,Parameter.Name,Season.Year) %>% summarize(mean = mean(Arithmetic.Mean),Units.of.Measure = unique(Units.of.Measure))
PM_speciation_reduced_new <- PM_speciation_reduced_new %>% 
  arrange(Parameter.Name,Season.Year)
PM_speciation_reduced_new.sf <- st_as_sf(PM_speciation_reduced_new,coords=c("Longitude","Latitude"),crs=4269)
#PM_speciation_reduced_new.sf <- PM_speciation_reduced_new.sf %>% dplyr::filter(!(Parameter.Name == "EC CSN PM2.5 LC TOT"))


#Create breaks for each PM2.5 species
species.breaks.list <- list()

for(i in 1:length(unique(PM_speciation_reduced_new$Parameter.Name))){
  species.breaks <- PM_speciation_reduced_new %>% 
    dplyr::filter(Parameter.Name == unique(PM_speciation_reduced_new$Parameter.Name)[i]) 
  species.breaks <- range(species.breaks$mean)
  print(unique(PM_speciation_reduced_new$Parameter.Name)[i])
  print(species.breaks)
  species.breaks.list[[i]] <- species.breaks
}
names(species.breaks.list) <- unique(PM_speciation_reduced_new$Parameter.Name)
species.breaks.list[[1]]<-c(seq(species.breaks.list[[1]][1],species.breaks.list[[1]][2],0.4), species.breaks.list[[1]][2])
species.breaks.list[[2]]<-c(seq(species.breaks.list[[2]][1],species.breaks.list[[2]][2],1), species.breaks.list[[2]][2])
species.breaks.list[[3]]<-c(seq(species.breaks.list[[3]][1],species.breaks.list[[3]][2],1), species.breaks.list[[3]][2])
species.breaks.list[[4]]<-c(seq(species.breaks.list[[4]][1],species.breaks.list[[4]][2],1), species.breaks.list[[4]][2])
species.breaks.list[[5]]<-c(seq(species.breaks.list[[5]][1],species.breaks.list[[5]][2],1), species.breaks.list[[5]][2])



speciation_list <- list()
unique(PM_speciation_reduced_new.sf$Season.Year)
unique(PM_speciation_reduced_new.sf$Parameter.Name)

for(i in 1:length(unique(PM_speciation_reduced_new.sf$Parameter.Name))){
  for(j in 1:length(unique(PM_speciation_reduced_new.sf$Season.Year))){
    PM_speciation_reduced_new.sf_subset <- PM_speciation_reduced_new.sf %>%
      dplyr::filter(Parameter.Name == unique(PM_speciation_reduced_new.sf$Parameter.Name)[i],
                    Season.Year == unique(PM_speciation_reduced_new.sf$Season.Year)[j])
    if(nrow(PM_speciation_reduced_new.sf_subset)==0) next
    PM_speciation_reduced_new.sf_subset$cat <- cut(PM_speciation_reduced_new.sf_subset$mean,breaks = species.quantile.list[[i]],labels=FALSE)
    speciation_list <- append(speciation_list,list(PM_speciation_reduced_new.sf_subset))
    print(unique(PM_speciation_reduced_new.sf$Parameter.Name)[i])
    print(unique(PM_speciation_reduced_new.sf$Season.Year)[j])
  }
}

view(speciation_list[[1]])
view(speciation_list[[210]])
colnames(speciation_list[[1]])
speciation_list_names <- c()
for(i in 1:length(speciation_list)){
  speciation_list_names <- c(speciation_list_names, paste(unique(speciation_list[[i]]$Season.Year),unique(speciation_list[[i]]$Parameter.Name)))
}

view(speciation_list[[155]])
names(speciation_list) <- speciation_list_names


us_bbox <- st_bbox(us_states)
us_bbox[1]
us_bbox[2]<-10
us_bbox[3] <- 0
us_bbox[4]
us_bbox

us_states <- st_crop(us_states,c(xmin = -179.23109,ymin = 0,xmax = -60, ymax = 71.43979))
st_crs(us_states)
st_crs(speciation_list[[1]])

tm_shape(us_states)+
  tm_borders()+
tm_shape(speciation_list[[120]])+
  tm_dots(col = "mean",
          size=0.4,
          shape = 21,
          border.col="black",
          style = "equal",
          #breaks = species.quantile.list[[2]],
          title = paste0(unique(speciation_list[[120]]$Parameter.Name),"\n",unique(speciation_list[[120]]$Units.of.Measure)))+
tm_layout(
  main.title = names(speciation_list)[120],
  main.title.position = "center"
)



for(i in 1:length(speciation_list)){
  name <- str_sub(names(speciation_list)[[i]],13,-1)
  map <- tm_shape(us_states)+
    tm_borders()+
  tm_shape(speciation_list[[i]])+
    tm_dots(col = "mean",
            size=0.4,
            shape = 21,
            border.col="black",
            style = "equal",
            #breaks =  species.quantile.list[[name]],
            title = paste0(unique(speciation_list[[i]]$Parameter.Name),"\n",unique(speciation_list[[i]]$Units.of.Measure))
            )+
  tm_layout(
    main.title = names(speciation_list)[i],
    main.title.position = "center")
  tmap_save(map,here::here("Atlanta Project","Results_New","Speciation Maps",paste0(names(speciation_list)[[i]],".png")))
}


#####Texas Mapping####
options(tigris_use_cache = TRUE)
Harris <- tigris::counties(state="Texas",cb=FALSE) %>%
  filter(NAME == "Harris")

Texas <- tigris::states() %>%
  filter(NAME == "Texas")

tm_shape(Texas)+
  tm_borders()+
  tm_shape(Harris)+
  tm_borders()


list.files(here::here("Data","PM2.5 Speciation"))
file.exists(here::here("Data","PM2.5 Speciation","Aggregated Speciation 2000-2022.rds"))
speciation_file <- here::here("Data","PM2.5 Speciation","Aggregated Speciation 2000-2022.rds")
PM_speciation_reduced_new <- readRDS(speciation_file)

PM_speciation_reduced_new <- PM_speciation_reduced_new %>% 
  mutate(Season = hydroTSM::time2season(as.Date(Date.Local),out.fmt="seasons"))
PM_speciation_reduced_new$Date.Local <- as.Date(PM_speciation_reduced_new$Date.Local)
summary(PM_speciation_reduced_new)
table(PM_speciation_reduced_new$Parameter.Name)

#lubridate::month(PM_speciation_reduced_new$Date.Local,label=TRUE)
#lubridate::year(PM_speciation_reduced_new$Date.Local)

year_and_season <- function(Date,Season){
  month <- lubridate::month(Date,label=TRUE)
  year <- lubridate::year(Date)
  year <- ifelse(month %in% c("Jan","Feb"),year-1,year)
  season <- paste(year,Season)
  return(season)
}

#year_and_season(PM_speciation_reduced_new$Date.Local,PM_speciation_reduced_new$Season)
PM_speciation_reduced_new.sf <- PM_speciation_reduced_new %>% 
  mutate(Season.Year = year_and_season(Date.Local,Season))%>% 
  group_by(Latitude,Longitude,Parameter.Name,Season.Year) %>% 
  summarize(mean = mean(Arithmetic.Mean),Units.of.Measure = unique(Units.of.Measure))%>% 
  arrange(Parameter.Name,Season.Year) %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs=4269)
#PM_speciation_reduced_new.sf <- PM_speciation_reduced_new.sf %>% dplyr::filter(!(Parameter.Name == "EC CSN PM2.5 LC TOT"))
PM_speciation_reduced_new_Harris.sf <- st_crop(PM_speciation_reduced_new.sf,Harris)
table(PM_speciation_reduced_new_Harris.sf$Parameter.Name)


#Create breaks for each PM2.5 species
species.breaks.list <- list()

for(i in 1:length(unique(PM_speciation_reduced_new_Harris.sf$Parameter.Name))){
  species.breaks <- PM_speciation_reduced_new_Harris.sf %>% 
    dplyr::filter(Parameter.Name == unique(Parameter.Name)[i]) 
  species.breaks.bins <- seq(min(range(species.breaks$mean)),max(range(species.breaks$mean)),by = (max(range(species.breaks$mean))-min(range(species.breaks$mean)))/4)
  print(unique(PM_speciation_reduced_new_Harris.sf$Parameter.Name)[i])
  print(species.breaks.bins)
  species.breaks.list[[i]] <- species.breaks.bins
}
names(species.breaks.list) <- unique(PM_speciation_reduced_new_Harris.sf$Parameter.Name)
# species.breaks.list[[1]]<-c(seq(species.breaks.list[[1]][1],species.breaks.list[[1]][2],0.4), species.breaks.list[[1]][2])
# species.breaks.list[[2]]<-c(seq(species.breaks.list[[2]][1],species.breaks.list[[2]][2],1), species.breaks.list[[2]][2])
# species.breaks.list[[3]]<-c(seq(species.breaks.list[[3]][1],species.breaks.list[[3]][2],1), species.breaks.list[[3]][2])
# species.breaks.list[[4]]<-c(seq(species.breaks.list[[4]][1],species.breaks.list[[4]][2],1), species.breaks.list[[4]][2])
# species.breaks.list[[5]]<-c(seq(species.breaks.list[[5]][1],species.breaks.list[[5]][2],1), species.breaks.list[[5]][2])




speciation_list <- list()
unique(PM_speciation_reduced_new_Harris.sf$Season.Year)
unique(PM_speciation_reduced_new_Harris.sf$Parameter.Name)

for(i in 1:length(unique(PM_speciation_reduced_new_Harris.sf$Parameter.Name))){
  for(j in 1:length(unique(PM_speciation_reduced_new_Harris.sf$Season.Year))){
    
    PM_speciation_reduced_new.sf_subset <- PM_speciation_reduced_new_Harris.sf %>%
      dplyr::filter(Parameter.Name == unique(Parameter.Name)[i],
                    Season.Year == unique(Season.Year)[j])
    
    if(nrow(PM_speciation_reduced_new.sf_subset)==0) next
    
    speciation_list <- append(speciation_list,list(PM_speciation_reduced_new.sf_subset))
    print(unique(PM_speciation_reduced_new.sf$Parameter.Name)[i])
    print(unique(PM_speciation_reduced_new.sf$Season.Year)[j])
  }
}

view(speciation_list[[1]])
view(speciation_list[[200]])
colnames(speciation_list[[1]])
speciation_list_names <- c()
for(i in 1:length(speciation_list)){
  speciation_list_names <- c(speciation_list_names, paste(unique(speciation_list[[i]]$Season.Year),unique(speciation_list[[i]]$Parameter.Name)))
}

view(speciation_list[[155]])
speciation_list_names[155]
names(speciation_list) <- speciation_list_names

decennial_2020_vars <- load_variables(
  year = 2020, 
  "pl", 
  cache = TRUE
)

race_variables <- c(white = "P1_003N",
                    black = "P1_004N",
                    american_indian = "P1_005N",
                    asian = "P1_006N",
                    native_hi_pi = "P1_007N",
                    other = "P1_008N",
                    multirace = "P1_009N")

census_data <- get_decennial(
  geography = "tract",
  state = "Texas",
  county = 201,
  variables = race_variables,
  summary_var = "P1_001N",
  year = 2020,
  geometry=TRUE,
) %>%
  mutate(proportion = value/summary_value)

Harris_not_white <- census_data %>%
  filter(variable == "white") %>%
  mutate(not_white = summary_value - value,
         proportion_not_white = not_white/summary_value)

tm_shape(Harris_not_white)+
  tm_fill(col="proportion_not_white")+
  tm_borders()

for(i in 1:length(speciation_list)){
  name <- str_sub(names(speciation_list)[[i]],13,-1)
  map <- 
    tm_shape(Harris)+
      tm_borders()+
    tm_shape(Harris_not_white)+
      tm_fill(col="proportion_not_white",
              title = "Proportion\nNot White",
            pal = brewer.pal(n=5,name = "Purples"))+
      tm_borders()+
    tm_shape(speciation_list[[i]])+
      tm_dots(col = "mean",
            size=0.4,
            shape = 21,
            border.col="black",
            breaks =  species.breaks.list[[name]],
            title = paste0(unique(speciation_list[[i]]$Parameter.Name),"\n",unique(speciation_list[[i]]$Units.of.Measure))
    )+
    
    tm_layout(
      main.title = names(speciation_list)[i],
      main.title.position = "center",
      legend.text.size = 0.6,
      legend.title.size = 0.8)
  tmap_save(map,here::here("Atlanta Project","Results_New","Harris Speciation Maps",paste0(names(speciation_list)[[i]],".png")))
}


#####Texas Time Series####
time_series_list <- list()

PM_speciation_reduced_new_Texas.sf <- PM_speciation_reduced_new %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs=4269) %>%
  st_intersection(Texas)

time_series_list <- split(PM_speciation_reduced_new_Texas.sf,list(PM_speciation_reduced_new_Texas.sf$Local.Site.Name,PM_speciation_reduced_new_Texas.sf$Parameter.Name))

# for(i in 1:length(unique(PM_speciation_reduced_new_Texas.sf$Local.Site.Name))){
#   unique.site <- PM_speciation_reduced_new_Texas.sf %>%
#     filter(Local.Site.Name == unique(PM_speciation_reduced_new_Texas.sf$Local.Site.Name)[i])
#   time_series_list[[i]] <- unique.site
#   print(i)
# }
# 
# time_series_list_2 <- list()
# 
# for(i in 1:length(time_series_list)){
#   for(j in unique(time_series_list[[j]]$Parameter.Name)){
#     time_series_list_2 <- time_series_list[[i]]
#   }
# }
view(time_series_list[[3]])
summary(time_series_list[[3]])

for(i in 1:length(time_series_list)){
  time_series_list[[i]]$Season.Year <- year_and_season(time_series_list[[i]]$Date.Local,time_series_list[[i]]$Season)
}

for(i in 1:length(time_series_list)){
  time_series_list[[i]] <- time_series_list[[i]] %>% 
  group_by(Season.Year) %>% 
  summarize(mean = mean(Arithmetic.Mean),
            Units.of.Measure = unique(Units.of.Measure), 
            Local.Site.Name = unique(Local.Site.Name),
            Parameter.Name = unique(Parameter.Name),
            Address = unique(Address),
            Date.Local = median(Date.Local),
            Season = unique(Season))
}

for(i in 1:length(time_series_list)){
  if(nrow(time_series_list[[i]]) ==0) next
  time_series_plot <- 
    ggplot(data=time_series_list[[i]],aes(x=Date.Local, y=mean, group = Season, col = Season))+
    # geom_line(data=cleandata1_2,mapping=aes(x=time,y=meanPM,col=Group))+
    geom_point()+
    geom_line()+
    
    # geom_text(data=cleandata1_2,aes(x=time, y=meanPM,fill=Group,label=Location), vjust=1.6, color="black",
    #        position = position_dodge(0.9), size=3.5)+
    labs(title = paste("Time Series of", unique(time_series_list[[i]]$Parameter.Name), "at",unique(time_series_list[[i]]$Local.Site.Name)),
         y = paste0(unique(time_series_list[[i]]$Parameter.Name), " (",unique(time_series_list[[i]]$Units.of.Measure),")"),
         x = "Date")+
    # scale_color_manual(name="Legend",
    #                    labels = c("Metro Atlanta","Select ZIP Codes"),
    #                    values=c("blue","red"))+
    
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent')) #transparent legend panel
  ggsave(here::here("Atlanta Project","Results_New","Harris Speciation Time Series",paste(unique(time_series_list[[i]]$Parameter.Name), "at",unique(time_series_list[[i]]$Local.Site.Name),".png")),time_series_plot,dpi=300)
}



