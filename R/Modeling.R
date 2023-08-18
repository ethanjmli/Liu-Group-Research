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
library(hydroTSM)
#library(gifski)
library(readxl)
library(seas)
library(here)
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


landusecrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
landusecrs <- "PROJCS[\"Sinusoidal\", GEOGCS[\"GCS_unnamed ellipse\",
                            DATUM[\"D_unknown\", SPHEROID[\"Unknown\",6371007.181,0]],
                            PRIMEM[\"Greenwich\",0], UNIT[\"Degree\",0.017453292519943295]],
       PROJECTION[\"Sinusoidal\"], PARAMETER[\"central_meridian\",0],
       PARAMETER[\"false_easting\",0], PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]"
croplanduse <- st_transform(cropLongLat5c_small,crs = landusecrs)
#croplanduse <- croplanduse[,2]
crs(croplanduse)

for(i in seq(1,length(list.filenames),2)){
  a <- sds(list.filenames[i])
  b<- sds(list.filenames[i+1])
  a <- a[1]
  b <- b[1]
  # a <- mean(a,na.rm=TRUE)
  # b<-mean(b,na.rm=TRUE)
  a <- crop(a, croplanduse)
  b <- crop(b,croplanduse)
  ab<-merge(a,b)
  ab <- terra::aggregate(ab,2,"mean")
  ab <- project(ab,"+proj=longlat +datum=WGS84 +no_defs")
  writeRaster(ab,paste0(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31"),"/Cropped ",substr(list.filenames[i],10,16)," 1km MCD12Q1 Land Use.tif"))
  print(i)
}

a <- raster(here::here("Data","MCD12Q1 500m Land Use 2003_01_01-2022_12_31","Cropped 2003001 MCD12Q1 Land Use.tif"))
plot(a)
a <- crop(a,cropLongLat5c_small)
a <- mask(a,cropLongLat5c_small)
a


for(i in 1:length(list.files())){
  a <- raster(list.files()[i])
  a <- crop(a,cropLongLat5c_small)
  a <- mask(a,cropLongLat5c_small)
  writeRaster(a,paste0("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Masked AOD/Masked",list.files()[i]))
  print(i)
}

AODdates <- as.Date(as.numeric(substr(list.files(),5,7))-1,
                    origin = as.Date(paste0(substr(list.files(),1,4),"-01-01")))
table(table(AODdates))
unique(AODdates)
which(table(AODdates)>1)
which(is.na(table(AODdates)))
AODindices <- format(AODdates,"%Y-%m")

setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Masked AOD")
list.files()
AOD <- stack(list.files())

AODdates <- as.Date(as.numeric(substr(list.files(),11,13))-1,
                    origin = as.Date(paste0(substr(list.files(),7,10),"-01-01")))
AODyears <- format(AODdates,"%Y")

table(table(AODyears))
unique(AODyears)
which(table(AODyears)>1)
which(is.na(table(AODyears)))
AODindices <- AODyears

AOD <- stackApply(AOD,AODindices,mean,na.rm=TRUE)
plot(AOD[[1]])
AODdf <- data.frame(rasterToPoints(AOD))
colnames(AODdf)[3:ncol(AODdf)] <- unique(AODindices)
AODdf <- AODdf[,c(-1,-2)]
mean(AODdf$`2002`,na.rm=T)
AODdf <- apply(AODdf,2,mean,na.rm=T)
AODdf <- as.data.frame(AODdf)
AODdf
AODdf$date <- paste0(unique(AODindices),"-01-01")
AODdf$date <- as.Date(AODdf$date,format="%Y-%m-%d")
AODdf$season <- time2season(AODdf$date,out.fmt="seasons")
colnames(AODdf)[1]<-"AOD"
AODdf$group = "5c"


AODzip <- crop(AOD,cropLongLatzip_small)
AODzip <- mask(AODzip,cropLongLatzip_small)
AODzipdf <- data.frame(rasterToPoints(AODzip))
colnames(AODzipdf)[3:ncol(AODzipdf)] <- unique(AODindices)
AODzipdf <- AODzipdf[,c(-1,-2)]
mean(AODzipdf$`2002`)
AODzipdf <- apply(AODzipdf,2,mean,na.rm=T)
AODzipdf <- as.data.frame(AODzipdf)
AODzipdf
AODzipdf$date <- paste0(unique(AODindices),"-01-01")
AODzipdf$date <- as.Date(AODzipdf$date,format="%Y-%m-%d")
AODzipdf$season <- time2season(AODzipdf$date,out.fmt="seasons")
colnames(AODzipdf)[1]<-"AOD"
AODzipdf$group = "ZIP"


combind_AOD_df <- rbind(AODdf,AODzipdf)
write.csv(combind_AOD_df,"Monthly Average AOD datatable.csv")
  
  