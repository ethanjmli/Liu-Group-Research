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
tempcolorvector <- colorRampPalette(c("grey","white","violet",
                     "blueviolet","blue","cadetblue2", "aquamarine",
                     "green","chartreuse","greenyellow","yellow",
                     "goldenrod","orange","orangered3",
                     "red","salmon4","black"))(1000)
airpolcolorvector <- c(turbo(1000),"grey","black")

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
crop_parameters_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
crop_parameters_5c <- st_read("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
cropLongLat_5c <- st_make_valid(cropLongLat_5c)
cropLongLat5c_small <- cropLongLat_5c[,9]
five_c_boundary <- st_combine(cropLongLat5c_small)
five_c_boundary <- st_bbox(five_c_boundary)
five_c_boundary <- st_as_sfc(five_c_boundary)
st_write(five_c_boundary,"5c_boundary.shp")
rm(crop_parameters_5c)
rm(cropLongLat_5c)
  
crop_parameters_zip <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/SelectedZipCode_ATL.shp")
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
cropLongLatzip_small <- cropLongLat_zip[,10]
colnames(cropLongLat_zip)[1]<-"ZIP Code"
rm(crop_parameters_zip)
rm(cropLongLat_zip)

tracts <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties_tracts.shp")
names(tracts) <- c("Tract GEOID","Tract Name", "County GEOID","County Name","geometry")

ggplot() +
  geom_sf(data=st_as_sf(cropLongLat_5c), aes(fill = as.factor(NAME)))+
  geom_sf(data=st_as_sf(cropLongLat_zip), aes(fill = as.factor(ZCTA5CE10)))




####2020 Daytime LST####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2003_01_01 - 2023_03_23/Time_Series/RData/Terra/LST_Day_1km")
setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Surf_Temp_Daily_1Km_v6/Time_Series/RData/Terra/LST_Day_1km")
dayraster2020 <- get(load("MOD11A1_LST_Day_1km_1_2020_366_2020_RData.RData"))
values(dayraster2020) <- values(dayraster2020)*0.02-273
day2020crop <- crop(dayraster2020, cropLongLat)
day2020mask <- mask(day2020crop, cropLongLat)
day2020maskdf <- as.data.frame(day2020mask,xy=TRUE)
day2020maskpoints <- rasterToPoints(day2020mask)

#names(yeardaymask)<-seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days")
#names(yeardaymask)

#Calculate daily Daytime Mean Temp
Mean_Temp_Celsius_Day<-apply(day2020maskdf[,c(3:length(day2020maskdf))],2,FUN=mean,na.rm=TRUE)
Mean_Temp_Celsius_Day<-as.data.frame(Mean_Temp_Celsius_Day)

#Plot daily Daytime Mean Temp
DaySeq <- c(18262:18627)
Mean_Temp_Celsius_Day<-cbind(Mean_Temp_Celsius_Day,DaySeq)
Mean_Temp_Celsius_Day$DaySeq <- as.Date.numeric(Mean_Temp_Celsius_Day$DaySeq, origin = "1970-01-01")
ggplot(data=Mean_Temp_Celsius_Day,aes(x=DaySeq,y=Mean_Temp_Celsius))+
  geom_point()+
  geom_line()
#spring <- seq(as.Date("2020/3/1"), as.Date("2020/5/31"), "days")

#Plot Arbitray range of Daily Daytime Mean Temp
ggplot(data=Mean_Temp_Celsius_Day[1:31,],aes(x=DaySeq,y=Mean_Temp_Celsius))+
  geom_point()+
  geom_smooth()

plot(yeardaymask, col = temperature.colors(30), nc = 6, maxnl = 24)

#Convert col names to numeric dates
names(day2020maskdf) <- c(names(day2020maskdf)[c(1,2)],
                            seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"))
#Get sequence of 2020 dates
Dates <- as.Date.numeric(as.numeric(colnames(day2020maskdf)[c(-1,-2)]),
                         origin = "1970-01-01")

##Get weekly and monthly average temperatures 
night2020maskdft <- as.data.frame(t(night2020maskdf))
night2020maskdftsub <- night2020maskdft[3:368,]
avgxcoords <- apply(night2020maskdftsub,1,FUN=mean,na.rm=TRUE)
avgxcoords<-as.data.frame(avgxcoords)

avgxcoords <- cbind(Dates,avgxcoords)

avgxcoords$week <- floor_date(avgxcoords$Dates, "week")
avgxcoords$month <- floor_date(avgxcoords$Dates, "month")

avgxcoordsbyweek <- avgxcoords %>%
  group_by(week) %>%
  summarize(mean = mean(avgxcoords,na.rm=TRUE))

avgxcoordsbymonth <- avgxcoords %>%
  group_by(month) %>%
  summarize(mean = mean(avgxcoords,na.rm=TRUE))

ggplot(data=avgxcoordsbyweek,aes(x=week,y=mean))+
  geom_point()+
  geom_smooth()

ggplot(data=avgxcoordsbymonth,aes(x=month,y=mean))+
  geom_point()+
  geom_smooth()


######Map weekly and average monthly temperatures#####
######Monthly#####
names(day2020mask)
names(day2020mask)<-Dates
names(day2020mask)
indices <- format(as.Date(names(day2020mask), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
Monthday2020mask<- stackApply(day2020mask, indices, fun = mean)
names(Monthday2020mask) <- month.abb
Monthday2020mask
max(Monthday2020mask,na.rm=TRUE)
min(Monthday2020mask,na.rm=TRUE)
cuts=seq(7,43,by = 1) #set breaks #set breaks
pal <- colorRampPalette(c("blue","firebrick1"))
plot(Monthday2020mask, breaks=cuts, col = inferno(length(cuts)))


wintercuts=seq(0,18,by = 0.5)
plot(Monthday2020mask[[c(1:2,12)]], breaks = wintercuts, col = rainbow(length(wintercuts)))
cuts =seq(20,41,by=1)
plot(Monthday2020mask[[c(5:10)]], breaks = cuts, col = inferno(length(wintercuts)))

######Weekly Daytime LST mapping#####
names(day2020mask)
week <- strftime(Dates,format = "%V")
#week2 <- week(Dates)
Weekday2020mask <- stackApply(day2020mask,week,fun=mean)
#Weekday2020mask2 <- stackApply(day2020mask,week2,fun=mean)
Weekday2020mask
#Weekday2020mask2
Weeknumbering <- paste(rep("Week",each=53), as.character(c(1:53)),  sep = " ")
names(Weekday2020mask) <- Weeknumbering
#names(Weekday2020mask2)<-c(1:53)
Weekday2020mask
#Weekday2020mask2
max(Weekday2020mask,na.rm=TRUE)
min(Weekday2020mask,na.rm=TRUE)
#max(Weekday2020mask2,na.rm=TRUE)
#min(Weekday2020mask2,na.rm=TRUE)
#cuts=seq(-1,44,by = 1) #set breaks #set breaks
#pal <- colorRampPalette(c("black","firebrick1"))
#plot(Weekday2020mask[[6]])#, breaks=cuts, col = inferno(length(cuts)),nc = 10, maxnl = 53)
#plot(Weekday2020mask2, breaks=cuts, col = inferno(length(cuts)),nc = 10, maxnl = 53)

######ggplot######
coords <- xyFromCell(Weekday2020mask, seq_len(ncell(Weekday2020mask)))
Weekday2020maskdf <- stack(as.data.frame(getValues(Weekday2020mask)))
names(Weekday2020maskdf) <- c('value', 'week')
Weekday2020maskdf$week <- as.numeric(Weekday2020maskdf$week)

Weekday2020maskdf <- cbind(coords, Weekday2020maskdf)
#Weekday2020maskdf <- rename(Weekday2020maskdf, week = variable) 
Weekday2020maskdf$season <- ifelse(Weekday2020maskdf$week %in% c(1:12,52:53), "winter",
                                  ifelse(Weekday2020maskdf$week %in% c(13:25),"spring",
                                  ifelse(Weekday2020maskdf$week %in% c(26:39),"summer","fall")))

  
  #new_scale_fill() + 

winter2020 <- 
  ggplot() + 
  geom_tile(
    aes(x, y, fill = value),
    filter(Weekday2020maskdf, season == "winter"))+
  facet_wrap(vars(week), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colours = tempcolorvector) +
  labs(fill = "Celsius",title = "Five Counties Weekly Winter Avg Temp") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal()
winter2020

##Map weekly and average monthly temperatures
#Monthly
names(night2020mask)
names(night2020mask)<-Dates
names(night2020mask)
indices <- format(as.Date(names(night2020mask), format = "X%Y.%m.%d"), format = "%m")
indices <- as.numeric(indices)
Monthnight2020mask<- stackApply(night2020mask, indices, fun = mean)
names(Monthnight2020mask) <- month.abb
Monthnight2020mask
max(Monthnight2020mask,na.rm=TRUE)
min(Monthnight2020mask,na.rm=TRUE)
cuts=seq(-1,27,by = 1) #set breaks #set breaks
pal <- colorRampPalette(c("black","firebrick1"))
plot(Monthnight2020mask, breaks=cuts, col = inferno(length(cuts)))

#Weekly
names(night2020mask)
week <- strftime(Dates,format = "%V")
Weeknight2020mask <- stackApply(night2020mask,week,fun=mean)
names(Weeknight2020mask) <- c(1:53)
Weeknight2020mask
max(Weeknight2020mask,na.rm=TRUE)
min(Weeknight2020mask,na.rm=TRUE)
cuts=seq(-5,29,by = 1) #set breaks #set breaks
pal <- colorRampPalette(c("black","firebrick1"))
plot(Weeknight2020mask, breaks=cuts, col = inferno(length(cuts)),nc = 10, maxnl = 53)

weekexperiment <- c(as.Date("2020-01-01"):as.Date("2023-12-31"))
weekexperiment2 <- as.Date.numeric(weekexperiment,origin="1970-01-01")
weekexperiment3 <- strftime(weekexperiment2, format="%V")
weekexperiment4 <- week(weekexperiment2)
table(weekexperiment3)
table(weekexperiment4)


####2020 Greenspace####
#scale = values*0.0001
crop_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
crop_5c <- st_read("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Shapefiles/five_counties.shp")
cropLongLat_5c <- st_transform(crop_5c,crs("+proj=longlat +datum=WGS84 +no_defs"))


setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2003_01_01-2023_03_23/NDVI")

list.files()
list.files(pattern="2020.*.tif$")
list.filenames<-list.files(pattern="2020.*.tif$")
list.filenames

NDVI_16day_2020 <- stack(list.filenames)
values(NDVI_16day_2020) <- values(NDVI_16day_2020)*0.0001
NDVI_16day_2020_5c <- crop(NDVI_16day_2020, cropLongLat_5c)
NDVI_16day_2020_5c <- mask(NDVI_16day_2020_5c, cropLongLat_5c)
#NDVI_16day_2020_5cdf <- as.data.frame(NDVI_16day_2020_5c,xy=TRUE)

#Get sequence of 2020 dates in 16-day increments
days_2020_by16 <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"),by=16)
#length(NDVI_16days_dates)

#Convert NDVI Layer names to readable dates
names(NDVI_16day_2020_5c)
names(NDVI_16day_2020_5c)<-days_2020_by16
names(NDVI_16day_2020_5c)

NDVI_16Day_2020_5ccoords <- xyFromCell(NDVI_16day_2020_5c, seq_len(ncell(NDVI_16day_2020_5c)))
NDVI_16day_2020_5cdf <- stack(as.data.frame(getValues(NDVI_16day_2020_5c)))
names(NDVI_16day_2020_5cdf) <- c('NDVI_value', 'date')
#NDVI_16day_2020maskdf$date <- as.numeric(NDVI_16day_2020maskdf$date)

NDVI_16day_2020_5cdf <- cbind(NDVI_16Day_2020_5ccoords, NDVI_16day_2020_5cdf)
#Weekday2020maskdf <- rename(Weekday2020maskdf, week = variable) 
#Weekday2020maskdf$season <- ifelse(Weekday2020maskdf$week %in% c(1:12,52:53), "winter",
 #                                  ifelse(Weekday2020maskdf$week %in% c(13:25),"spring",
  #                                     ifelse(Weekday2020maskdf$week %in% c(26:39),"summer","fall")))

NDVI_16days_dates_5c <- rep(days_2020_by16, each = 12928)
NDVI_16day_2020_5cdf <- cbind(NDVI_16day_2020_5cdf,NDVI_16days_dates_5c )
table(NDVI_16day_2020_5cdf$date, NDVI_16day_2020_5cdf$NDVI_16days_dates_5c)
NDVI_16day_2020_5cdf <- NDVI_16day_2020_5cdf[,-4]
colnames(NDVI_16day_2020_5cdf)[4]<-"date"

ggplot() + 
  geom_tile(data = NDVI_16day_2020_5cdf[NDVI_16day_2020_5cdf$date=="2020-09-29",],
    aes(x, y, fill = NDVI_value)
    #filter(Weekday2020maskdf, season == "winter")
    )+
  facet_wrap(vars(date), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = rev(terrain.colors(n=1000))) +
  labs(fill = "NDVI",title = "Five Counties 2020 16 Day NDVI") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal()

NDVI_16day_2020_5csf <- st_as_sf(NDVI_16day_2020_5cdf, coords = c("x","y"),crs = "+proj=longlat +datum=WGS84 +no_defs")

tm_shape(NDVI_16day_2020_5csf[NDVI_16day_2020_5csf$date=="2020-09-29",])+
  tm_sf(col = "NDVI_value")


######Pair with 16-day increment Day LST#####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Surf_Temp_Daily_1Km_v6/LST_Day_1km")
setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Surf_Temp_Daily_1Km_v6/Time_Series/RData/Terra/LST_Day_1km")

list.files()
list.files(pattern="MOD11A1.*.tif$")
list.filenames<-list.files(pattern="MOD11A1.*.tif$")
list.filenames

dayraster2020 <- stack(list.filenames)
values(dayraster2020) <- values(dayraster2020)*0.02-273
day20205c <- crop(dayraster2020, cropLongLat_5c)
day20205c <- mask(day20205c, cropLongLat_5c)
day20205cdf <- as.data.frame(day20205c,xy=TRUE)
names(day20205c)

days_2020_by16 <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"),by=16)
LST2020_16days_dates <- rep(days_2020_by16,each=16)
LST2020_16days_dates <- head(LST2020_16days_dates,-2)
LST2020_16days_5c <- stackApply(day20205c,LST2020_16days_dates,fun=mean)
names(LST2020_16days_5c) <- days_2020_by16
names(LST2020_16days_5c)

LST2020_16days_5ccoords <- xyFromCell(LST2020_16days_5c, seq_len(ncell(LST2020_16days_5c)))
LST2020_16days_5cdf <- stack(as.data.frame(getValues(LST2020_16days_5c)))
names(LST2020_16days_5cdf) <- c('temperature', 'date')


LST2020_16days_dates_5c <- rep(days_2020_by16, each = 12928)
LST2020_16days_5cdf <- cbind(LST2020_16days_5cdf,LST2020_16days_dates_5c)
table(LST2020_16days_5cdf$date, LST2020_16days_5cdf$LST2020_16days_dates_5c)
LST2020_16days_5cdf <- cbind(LST2020_16days_5ccoords, LST2020_16days_5cdf)
LST2020_16days_5cdf <- LST2020_16days_5cdf[,-4]
colnames(LST2020_16days_5cdf)[4]<-"date"
#NDVI_16day_2020maskdf$date <- as.numeric(NDVI_16day_2020maskdf$date)

ggplot() + 
  geom_tile(data = LST2020_16days_5cdf[LST2020_16days_5cdf$date == "2020-09-29",],
            aes(x, y, fill = temperature)
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(date), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "NDVI",title = "Five Counties 2020 16 Day NDVI") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal() +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=2,colour="black",alpha=0)


####Mapping Socioeconomic Data####
#install.packages("tidycensus")
library(tidycensus)

v21 <- load_variables(2021, "acs5", cache = TRUE)
#Census API key: 962254c4a7787dd62a33cbf37842dd5dcb8ff620

census_api_key("962254c4a7787dd62a33cbf37842dd5dcb8ff620",install=TRUE)

######Mapping Black population#####
five_counties_Blackpop <- get_acs(
  geography = "tract", 
  variables = "C02003_004",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE
)
five_counties_Blackpop

plot(five_counties_Blackpop)

five_counties_Totalpop <- get_acs(
  geography = "tract", 
  variables = "B01003_001",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE
)


totalpop <- 
  ggplot()+
  geom_sf(data=five_counties_Totalpop, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Total pop",title = "Five Counties 2020 Total Pop") +
  xlab("Longitude")+
  ylab("Latitude")
totalpopalt + totalpop

five_counties_Blackpercent <- five_counties_Blackpop
five_counties_Blackpercent$estimate <- (five_counties_Blackpop$estimate/
                                            five_counties_Totalpop$estimate)*100

plot(five_counties_Blackpercent)

plot3<- ggplot()+
  geom_sf(data=five_counties_Blackpop, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Black pop",title = "Five Counties 2020 Black Pop") +
  xlab("Longitude")+
  ylab("Latitude")
plot3

plot1+plot3

######Mapping Household Income#####
five_counties_12monthincome <- get_acs(
  geography = "tract", 
  variables = "B19013A_001",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE,
  survey = "acs5"
)

plot(five_counties_12monthincome)
plot4<- ggplot()+
  geom_sf(data=five_counties_12monthincome, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Income",title = "Five Counties 2020 Household Income") +
  xlab("Longitude")+
  ylab("Latitude")

plot1+plot2+plot3+plot4

####Mapping Traffic Intensity####
#install.packages("osmdata")
library(osmdata)
available_features()

available_tags(feature = "highway")

class(cropLongLat5c_small)

five_c_bb <- st_bbox(cropLongLat5c_small)
five_c_major <- osmdata_sf(add_osm_feature(opq(five_c_bb), 
                    key = "highway", value = c("motorway","primary","secondary")))

five_c_highways <- five_c_major$osm_lines
st_crs(five_c_highways)


five_c_highways <- st_intersection(five_c_highways, cropLongLat5c_small)
zip_highways <- st_crop(five_c_highways, cropLongLatzip_small)

zip_highways <- st_make_valid(zip_highways)
five_c_highways <- st_make_valid(five_c_highways)

colnames(five_c_highways)[73] <- "Highway Type"
colnames(zip_highways)[73] <- "Highway Type"
tmap_options(check.and.fix = TRUE)

MetroATLHighways <- 
tm_shape(cropLongLat_5c)+
  tm_polygons(col="NAME",title= "County",alpha=0.5)+
tm_shape(five_c_highways)+
  tm_lines(col = "Highway Type", 
           palette = c("red","blue","green","purple"),
           labels = c("Motorway","Motorway Link","Primary","Secondary")
           )+
tm_shape(cropLongLatzip_small)+
  tm_borders(col = 'black', lwd = 2)+
  tm_compass(position = c("LEFT","BOTTOM"))+
  tm_scale_bar(position = c("LEFT","BOTTOM")) +
  tm_layout(main.title=paste0("Highways in\nFive Counties"),
            main.title.size=1.2,
            main.title.position= "center",
            legend.position = c("RIGHT","BOTTOM"))+
tm_grid(lines = FALSE)

ZIPHighways <- 
  tm_shape(cropLongLat_zip)+
    tm_polygons(col="ZIP Code",alpha=0.5)+
  tm_shape(zip_highways)+
    tm_lines(col = "Highway Type", 
           palette = c("red","green","purple"),
           labels = c("Motorway","Primary","Secondary"),
           lwd=3)+
  tm_compass(position = c("LEFT","BOTTOM"))+
  tm_scale_bar(position = c("LEFT","BOTTOM")) +  # add a scale bar
 
  tm_layout(main.title=paste0("Highways in\nSelect ZIP Codes"),
            main.title.size=1,
            main.title.position=0.25,
            legend.outside = TRUE)+
  tm_grid(lines = FALSE)


####5 Counties + Zip code level mapping####
######Get shapefiles and boundaries######
crop_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
crop_5c <- st_read("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Shapefiles/five_counties.shp")
cropLongLat_5c <- st_transform(crop_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

crop_parameters_zip <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/SelectedZipCode_ATL.shp")
crop_parameters_zip <- st_read("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Shapefiles/SelectedZipCode_ATL.shp")
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

boundary_zip <- as(cropLongLat_zip, "sf")
boundary_5c <- as(cropLongLat_5c, "sf")

st_write(boundary_5c, "H:/DayMet Data/boundary_5c.shp")

ggplot() +
  geom_sf(data=boundary_5c, aes(fill = as.factor(NAME)))+
  geom_sf(data=boundary_zip, aes(fill = as.factor(ZCTA5CE10)))

######LST######

#Crop 2020 LST with selected zip codes#
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2003_01_01 - 2023_03_23/LST_Day_1km")

list.files()
list.files(pattern="MOD11A1.*.tif$")
list.filenames<-list.files(pattern="MOD11A1.*.tif$")
list.filenames

dayraster2020 <- stack(list.filenames)
values(dayraster2020) <- values(dayraster2020)*0.02-273
day2020zip <- crop(dayraster2020, cropLongLat_zip)
day2020zip <- mask(day2020zip, cropLongLat_zip)
day2020zipdf <- as.data.frame(day2020zip,xy=TRUE)

#Get mean LST in 16 day intervals#
names(day2020zip)
days_2020_by16 <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"),by=16)
LST2020_16days_dates <- rep(days_2020_by16,each=16)
LST2020_16days_dates <- head(LST2020_16days_dates,-2)
LST2020_16days_zip <- stackApply(day2020zip,LST2020_16days_dates,fun=mean)

names(LST2020_16days_zip) <- days_2020_by16
names(LST2020_16days_zip)

#Prepare for ggplotting#
LST2020_16days_zipcoords <- xyFromCell(LST2020_16days_zip, seq_len(ncell(LST2020_16days_zip)))
LST2020_16days_zipdf <- stack(as.data.frame(getValues(LST2020_16days_zip)))
names(LST2020_16days_zipdf) <- c('temperature', 'date')

LST2020_16days_dates_zip <- rep(days_2020_by16, each = 432)
LST2020_16days_zipdf <- cbind(LST2020_16days_zipdf,LST2020_16days_dates_zip)
table(LST2020_16days_zipdf$date, LST2020_16days_zipdf$LST2020_16days_dates_zip)
LST2020_16days_zipdf <- cbind(LST2020_16days_zipcoords, LST2020_16days_zipdf)
LST2020_16days_zipdf <- LST2020_16days_zipdf[,-4]
colnames(LST2020_16days_zipdf)[4]<-"date"

LST2020_16days_zipdf$period <- paste0(LST2020_16days_zipdf$date, " - ", LST2020_16days_zipdf$date+15)  
class(LST2020_16days_zipdf$period)

#Map zip code LST
zipLST <- 
  ggplot()+
  geom_tile(data = LST2020_16days_zipdf[LST2020_16days_zipdf$date == "2020-01-17"
                                        & is.na(LST2020_16days_zipdf$temperature) == FALSE,],
            aes(x, y, fill = temperature)#,
            #filter(LST2020_16daysmaskdf, date == "X2020.01.01")
  )+
  facet_wrap(vars(period), ncol=2) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Temperature",title = "Select Zip 2020 16 Day Average Temperature") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  coord_equal()+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=2, colour="black",alpha=0) + 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

#Map five counties LST
LST2020_16days_5cdf$period <- paste0(LST2020_16days_5cdf$date, " - ", LST2020_16days_5cdf$date+15)  
class(LST2020_16days_5cdf$period)

five_c_LST<- 
  ggplot() + 
  geom_tile(data = LST2020_16days_5cdf[LST2020_16days_5cdf$date == "2020-09-29" 
                                       & is.na(LST2020_16days_5cdf$temperature) == FALSE,],
            aes(x, y, fill = temperature),
            na.rm = TRUE
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(period), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Temperature °C",title = "Five Counties 2020 Temperature 16 Day Average") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  coord_sf() +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=2,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

five_c_LST + zipLST
five_c_LST + five_counties_Blackpercent

######NDVI######
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/VI_16Days_1Km_v6/NDVI")
setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/VI_16Days_1Km_v6/Time_Series/RData/Terra/NDVI")

list.files()
list.files(pattern="MOD13A2.*.tif$")
list.filenames<-list.files(pattern="MOD13A2.*.tif$")
list.filenames

NDVI_16day_2020 <- stack(list.filenames)
values(NDVI_16day_2020) <- values(NDVI_16day_2020)*0.0001
NDVI_16day_2020_zip <- crop(NDVI_16day_2020, cropLongLat_zip)
NDVI_16day_2020_zip <- mask(NDVI_16day_2020_zip, cropLongLat_zip)
NDVI_16day_2020_zipdf <- as.data.frame(NDVI_16day_2020_zip,xy=TRUE)

#Get sequence of 2020 dates in 16-day increments
days_2020_by16 <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"),by=16)
#length(NDVI_16days_dates)

#Convert NDVI Layer names to readable dates
names(NDVI_16day_2020_zip)
names(NDVI_16day_2020_zip)<-days_2020_by16
names(NDVI_16day_2020_zip)

NDVI_16Day_2020_zipcoords <- xyFromCell(NDVI_16day_2020_zip, seq_len(ncell(NDVI_16day_2020_zip)))
NDVI_16day_2020_zipdf <- stack(as.data.frame(getValues(NDVI_16day_2020_zip)))
names(NDVI_16day_2020_zipdf) <- c('NDVI_value', 'date')
#NDVI_16day_2020maskdf$date <- as.numeric(NDVI_16day_2020maskdf$date)

NDVI_16day_2020_zipdf <- cbind(NDVI_16Day_2020_zipcoords, NDVI_16day_2020_zipdf)
NDVI_16days_dates_zip <- rep(days_2020_by16, each = 432)
NDVI_16day_2020_zipdf <- cbind(NDVI_16day_2020_zipdf,NDVI_16days_dates_zip )
table(NDVI_16day_2020_zipdf$date, NDVI_16day_2020_zipdf$NDVI_16days_dates_zip)
NDVI_16day_2020_zipdf <- NDVI_16day_2020_zipdf[,-4]
colnames(NDVI_16day_2020_zipdf)[4]<-"date"

NDVI_16day_2020_zipdf$period <- paste0(NDVI_16day_2020_zipdf$date, " - ", NDVI_16day_2020_zipdf$date+15)  
class(NDVI_16day_2020_zipdf$period)


zipNDVI <- 
  ggplot() + 
  geom_tile(data = NDVI_16day_2020_zipdf[NDVI_16day_2020_zipdf$date == "2020-09-29"
                                         & is.na(NDVI_16day_2020_zipdf$NDVI_value) == FALSE,],
            aes(x, y, fill = NDVI_value)
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(period), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = rev(terrain.colors(n=1000))) +
  labs(fill = "NDVI",title = "Select Zip 2020 16 Day NDVI") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  coord_equal()+ 
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=2, colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel


#Five counties
NDVI_16day_2020_5cdf$period <- paste0(NDVI_16day_2020_5cdf$date, " - ", NDVI_16day_2020_5cdf$date+15)  
class(NDVI_16day_2020_5cdf$period)

five_c_NDVI<- 
  ggplot() + 
  geom_tile(data = NDVI_16day_2020_5cdf[NDVI_16day_2020_5cdf$date=="2020-09-29"
                                        & is.na(NDVI_16day_2020_5cdf$NDVI_value) == FALSE,],
            aes(x, y, fill = NDVI_value)
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(period), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = rev(terrain.colors(n=1000))) +
  labs(fill = "NDVI",title = "Five Counties 2020 16 Day NDVI") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  coord_equal() +

  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=2,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

five_c_NDVI + zipNDVI

######Black Pop######
#Absolute count#
five_counties_Blackpop <- get_acs(
  geography = "tract", 
  variables = "C02003_004",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE,
  survey = "acs5"
)
class(five_counties_Blackpop)

plot(five_counties_Blackpop)

five_c_Blackpop <- 
  ggplot()+
  geom_sf(data=five_counties_Blackpop, aes(fill=estimate))+
  scale_fill_distiller(palette = "BuPu", 
                       direction = 1)+
  labs(fill = "Number of People",
       title = "Black/African American Population (2021) Five Counties",
       caption = "Data: US Census ACS5 2021 C02003_004") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

cropLongLat_zipNAD83 <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_Blackpop <- st_intersection(five_counties_Blackpop,cropLongLat_zipNAD83)

plot(zip_Blackpop)
zipBlackpop <-
  ggplot()+
  geom_sf(data=zip_Blackpop, aes(fill=estimate))+
  scale_fill_distiller(palette = "BuPu", 
                       direction = 1)+
  labs(fill = "Number of People",
       title = "Black/African American Population (2021) Select ZIP Codes",
       caption = "Data: US Census ACS5 2021 C02003_004")+
  xlab("Longitude")+
  ylab("Latitude")+ 
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=5, colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

five_c_Blackpop + zipBlackpop

#Percentile of total population#
five_c_Blackpercent <- 
  ggplot()+
  geom_sf(data=five_counties_Blackpercent, aes(fill=estimate))+
  scale_fill_distiller(palette = "BuPu", 
                       direction = 1)+
  labs(fill = "Percent of Population",
       title = "Black/African American Population Percentage (2021) Five Counties",
       caption = "Data: US Census ACS5 2021 C02003_004/B01003_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

cropLongLat_zip_NAD83 <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_Blackpercent <- st_intersection(five_counties_Blackpercent,cropLongLat_zip_NAD83)

five_c_Blackpercent <- tm_shape(five_counties_Blackpercent)+
  tm_fill(col="estimate",title="Percent of \nPopulation",palette="BuPu")+
  tm_borders()+
  tm_compass(position = c("LEFT","BOTTOM"))+
  tm_scale_bar(position = c("LEFT","BOTTOM"))+
  tm_credits("Data Source: US Census 2021 ACS5")+
  tm_layout(frame=F,
            bg.color="transparent",
            main.title=("Study Area Black Population"),
            main.title.size=1.5,
            main.title.position="center",
            legend.height=-0.4,
            legend.text.size = 0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
tmap_save(five_c_Blackpercent,"Study Area Black Percent.png",bg="transparent")

plot(zip_Blackpercent)
zipBlackpercent <-
  ggplot()+
  geom_sf(data=zip_Blackpercent, aes(fill=estimate))+
  scale_fill_distiller(palette = "BuPu", 
                       direction = 1,
                       limits = c(0,100))+
  labs(fill = "Percent of Population",
       title = "Black/African American Population Percentage (2021) Select Zip Codes",
       caption = "Data: US Census ACS5 2021 C02003_004/B01003_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel
five_c_Blackpercent + zipBlackpercent

######Median Household Income#####
five_counties_12monthincome_median <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE,
  survey = "acs5"
)

plot(five_counties_12monthincome_median)

five_c_12monthincome_median<-
  ggplot()+
  geom_sf(data=five_counties_12monthincome_median, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1)+
  labs(fill = "Income (2021 Inflation-Adjusted Dollars)",
       title = ("Median Household Income in the Last 12 Months (2021) Five Counties"),
       caption = "Data: US Census ACS5 2021 B19013_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel

cropLongLat_zipNAD83 <- st_transform(crop_parameters_zip,crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_12monthincome_median <- st_intersection(five_counties_12monthincome_median,cropLongLat_zipNAD83)

zip12monthincome_median <-
  ggplot()+
  geom_sf(data=zip_12monthincome_median, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1)+
  labs(fill = "Income (2021 Inflation-Adjusted Dollars)",
       title = ("Median Household Income in the Last 12 Months (2021) Select ZIP Codes"),
       caption = "Data: US Census ACS5 2021 B19013_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel

five_c_12monthincome_median + zip12monthincome_median


######Less Than High School Education#####
five_counties_ltHS <- get_acs(
  geography = "tract", 
  variables = "B16010_002",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE
)

five_counties_ltHSpercent <- five_counties_ltHS
five_counties_ltHSpercent$estimate <- (five_counties_ltHS$estimate/
                                          five_counties_Totalpop$estimate)*100

five_c_ltHS<-
  ggplot()+
  geom_sf(data=five_counties_ltHS, aes(fill=estimate))+
  scale_fill_distiller(palette = "OrRd", 
                       direction = 1)+
  labs(fill = "# of People",
       title = "Population with Less Than High School Graduate Education",
       caption = "Data: US Census ACS5 2021 B16010_002") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel


five_c_ltHSpercent<-
  ggplot()+
  geom_sf(data=five_counties_ltHSpercent, aes(fill=estimate))+
  scale_fill_distiller(palette = "OrRd", 
                       direction = 1)+
  labs(fill = "Percent Population",
       title = "Percent Population with Less Than High School Graduate Education",
       caption = "Data: US Census ACS5 2021 B16010_002/B01003_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel


five_c_ltHS + five_c_ltHSpercent

cropLongLat_zipNAD83 <- st_transform(crop_parameters_zip,crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_ltHS <- st_intersection(five_counties_ltHS,cropLongLat_zipNAD83)
zip_ltHSpercent <- st_intersection(five_counties_ltHSpercent,cropLongLat_zipNAD83)

#Zip code less than high school
zip_ltHS_plot<-
  ggplot()+
  geom_sf(data=zip_ltHS, aes(fill=estimate))+
  scale_fill_distiller(palette = "OrRd", 
                       direction = 1)+
  labs(fill = "# of People",
       title = "Population with Less Than High School Graduate Education",
       caption = "Data: US Census ACS5 2021 B16010_002") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel


zip_ltHSpercent_plot<-
  ggplot()+
  geom_sf(data=zip_ltHSpercent, aes(fill=estimate))+
  scale_fill_distiller(palette = "OrRd", 
                       direction = 1)+
  labs(fill = "Percent Population",
       title = "Percent Population with Less Than High School Graduate Education",
       caption = "Data: US Census ACS5 2021 B16010_002/B01003_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel


zip_ltHS_plot + zip_ltHSpercent_plot

######Unemployment Percentage#####
five_counties_Unemployed <- get_acs(
  geography = "tract", 
  variables = "B23025_005",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2021,
  geometry = TRUE
)

five_counties_Unemployedpercent <- five_counties_Unemployed
five_counties_Unemployedpercent$estimate <- (five_counties_Unemployed$estimate/
                                          five_counties_Totalpop$estimate)*100

five_c_unemployedpercent <- 
  ggplot()+
  geom_sf(data=five_counties_Unemployedpercent, aes(fill=estimate))+
  scale_fill_gradientn(guide = "colourbar",colors = airpolcolorvector)+
  labs(fill = "Percent Unemployment",
       title = "Unemployment Rate (2021) Five Counties",
       caption = "Data: US Census ACS5 2021 B23025_005/B01003_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

cropLongLat_zip_NAD83 <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_Unemployedpercent <- st_intersection(five_counties_Unemployedpercent,cropLongLat_zip_NAD83)

zipUnemployedpercent <-
  ggplot()+
  geom_sf(data=zip_Unemployedpercent, aes(fill=estimate))+
  scale_fill_gradientn(guide = "colourbar",colors = airpolcolorvector)+
  labs(fill = "Percent Unemployment",
       title = "Unemployment Rate (2021) Select Zip Codes",
       caption = "Data: US Census ACS5 2021 B23025_005/B01003_001") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel
five_c_unemployedpercent + zipUnemployedpercent



####Map Model Data####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Georgia_subset_2020")

list.files()
list.files(pattern="ATL.*.tif$")
list.filenames<-list.files(pattern="ATL.*.tif$")
list.filenames

model2020_stack <- stack(list.filenames)

crs(model2020_stack)
plot(model2020_stack[[1]])
model2020_stack <- projectRaster(model2020_stack,crs = "+proj=longlat +datum=WGS84 +no_defs") #Takes time to run
crs(model2020_stack)
plot(model2020_stack[[1]])
values(model2020_stack) <- values(model2020_stack)*0.1

model2020_5c <- crop(model2020_stack, cropLongLat_5c)
model2020_5c <- mask(model2020_5c, cropLongLat_5c)
plot(model2020_5c[[1]])

days_2020_by16 <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"),by=16)
model2020_16days_dates <- rep(days_2020_by16,each=16)
model2020_16days_dates <- head(model2020_16days_dates,-3)
model2020_16days_5c <- stackApply(model2020_5c, model2020_16days_dates,fun=mean)
names(model2020_16days_5c) <- days_2020_by16
names(model2020_16days_5c)

model2020_16days_5ccoords <- xyFromCell(model2020_16days_5c, seq_len(ncell(model2020_16days_5c)))
model2020_16days_5cdf <- stack(as.data.frame(getValues(model2020_16days_5c)))
names(model2020_16days_5cdf) <- c('Modeled temperature', 'date')


model2020_16days_dates_5c <- rep(days_2020_by16, each = 10500)
model2020_16days_5cdf <- cbind(model2020_16days_5cdf,model2020_16days_dates_5c)
table(model2020_16days_5cdf$date, model2020_16days_5cdf$model2020_16days_dates_5c)
model2020_16days_5cdf <- cbind(model2020_16days_5ccoords, model2020_16days_5cdf)
model2020_16days_5cdf <- model2020_16days_5cdf[,-4]
colnames(model2020_16days_5cdf)[4]<-"date"

model2020_16days_5cdf$period <- paste0(model2020_16days_5cdf$date, " - ", model2020_16days_5cdf$date+15)  
class(model2020_16days_5cdf$period)

model_five_c_AT<- 
  ggplot() + 
  geom_tile(data = model2020_16days_5cdf[model2020_16days_5cdf$date == "2020-09-29" 
                                       & is.na(model2020_16days_5cdf$`Modeled temperature`) == FALSE,],
            aes(x, y, fill = `Modeled temperature`),
            na.rm = TRUE
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(period), ncol=3) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Modeled Air Temperature °C",title = "2020 Modeled Air Temperature 16 Day Average") +
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')#transparent legend panel
  )+  
    coord_sf() +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=2,colour="black",alpha=0)
 
five_c_LST + model_five_c_AT

#####Map AOD#####################
#################################

setwd("C:/Users/EJLI4/OneDrive - Emory University/MODIS AOD Data")
list.files()
AODfilenames <- list.files()
AODfilenames
AODfilenames <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames
AODfilenames2 <- substr(AODfilenames,1,8)
table(table(AODfilenames2))
which(table(AODfilenames2)==1)

list.AODfiles <- list()
for (i in 1:length(list.files())){
  list.AODfiles[[i]] <- sds(list.files()[i])
  print(i)
}

list.AOD.files.55 <- list()
for (i in 1:length(list.files())){
  list.AOD.files.55[[i]] <- list.AODfiles[[i]][2]
  print(i)
}
rm(list.AODfiles)

names(list.AOD.files.55) <- AODfilenames
str(list.AOD.files.55)
a <- unlist(lapply(list.AOD.files.55,nlyr))
table(a)

list.AOD.files.55.merged1 <- list()
for (i in seq(1, 706, 2)){
  print((i/2)+0.5)
  list.AOD.files.55.merged1[[(i/2)+0.5]] <- merge(list.AOD.files.55[[i]],list.AOD.files.55[[i+1]])
}
days2002 <- unique(grep("2002",AODfilenames2,fixed=T, value=T))
names(list.AOD.files.55.merged1) <- days2002

list.AOD.files.55.merged.projected1 <- list()
for (i in 1:length(list.AOD.files.55.merged1)){
  list.AOD.files.55.merged.projected1[[i]] <- project( list.AOD.files.55.merged1[[i]],
                                                      "+proj=longlat +datum=WGS84 +no_defs")
  print(i)
} 

list.AOD.files.55.merged.mask1 <- lapply(list.AOD.files.55.merged.projected1, function(x){
  crop(x, cropLongLat_5c)
})

list.AOD.files.55.merged.mask1 <- lapply(list.AOD.files.55.merged.mask1, function(x){
  mask(x, cropLongLat_5c)
})

names(list.AOD.files.55.merged.mask1) <- days2002

list.AOD.files.55.merged.mean1 <- lapply(list.AOD.files.55.merged.mask1, function(x){
  mean(x, na.rm=TRUE)
})

list.AOD.files.55.merged.df1 <- lapply(list.AOD.files.55.merged.mean1, function(x){
  as.data.frame(x,xy=TRUE)
})
names(list.AOD.files.55.merged.df1) <- days2002

setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
sapply(names(list.AOD.files.55.merged.df1), 
       function (x) saveRDS(list.AOD.files.55.merged.df1[[x]], file=paste(x, "RDS", sep=".") ))


#########
list.files()
AODfilenames<- list.files()[707:1438]
AODfilenames
AODfilenames2 <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames2
AODfilenames2 <- substr(AODfilenames2,1,8)
table(table(AODfilenames2))

list.AODfiles <- list()
sds(AODfilenames[1])
for (i in 1:length(AODfilenames)){
  list.AODfiles[[i]] <- sds(AODfilenames[i])
  print(AODfilenames[i])
}

list.AOD.files.55 <- list()
for (i in 1:length(list.AODfiles)){
  list.AOD.files.55[[i]] <- list.AODfiles[[i]][2]
  print(i)
}
rm(list.AODfiles)

names(list.AOD.files.55) <- AODfilenames2
str(list.AOD.files.55)
grep("2012",AODfilenames2)

list.AOD.files.55 <- list.AOD.files.55[1:366]
list.AOD.files.55.merged <- list()
for (i in seq(1, 366, 2)){
  print((i/2)+0.5)
  list.AOD.files.55.merged[[(i/2)+0.5]] <- merge(list.AOD.files.55[[i]],list.AOD.files.55[[i+1]])
}

list.AOD.files.55.projected <- list()
for (i in 1:183){
  print(i)
  list.AOD.files.55.projected[[i]] <- project(list.AOD.files.55.merged[[i]],"+proj=longlat +datum=WGS84 +no_defs")
}


list.AOD.files.55.mask <- lapply(list.AOD.files.55.projected, function(x){
  crop(x, cropLongLat_5c)
})

list.AOD.files.55.mask <- lapply(list.AOD.files.55.mask, function(x){
  mask(x, cropLongLat_5c)
})


days2012 <- unique(grep("2012",AODfilenames2,fixed=T, value=T))
names(list.AOD.files.55.mask) <- days2012[1:183]


list.AOD.files.55.mean <- lapply(list.AOD.files.55.mask, function(x){
  mean(x, na.rm=TRUE)
})

list.AOD.files.55.df <- lapply(list.AOD.files.55.mean, function(x){
  as.data.frame(x,xy=TRUE)
})

setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
sapply(names(list.AOD.files.55.df), 
       function (x) saveRDS(list.AOD.files.55.df[[x]], file=paste(x, "RDS", sep=".") ))

########

length(list.files())
AODfilenames<- list.files()[1439:2168]
AODfilenames
AODfilenames2 <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames2
AODfilenames2 <- substr(AODfilenames2,1,8)
table(table(AODfilenames2))

list.AODfiles <- list()
sds(AODfilenames[1])
for (i in 1:length(AODfilenames)){
  list.AODfiles[[i]] <- sds(AODfilenames[i])
  print(AODfilenames[i])
}

list.AOD.files.55 <- list()
for (i in 1:length(list.AODfiles)){
  list.AOD.files.55[[i]] <- list.AODfiles[[i]][2]
  print(i)
}
rm(list.AODfiles)

names(list.AOD.files.55) <- AODfilenames2
str(list.AOD.files.55)
grep("2022",AODfilenames2)

list.AOD.files.55 <- list.AOD.files.55[367:730]
list.AOD.files.55.merged <- list()
for (i in seq(1, 364, 2)){
  print((i/2)+0.5)
  list.AOD.files.55.merged[[(i/2)+0.5]] <- merge(list.AOD.files.55[[i]],list.AOD.files.55[[i+1]])
}

list.AOD.files.55.projected <- list()
for (i in 1:182){
  print(i)
  list.AOD.files.55.projected[[i]] <- project(list.AOD.files.55.merged[[i]],"+proj=longlat +datum=WGS84 +no_defs")
}


list.AOD.files.55.mask <- lapply(list.AOD.files.55.projected, function(x){
  crop(x, cropLongLat_5c)
})

list.AOD.files.55.mask <- lapply(list.AOD.files.55.mask, function(x){
  mask(x, cropLongLat_5c)
})


days2022 <- unique(grep("2022",AODfilenames2,fixed=T, value=T))
names(list.AOD.files.55.mask) <- days2022[184:365]


list.AOD.files.55.mean <- lapply(list.AOD.files.55.mask, function(x){
  mean(x, na.rm=TRUE)
})

list.AOD.files.55.df <- lapply(list.AOD.files.55.mean, function(x){
  as.data.frame(x,xy=TRUE)
})

setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
sapply(names(list.AOD.files.55.df), 
       function (x) saveRDS(list.AOD.files.55.df[[x]], file=paste(x, "RDS", sep=".") ))





#######
setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
AODlist<-list()
list.filenames <- list.files()[grepl("2002",list.files(),fixed=T)]
for(i in 1:length(list.filenames)){
  AODlist[[i]] <- readRDS(list.filenames[i])
}

list.filenames <- as.Date(as.numeric(substr(list.filenames,6,8))-1,origin = as.Date("2002-01-01"))
names(AODlist) <- list.filenames
names(AODlist) <- substr(names(AODlist),6,7)

for (i in 1:length(AODlist)){
  names(AODlist[[i]]) <- c("Longitude","Latitude","Mean AOD")
}

AOD.2002.monthly <-lapply(unique(names(AODlist)), function(x){
  bind_rows(AODlist[x==names(AODlist)])
} )

AOD.2002.monthly <- lapply(AOD.2002.monthly, function(element){
  summarise(group_by(element, Longitude, Latitude), across(everything(),mean))
})

AOD.2002.monthly <- lapply(AOD.2002.monthly, function(x){
  as.data.frame(x)
})

names(AOD.2002.monthly)<-paste0(month.name," 2002 AOD")
sapply(names(AOD.2002.monthly), 
       function (x) saveRDS(AOD.2002.monthly[[x]], file=paste(x, "RDS", sep=".") ))
######
setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
AODlist<-list()
list.filenames <- list.files()[grepl("2012",list.files(),fixed=T)]
for(i in 1:length(list.filenames)){
  AODlist[[i]] <- readRDS(list.filenames[i])
}

list.filenames <- as.Date(as.numeric(substr(list.filenames,6,8))-1,origin = as.Date("2012-01-01"))
names(AODlist) <- list.filenames
names(AODlist) <- substr(names(AODlist),6,7)

for (i in 1:length(AODlist)){
  names(AODlist[[i]]) <- c("Longitude","Latitude","Mean AOD")
}

AOD.2012.monthly <-lapply(unique(names(AODlist)), function(x){
  bind_rows(AODlist[x==names(AODlist)])
} )

AOD.2012.monthly <- lapply(AOD.2012.monthly, function(element){
  summarise(group_by(element, Longitude, Latitude), across(everything(),mean))
})

AOD.2012.monthly <- lapply(AOD.2012.monthly, function(x){
  as.data.frame(x)
})

names(AOD.2012.monthly)<-paste0(month.name," 2012 AOD")
sapply(names(AOD.2012.monthly), 
       function (x) saveRDS(AOD.2012.monthly[[x]], file=paste(x, "RDS", sep=".") ))
######
setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
AODlist<-list()
list.filenames <- list.files()[grepl("2022",list.files(),fixed=T)]
for(i in 1:length(list.filenames)){
  AODlist[[i]] <- readRDS(list.filenames[i])
}

list.filenames <- as.Date(as.numeric(substr(list.filenames,6,8))-1,origin = as.Date("2022-01-01"))
names(AODlist) <- list.filenames
names(AODlist) <- substr(names(AODlist),6,7)

for (i in 1:length(AODlist)){
  names(AODlist[[i]]) <- c("Longitude","Latitude","Mean AOD")
}

AOD.2022.monthly <-lapply(unique(names(AODlist)), function(x){
  bind_rows(AODlist[x==names(AODlist)])
} )

AOD.2022.monthly <- lapply(AOD.2022.monthly, function(element){
  summarise(group_by(element, Longitude, Latitude), across(everything(),mean))
})

AOD.2022.monthly <- lapply(AOD.2022.monthly, function(x){
  as.data.frame(x)
})

names(AOD.2022.monthly)<-paste0(month.name," 2022 AOD")
sapply(names(AOD.2022.monthly), 
       function (x) saveRDS(AOD.2022.monthly[[x]], file=paste(x, "RDS", sep=".") ))


#####
for (i in seq(1, length(list.AOD.files.55), 2)){
  print((i/2)+0.5)
  list.AOD.files.55.merged[[(i/2)+0.5]] <- merge(list.AOD.files.55[[i]],list.AOD.files.55[[i+1]])
}
days_2020 <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"),by=1)
names(list.AOD.files.55.merged) <- days_2020


list.AOD.files.55.merged.projected <- list()
for (i in 1:length(list.AOD.files.55.merged)){
  list.AOD.files.55.merged.projected[[i]] <- project( list.AOD.files.55.merged[[i]],
                                                      "+proj=longlat +datum=WGS84 +no_defs")
  print(i)
} 

list.AOD.files.55.merged.mask <- lapply(list.AOD.files.55.merged.projected, function(x){
  crop(x, cropLongLat_5c)
})

list.AOD.files.55.merged.mask <- lapply(list.AOD.files.55.merged.mask, function(x){
  mask(x, cropLongLat_5c)
})

names(list.AOD.files.55.merged.mask) <- days_2020

list.AOD.files.55.merged.mean <- lapply(list.AOD.files.55.merged.mask, function(x){
  mean(x, na.rm=TRUE)
})

list.AOD.files.55.merged.df <- lapply(list.AOD.files.55.merged.mean, function(x){
  as.data.frame(x,xy=TRUE)
})
names(list.AOD.files.55.merged.df) <- days_2020

setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
sapply(names(list.AOD.files.55.merged.df), 
       function (x) write.table(list.AOD.files.55.merged.df[[x]], file=paste(x, "txt", sep=".") ))

######Mapping processed files######
setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
setwd("/Users/ethan_j_li/Library/CloudStorage/OneDrive-EmoryUniversity/Processed AOD files")
filenames2002 <- list.files(pattern = "*2002 AOD.RDS")
filenames2022 <- list.files(pattern = "*2022 AOD.RDS")
filenames <- c(filenames2002,filenames2022)

AODlist <- list()
for (i in 1:length(filenames)){
  AODlist[[i]] <- readRDS(filenames[i])
  print(i)
}

names(AODlist) <- (filenames)
for(i in 1:length(AODlist)){
  AODlist[[i]]$origin <- names(AODlist[i])
}


for(i in 1:length(AODlist)){
coords <- st_as_sf(AODlist[[i]][,c(1,2)], coords = c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")
coords <- st_intersection(coords, st_difference(cropLongLat5c_small))
coords <- st_coordinates(coords)
coords <- as.data.frame(coords)
names(coords) <- c("Longitude","Latitude")
AODlist[[i]] <- merge(AODlist[[i]], coords, by = c("Longitude","Latitude"))
}


##
library(Metrics)
suppressMessages(library(spatstat))

obs_window_5c <- owin(xrange=st_bbox(cropLongLat_5c)[c(1,3)],yrange=st_bbox(cropLongLat_5c)[c(2,4)])
ppp_AOD <- ppp(AODlist[[1]]$Longitude,AODlist[[1]]$Latitude,
                 marks = AODlist[[1]]$`Mean AOD`,window=obs_window_5c)
powers <- seq(1,10,.1)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_AOD, power=power, at="points")
  mse_result <- c(mse_result,Metrics::mse(ppp_AOD$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

AODidw<-list()
for(i in 1:length(AODlist)){
  ppp_AOD <- ppp(AODlist[[i]]$Longitude,AODlist[[i]]$Latitude,
                   marks = AODlist[[i]]$`Mean AOD`,window=obs_window_5c)
  AODidw[[i]] <- idw(ppp_AOD, power=9.2, at="pixel")
  print(i)
}

AODidw <- lapply(AODidw,function(x){
  raster(x)
})

AODidw <- stack(AODidw)
names(AODidw) <- names(AODlist)
plot(AODidw$April.2022.AOD.RDS)
AODperiods <- str_sub(names(AODlist),1,-9)
AODperiods <- paste0(str_sub(AODperiods,1,-6),"/1/",str_sub(AODperiods,-4,-1))
AODperiods <- as.Date(AODperiods,format = "%B/%d/%Y")
AODidw <- setZ(AODidw,AODperiods,name="Date")
getZ(AODidw)

AODidw <- AODidw[[order(AODperiods)]]
AODperiods <- sort(AODperiods)
AODperiods<-format(AODperiods, "%Y %B")
names(AODidw) <- AODperiods
res(AODidw)
crs(AODidw) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(AODidw$X2022.January)

AODidw <- stackApply(AODidw,rep(1,12),mean)
plot(AODidw)
AOD5c <- crop(AODidw, cropLongLat5c_small)
AOD5c <- mask(AOD5c, cropLongLat5c_small)
plot(AOD5c)
res(AOD5c)
proj4string(AOD5c)
target <- raster(resolution=res(AOD5c)/10,crs=proj4string(AOD5c),ext=extent(AOD5c))
res(target)
AOD5c <- resample(AOD5c,target)
res(AOD5c)
plot(AOD5c)
#AOD5c <- setZ(AOD5c,AODperiods,name="Date")

AODZIP<- crop(AODidw,cropLongLatzip_small)
AODZIP <- mask(AODZIP, cropLongLatzip_small)
plot(AODZIP)
res(AODZIP)
proj4string(AODZIP) 
target <- raster(resolution=res(AODZIP)/10,crs=proj4string(AODZIP),ext=extent(AODZIP))
res(target)
AODZIP <- resample(AODZIP,target)
res(AODZIP)
plot(AODZIP)
AODZIP <- setZ(AODZIP,AODperiods,name="Date")
AODZIP <- mask(AODZIP, cropLongLatzip_small)
getZ(AODZIP)
######Generate AOD PNGs#####
#############################
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/AOD")
names(AOD5c)

AOD5c2002 <- subset(AOD5c,grep("2002",names(AOD5c),value=T))
AOD5c2002 <- setZ(AOD5c2002,AODperiods[1:12],"Period")

#AOD5c2012 <- subset(AOD5c,grep("2012",names(AOD5c),value=T))
#AOD5c2012 <- setZ(AOD5c2012,AODperiods[13:24],"Period")

AOD5c2022 <- subset(AOD5c,grep("2022",names(AOD5c),value=T))
AOD5c2022 <- setZ(AOD5c2022,AODperiods[13:24],"Period")

AODZIP2002<- subset(AODZIP,grep("2002",names(AODZIP),value=T))
AODZIP2002 <- setZ(AODZIP2002,AODperiods[1:12],"Period")

#AODZIP2012<- subset(AODZIP,grep("2012",names(AODZIP),value=T))
#AODZIP2012 <- setZ(AODZIP2012,AODperiods[13:24],"Period")

AODZIP2022<- subset(AODZIP,grep("2022",names(AODZIP),value=T))
AODZIP2022 <- setZ(AODZIP2022,AODperiods[13:24],"Period")

###2002 AOD
maplist <- list()
for(i in 1:nlayers(AOD5c)){
AOD5cPlot<- tm_shape(AOD5c[[i]])+
  tm_raster(col=names(AOD5c[[i]]),palette=airpolcolorvector,
            breaks =c(seq(min(values(AOD5c),na.rm=T),max(values(AOD5c),na.rm=T),0.075),
                      max(values(AOD5c),na.rm=T)),
            style = "cont",title = "Average \nAOD",legend.reverse=T)+
  #tm_facets(by="year")+
tm_shape(cropLongLat_5c)+
  tm_borders(col="Black", lwd = 2)+
  #tm_text("NAME",col = "white",shadow=T)+
tm_scale_bar(position = c(0.05,0.01))+
tm_compass(position = c(0.03,0.07))+
tm_grid(lines=FALSE)+
tm_layout(main.title=paste0("Five Counties\n",getZ(AOD5c)[i], " Average AOD"),
          main.title.size=1,
          main.title.position="center",
          #legend.outside = T
          legend.position=c("RIGHT","BOTTOM"),
          legend.title.size=1,
          legend.text.size=0.8,
          legend.height=-0.5
          )

AODZIPPlot<- tm_shape(AODZIP[[i]])+
  tm_raster(col=names(AODZIP[[i]]),palette=airpolcolorvector,
            breaks =c(seq(min(values(AOD5c),na.rm=T),max(values(AOD5c),na.rm=T),0.075),
                      max(values(AOD5c),na.rm=T)),
            style = "cont",title = "Average \nAOD",legend.reverse=T)+
  #tm_facets(by="year")+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="Black", lwd = 2)+
  #tm_text("NAME",col = "white",shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_grid(lines=FALSE)+
  tm_layout(main.title=paste0("Select ZIP Codes \n",getZ(AODZIP)[i], " Average AOD"),
            main.title.size=1,
            main.title.position="center",
            #legend.outside = T
            legend.position=c("RIGHT","TOP"),
            legend.title.size=1,
            legend.text.size=0.8,
            legend.height=-0.36)
maplist[[i]]<-tmap_arrange(AOD5cPlot,AODZIPPlot)
}
names(maplist) <- names(AOD5c)
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," AOD.png"),width=2500,height=2000)
}
maplist[[31]]

###2012 AOD
maplist <- list()
for(i in 1:nlayers(AOD5c2012)){
  AOD5cPlot<- tm_shape(AOD5c2012[[i]])+
    tm_raster(col=names(AOD5c2012[[i]]),palette=airpolcolorvector,
              breaks = c(seq(min(values(AOD5c2012),na.rm=T),max(values(AOD5c2012),na.rm=T),0.04),
                         max(values(AOD5c2012),na.rm=T)),
              style = "cont",title = "Average \nAOD",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties \n",getZ(AOD5c2012)[i], " Average AOD"),
              main.title.size=1,
              main.title.position="center",
              #legend.outside = T
              legend.position=c("RIGHT","BOTTOM"),
              legend.title.size=1,
              legend.text.size=0.8,
              legend.height=-0.5
    )
  
  AODZIPPlot<- tm_shape(AODZIP2012[[i]])+
    tm_raster(col=names(AODZIP2012[[i]]),palette=airpolcolorvector,
              breaks =c(seq(min(values(AOD5c2012),na.rm=T),max(values(AOD5c2012),na.rm=T),0.04),
                        max(values(AOD5c2012),na.rm=T)),
              style = "cont",title = "Average \nAOD",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes \n",getZ(AODZIP2012)[i], " Average AOD"),
              main.title.size=1,
              main.title.position="center",
              #legend.outside = T
              legend.position=c("RIGHT","TOP"),
              legend.title.size=1,
              legend.text.size=0.8,
              legend.height=-0.36)
  maplist[[i]]<-tmap_arrange(AOD5cPlot,AODZIPPlot)
}
names(maplist) <- names(AOD5c2012)
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," AOD.png"),width=2500,height=2000)
}

###2022 AOD
maplist <- list()
for(i in 1:nlayers(AOD5c2022)){
  AOD5cPlot<- tm_shape(AOD5c2022[[i]])+
    tm_raster(col=names(AOD5c2022[[i]]),palette=airpolcolorvector,
              breaks = c(seq(min(values(AOD5c2022),na.rm=T),max(values(AOD5c2022),na.rm=T),0.05),
                           max(values(AOD5c2022),na.rm=T)),
              style = "cont",title = "Average \nAOD",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties \n",getZ(AOD5c2022)[i], " Average AOD"),
              main.title.size=1,
              main.title.position="center",
              #legend.outside = T
              legend.position=c("RIGHT","BOTTOM"),
              legend.title.size=1,
              legend.text.size=0.8,
              legend.height=-0.5
    )
  
  AODZIPPlot<- tm_shape(AODZIP2022[[i]])+
    tm_raster(col=names(AODZIP2022[[i]]),palette=airpolcolorvector,
              breaks =  c(seq(min(values(AOD5c2022),na.rm=T),max(values(AOD5c2022),na.rm=T),0.05),
                          max(values(AOD5c2022),na.rm=T)),
              style = "cont",title = "Average \nAOD",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes \n",getZ(AODZIP2022)[i], " Average AOD"),
              main.title.size=1,
              main.title.position="center",
              #legend.outside = T
              legend.position=c("RIGHT","TOP"),
              legend.title.size=1,
              legend.text.size=0.8,
              legend.height=-0.36)
  maplist[[i]]<-tmap_arrange(AOD5cPlot,AODZIPPlot)
}
names(maplist) <- names(AOD5c2022)
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," AOD.png"),width=2500,height=2000)
}




####DayMet Data####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
list.files()

crop_parameters_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


daymetlist<-list()
list.filenames <- list.files()[grepl("-07-",list.files(),fixed=T)]
for(i in 1:length(list.filenames)){
  daymetlist[[i]] <- readRDS(list.filenames[i])
}
substr(list.filenames,8,17)
dates<-substr(list.filenames,8,17)
dates <- as.Date.character(dates)
names(daymetlist) <- dates
names(daymetlist)<-substr(names(daymetlist),1,4)
unique(names(daymetlist))

daymetlist.monthly <-lapply(unique(names(daymetlist)), function(x){
  bind_rows(daymetlist[x==names(daymetlist)])
} )

daymetlist.monthly<- lapply(daymetlist.monthly, function(element){
  summarise(group_by(element, Longitude, Latitude), across(everything(),mean))
})

daymetlist.monthly <- lapply(daymetlist.monthly, function(x){
  as.data.frame(x)
})

monthnames <- seq.Date(from = dates[1], length.out = 12, by = "month")
monthnames <- format(monthnames, "%B %Y")
names(daymetlist.monthly) <- monthnames

yearnames <-  seq.Date(from = dates[1], length.out = 21, by = "year")
yearnames <- format(yearnames, "%Y")
names(daymetlist.yearly) <- yearnames


daymetlist.yearly <- lapply(daymetlist.yearly, function(x){
  st_as_sf(x, coords = c("Longitude", "Latitude"), 
           crs= "+proj=longlat +datum=WGS84 +no_defs")
})

daymetlist.yearly <- lapply(daymetlist.yearly, function(x){
  st_intersection(x, st_difference(cropLongLat_5c))
})

for(i in 1:length(daymetlist.yearly)){
  daymetlist.yearly[[i]]$year <- names(daymetlist.yearly[i])
  daymetlist.yearly[[i]] <- daymetlist.yearly[[i]][,-which(names(daymetlist.yearly[[i]]) %in% 
                                                     c("NAME","FIPS","POP2010","WHITE","BLACK","NAME_U"))]
}

Summer <- bind_rows(daymetlist.yearly)

tm_shape(Summer)+
tm_dots(col = "tmean", size = 0.35, palette = tempcolorvector, 
          style = "cont",title = "Temperature °C",legend.col.reverse=T)+
  tm_facets(by="year")+
tm_shape(cropLongLat_5c)+
  tm_borders(col="Black", lwd = 2)+
  tm_text("NAME",col = "white",shadow=T)+
tm_scale_bar(position = c(0.05,0.01))+
tm_compass(position = c(0.03,0.07))+
tm_grid(lines=FALSE)+
tm_layout(title="July Average Temperature 2000-2020",legend.height=-0.32,legend.frame=T)
            
tractstemps <- st_join(daymet,tracts, join = st_nearest_feature)


####2003-2020 LST####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2003_01_01 - 2023_03_23/LST_Day_1km")
list.files()
substr(list.files(),21,28)
years<-substr(list.files(),21,24)
days <- substr(list.files(),26,28)
dates <- as.Date(as.numeric(days)-1,origin = paste0(years,"-01-01"))
dates
datesdf <- data.frame(years,days,dates)
datesdf[grepl("2003", as.character(dates), fixed=TRUE),]

length(grep("2010",list.files(),fixed=TRUE))
which((seq.Date(as.Date("2003-01-01"),as.Date("2003-12-31"),by=1) %in% datesdf$dates ) == FALSE)

tmap_options(check.and.fix = TRUE)
tm_shape(cropLongLat_5c)+
  tm_fill("NAME")+
  tm_borders()+
  tm_text("NAME")

#####Workshop NDVI#####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")
workshopNDVIfiles <- list.files()[c(grep("2002",list.files(),fixed=TRUE),grep("2012",list.files(),fixed=TRUE),grep("2022",list.files(),fixed=TRUE))]
NDVI <- stack(workshopNDVIfiles)
crs(NDVI)
res(NDVI)
str_sub(workshopNDVIfiles,-12,-9)
str_sub(workshopNDVIfiles,-7,-5)
NDVIdates <- format(as.Date(as.numeric(str_sub(workshopNDVIfiles,-7,-5))-1,
                     origin=paste0(str_sub(workshopNDVIfiles,-12,-9),"-01-01")),"%Y %b %d")
NDVIdates <- paste0(NDVIdates,"-",format(as.Date(NDVIdates,format = "%Y %b %d")+15,"%b %d"))
NDVIdates[23] <- "2002 Dec 19-Dec 31"
NDVIdates[46] <- "2012 Dec 18-Dec 31"
NDVIdates[69] <- "2022 Dec 19-Dec 31"
NDVI <- setZ(NDVI,NDVIdates,"Period")
names(NDVI) <- NDVIdates

plot(NDVI$Jan.01.2002.Jan.16.2002)
target <- raster(resolution=res(NDVI)/2,crs=proj4string(NDVI),ext=extent(NDVI))
res(target)
NDVI <- resample(NDVI,target)
res(NDVI)
crs(NDVI)
plot(NDVI$Jan.01.2002.Jan.16.2002)


NDVI5c <- crop(NDVI,cropLongLat_5c)
NDVI5c <- mask(NDVI5c,cropLongLat_5c)
NDVI5c <- setZ(NDVI5c,NDVIdates,"Period")
plot(NDVI5c$Jan.01.2002.Jan.16.2002)

NDVIZIP <- crop(NDVI,cropLongLat_zip)
NDVIZIP <- mask(NDVIZIP,cropLongLat_zip)
plot(NDVIZIP$Jan.01.2002.Jan.16.2002)
res(NDVIZIP)
crs(NDVIZIP)
target <- raster(resolution=res(NDVIZIP)/2.5,crs=proj4string(NDVIZIP),ext=extent(NDVIZIP))
res(target)
NDVIZIP <- resample(NDVIZIP,target)
res(NDVIZIP)
crs(NDVIZIP)
NDVIZIP <- mask(NDVIZIP,cropLongLat_zip)
NDVIZIP <- setZ(NDVIZIP,NDVIdates,"Period")
plot(NDVIZIP$Jan.01.2002.Jan.16.2002)


######Generate Workshop NDVI PNGs######
################
values(NDVIZIP) <- values(NDVIZIP)*0.0001
values(NDVI5c) <- values(NDVI5c)*0.0001
maplist <- list()

for(i in 1:length(NDVIdates)){
  NDVI5cPlot<- tm_shape(NDVI5c[[i]])+
    tm_raster(col=names(NDVI5c[[i]]),palette = NDVIcolorvector,
              breaks =c(seq(min(values(NDVI5c),na.rm=T),max(values(NDVI5c),na.rm=T),0.2),
                        max(values(NDVI5c),na.rm=T)),
              style = "cont",title = "NDVI",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties\n",getZ(NDVI5c[[i]]), " NDVI"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size=0.8,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  NDVIZIPPlot<- tm_shape(NDVIZIP[[i]])+
    tm_raster(col=names(NDVIZIP[[i]]),palette=NDVIcolorvector ,
              breaks =c(seq(min(values(NDVI5c),na.rm=T),max(values(NDVI5c),na.rm=T),0.2),
                        max(values(NDVI5c),na.rm=T)),
              style = "cont",title = "NDVI",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes\n",getZ(NDVIZIP[[i]]), " NDVI"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.32,
              legend.frame=F,
              legend.text.size = 0.8,
              legend.position=c("RIGHT","TOP"))
  maplist[[i]]<-tmap_arrange(NDVI5cPlot,NDVIZIPPlot)
}
names(maplist) <- NDVIdates

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/NDVI")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," NDVI.png"),width=2500,height=2000)
}

maplist[[9]]
NDVIshp<- NDVI5c[[13]]
res(NDVIshp)
target <-  raster(resolution=res(NDVIshp)*7.6,crs=proj4string(NDVIshp),ext=extent(NDVIshp))
NDVIshp <- resample(NDVIshp,target)
polys1 <- rasterToPolygons(NDVIshp)
names(polys1)
cols = rev(terrain.colors(255))
spplot(polys1, "Jul.12.2002.Jul.27.2002", col.regions=cols, lwd=0)
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Five Counties Daymet Shapefiles")
raster::shapefile(polys1, "Five Counties 2002 Jul 12-Jul 27 NDVI.shp")
test <- st_read("Five Counties 2002 Jul 12-Jul 27 NDVI.shp")


#####Workshop LST#######
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2002_01_01 - 2023_03_23/LST_Day_1km")
workshopLSTfiles <- list.files()[c(grep("2002",list.files(),fixed=TRUE))]
which(!(1:365 %in% as.numeric(str_sub(workshopLSTfiles,-7,-5))))
LST <- stack(workshopLSTfiles)
crs(LST)
res(LST)
str_sub(workshopLSTfiles,-12,-9)
str_sub(workshopLSTfiles,-7,-5)
LSTdates <- as.Date(as.numeric(str_sub(workshopLSTfiles,-7,-5))-1,
                    origin=paste0(str_sub(workshopLSTfiles,-12,-9),"-01-01"))
#LSTdates <- paste0(LSTdates,"--",LSTdates+15)
names(LST) <- LSTdates

indices <- seq(1,365,by=16)
indices <- rep(indices,each=16)[1:365]
indices <- indices[-c(79,80,81,82,83,84,85,86,87,105)]
LST2002 <- stackApply(LST,indices,fun=mean,na.rm=T)


workshopLSTfiles <- list.files()[c(grep("2012",list.files(),fixed=TRUE))]
which(!(1:366 %in% as.numeric(str_sub(workshopLSTfiles,-7,-5))))
LST <- stack(workshopLSTfiles)
crs(LST)
res(LST)
str_sub(workshopLSTfiles,-12,-9)
str_sub(workshopLSTfiles,-7,-5)
LSTdates <- as.Date(as.numeric(str_sub(workshopLSTfiles,-7,-5))-1,origin=paste0(str_sub(workshopLSTfiles,-12,-9),"-01-01"))
#LSTdates <- paste0(LSTdates,"--",LSTdates+15)
names(LST) <- LSTdates

indices <- seq(1,366,by=16)
indices <- rep(indices,each=16)[1:366]
LST2012 <- stackApply(LST,indices,fun=mean,na.rm=T)


workshopLSTfiles <- list.files()[c(grep("2022",list.files(),fixed=TRUE))]
which(!(1:365 %in% as.numeric(str_sub(workshopLSTfiles,-7,-5))))
LST <- stack(workshopLSTfiles)
crs(LST)
res(LST)
str_sub(workshopLSTfiles,-12,-9)
str_sub(workshopLSTfiles,-7,-5)
LSTdates <- as.Date(as.numeric(str_sub(workshopLSTfiles,-7,-5))-1,origin=paste0(str_sub(workshopLSTfiles,-12,-9),"-01-01"))
#LSTdates <- paste0(LSTdates,"--",LSTdates+15)
names(LST) <- LSTdates

indices <- seq(1,365,by=16)
indices <- rep(indices,each=16)[1:365]
indices <- indices[-which(!(1:365 %in% as.numeric(str_sub(workshopLSTfiles,-7,-5))))]
LST2022 <- stackApply(LST,indices,fun=mean,na.rm=T)

LST <- stack(LST2002,LST2012,LST2022)
target <- raster(resolution=res(LST)/4,crs=proj4string(LST),ext=extent(LST))
res(target)
LST <- resample(LST,target)
res(LST)
crs(LST)
plot(LST[[1]])


LST5c <- crop(LST,cropLongLat_5c)
LST5c <- mask(LST5c,cropLongLat_5c)
plot(LST5c[[1]])

LSTZIP <- crop(LST,cropLongLat_zip)
LSTZIP <- mask(LSTZIP,cropLongLat_zip)
plot(LSTZIP[[1]])
res(LSTZIP)
crs(LSTZIP)
target <- raster(resolution=res(LSTZIP)/2.5,crs=proj4string(LSTZIP),ext=extent(LSTZIP))
res(target)
LSTZIP <- resample(LSTZIP,target)
res(LSTZIP)
crs(LSTZIP)
LSTZIP <- mask(LSTZIP,cropLongLat_zip)
plot(LSTZIP[[1]])

periods2002<-paste0(format(seq(as.Date("2002/1/1"), as.Date("2002/12/31"),by=16),"%Y %b %d"),"-",
                    format(seq(as.Date("2002/1/1"), as.Date("2002/12/31"),by=16)+15,"%b %d"))
periods2012<-paste0(format(seq(as.Date("2012/1/1"), as.Date("2012/12/31"),by=16),"%Y %b %d"),"-",
                    format(seq(as.Date("2012/1/1"), as.Date("2012/12/31"),by=16)+15,"%b %d"))
periods2022<-paste0(format(seq(as.Date("2022/1/1"), as.Date("2022/12/31"),by=16),"%Y %b %d"),"-",
                    format(seq(as.Date("2022/1/1"), as.Date("2022/12/31"),by=16)+15,"%b %d"))
periods2002[23]<-"2002 Dec 19-Dec 31"
periods2012[23]<-"2012 Dec 18-Dec 31"
periods2022[23]<-"2022 Dec 19-Dec 31"
periods <- c(periods2002,periods2012,periods2022)
periods<-periods[-c(67,68,69)]
periods[66] <- "2022 Nov 01-Nov 15"
names(LST5c) <- periods
LST5c <- setZ(LST5c,periods,"Period")
names(LSTZIP) <- periods
LSTZIP <- setZ(LSTZIP,periods,"period")

########Generate LST PNGs######
#############

values(LSTZIP) <- (values(LSTZIP)*0.02)-273
values(LST5c) <- (values(LST5c)*0.02)-273
values(LSTZIP) <- (values(LSTZIP)*(9/5))+32
values(LST5c) <- (values(LST5c)*(9/5))+32
maplist <- list()

for(i in 1:nlayers(LST5c)){
  LST5cPlot<- tm_shape(LST5c[[i]])+
    tm_raster(col=names(LST5c[[i]]),palette = tempcolorvector,
              breaks =c(seq(min(values(LST5c),na.rm=T),max(values(LST5c),na.rm=T),8),
                        max(values(LST5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties\n",getZ(LST5c[[i]]), " Land Surface Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size=0.8,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  LSTZIPPlot<-tm_shape(LSTZIP[[i]])+
    tm_raster(col=names(LSTZIP[[i]]),palette=tempcolorvector ,
              breaks =c(seq(min(values(LST5c),na.rm=T),max(values(LST5c),na.rm=T),8),
                        max(values(LST5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes\n",getZ(LSTZIP[[i]]), " Land Surface Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","TOP"))
  maplist[[i]]<-tmap_arrange(LST5cPlot,LSTZIPPlot)
}
names(maplist) <- periods

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/MODIS LST")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," MODIS LST.png"),width=2500,height=2000)
}

maplist[[66]]

#######Workshop Daymet######
#########
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
workshopDaymet <- list.files()[c(grep("-06-",list.files(),fixed=TRUE),
                                 grep("-07-",list.files(),fixed=TRUE),
                                 grep("-08-",list.files(),fixed=TRUE))]
daymetfiles <- list()
for(i in 1:length(workshopDaymet)){
  daymetfiles[[i]]<-readRDS(workshopDaymet[i])
}

daymetfiles <- lapply(daymetfiles, function(x){
  subset(x,select=c("Latitude","Longitude","tmin","tmax","tmean","RH"))
})

names(daymetfiles) <- str_sub(workshopDaymet,1,-8)
unique(names(daymetfiles))

daymetfiles <-lapply(unique(names(daymetfiles)), function(x){
  bind_rows(daymetfiles[x==names(daymetfiles)])
} )

###Generate tmax
##
daymetfilestmax<- lapply(daymetfiles, function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),max))
})

daymetfilestmax<- lapply(daymetfilestmax, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmax")))
})


###Generate tmin
##
daymetfilestmin<- lapply(daymetfiles, function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),min))
})

daymetfilestmin<- lapply(daymetfilestmin, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmin")))
})


###Generate tmean
##
daymetfilestmean<- lapply(daymetfiles, function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),mean))
})

daymetfilestmean<- lapply(daymetfilestmean, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmean")))
})


###Generate RH
##
daymetfilesRH<- lapply(daymetfiles, function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),median))
})

daymetfilesRH<- lapply(daymetfilesRH, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","RH")))
})

names(daymetfilestmax) <- unique(str_sub(workshopDaymet,1,-8))
names(daymetfilestmin) <- unique(str_sub(workshopDaymet,1,-8))
names(daymetfilestmean) <- unique(str_sub(workshopDaymet,1,-8))
names(daymetfilesRH) <- unique(str_sub(workshopDaymet,1,-8))

rm(daymetfiles)

library(Metrics)
library(gstat)
suppressMessages(library(spatstat))
bbox <- st_bbox(cropLongLat5c_small)
obs_window_5c <- owin(xrange=bbox[c(1,3)],yrange=bbox[c(2,4)])


######tmin#####
for(i in 1:length(daymetfilestmin)){
  coords <- st_as_sf(daymetfilestmin[[i]][,c(1,2)], coords = c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")
  coords <- st_intersection(coords, st_difference(cropLongLat5c_small))
  coords <- st_coordinates(coords)
  coords <- as.data.frame(coords)
  names(coords) <- c("Longitude","Latitude")
  daymetfilestmin[[i]] <- merge(daymetfilestmin[[i]], coords, by = c("Longitude","Latitude"))
}


library(Metrics)
suppressMessages(library(spatstat))

ppp_5c <- ppp(daymetfilestmin[[1]]$Longitude,daymetfilestmin[[1]]$Latitude,
                     marks = daymetfilestmin[[1]]$tmin,window=obs_window_5c)


powers <- seq(6,8,0.1)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_5c, power=power, at="points")
  mse_result <- c(mse_result,Metrics::mse(ppp_5c$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

for(i in 1:length(daymetfilestmin)){
  ppp_5c <- ppp(daymetfilestmin[[i]]$Longitude,daymetfilestmin[[i]]$Latitude,
                marks = daymetfilestmin[[i]]$tmin,window=obs_window_5c)
  daymetfilestmin[[i]]<- idw(ppp_5c, power=6.4, at="pixels")
  print(i)
}

daymetfilestmin_df <- daymetfilestmin
daymetfilestmin <- lapply(daymetfilestmin,function(x){
  raster(x)
})

daymetfilestmin <- stack(daymetfilestmin)
target <- raster(resolution=res(daymetfilestmin)/4,crs=crs(cropLongLat5c_small),
                 ext=extent(cropLongLat5c_small), vals=0)
daymetfilestmin <- resample(daymetfilestmin,target)

res(daymetfilestmin)
crs(daymetfilestmin)
plot(daymetfilestmin[[1]])

daymetfilestmin5c <- mask(daymetfilestmin,cropLongLat5c_small)
plot(daymetfilestmin5c[[52]])

daymetfilestminzip <- crop(daymetfilestmin,cropLongLatzip_small)
daymetfilestminzip <- mask(daymetfilestminzip, cropLongLatzip_small)
plot(daymetfilestminzip[[1]])
target <-  raster(resolution=res(daymetfilestminzip)/2.5,crs=crs(daymetfilestminzip),
                   ext=extent(daymetfilestminzip), vals=0)
daymetfilestminzip <- resample(daymetfilestminzip,target)
plot(daymetfilestminzip[[1]])
daymetfilestminzip <- mask(daymetfilestminzip, cropLongLat_zip)
plot(daymetfilestminzip[[1]])

###Tmin time series
daymetfilestmin5c
periodvector <- NULL
mintempvector <- NULL

for(i in 1:nlayers(daymetfilestmin5c)){
  periodvector <- c(periodvector,getZ(daymetfilestmin5c[[i]]))
  mintempvector <- c(mintempvector, min(values(daymetfilestmin5c[[i]]),na.rm=T))
  
}

mintempdf <- as.data.frame(cbind(periodvector,mintempvector,period))
names(mintempdf) <- c("Period","MinTemp","Period2")
mintempdf$MinTemp <- as.numeric(mintempdf$MinTemp)
mintempdf$Period2 <- as.Date(as.numeric(mintempdf$Period2),origin = "1970-01-01")

ggplot(data=mintempdf, aes(x=Period2, y=MinTemp))+
  geom_line()+
  scale_x_discrete(labels=mintempdf$Period)
  

###Tmin PNGs

values(daymetfilestmin5c) <- (values(daymetfilestmin5c)*(9/5))+32
values(daymetfilestminzip) <- (values(daymetfilestminzip)*(9/5))+32
maplist <- list()
period <- names(daymetfilestmin5c)
period <- str_sub(period,8,14)
period <- paste0(str_sub(period,6,7),"/01/",str_sub(period,1,4))
period <- as.Date(period, "%m/%d/%Y")
period <- format(period, "%B %Y")
daymetfilestmin5c <- setZ(daymetfilestmin5c,period,"Period")
daymetfilestminzip <- setZ(daymetfilestminzip,period,"Period")

for(i in 1:nlayers(daymetfilestmin5c)){
  DaymetTmin5cPlot<- tm_shape(daymetfilestmin5c[[i]])+
    tm_raster(col=names(daymetfilestmin5c[[i]]),palette = tempcolorvector,
              breaks =c(seq(min(values(daymetfilestmin5c),na.rm=T),max(values(daymetfilestmin5c),na.rm=T),2.5),
                        max(values(daymetfilestmin5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat5c_small)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties\n",getZ(daymetfilestmin5c[[i]]), " Minimum Air Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  DaymetTminZIPPlot<-tm_shape(daymetfilestminzip[[i]])+
    tm_raster(col=names(daymetfilestminzip[[i]]),palette=tempcolorvector ,
              breaks =c(seq(min(values(daymetfilestmin5c),na.rm=T),max(values(daymetfilestmin5c),na.rm=T),2.5),
                        max(values(daymetfilestmin5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes\n",getZ(daymetfilestminzip[[i]]), " Minimum Air Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size=0.8,
              legend.frame=F,
              legend.position=c("RIGHT","TOP"))
  maplist[[i]]<-tmap_arrange(DaymetTmin5cPlot,DaymetTminZIPPlot)
}
names(maplist) <- period

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/DayMet Tmin")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," Tmin.png"),width=2500,height=2000)
}

maplist[[63]]

######tmax#####
for(i in 1:length(daymetfilestmax)){
  coords <- st_as_sf(daymetfilestmax[[i]][,c(1,2)], coords = c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")
  coords <- st_intersection(coords, st_difference(cropLongLat5c_small))
  coords <- st_coordinates(coords)
  coords <- as.data.frame(coords)
  names(coords) <- c("Longitude","Latitude")
  daymetfilestmax[[i]] <- merge(daymetfilestmax[[i]], coords, by = c("Longitude","Latitude"))
}

library(Metrics)
suppressMessages(library(spatstat))

ppp_5c <- ppp(daymetfilestmax[[1]]$Longitude,daymetfilestmax[[1]]$Latitude,
              marks = daymetfilestmax[[1]]$tmax,window=obs_window_5c)

powers <- seq(7.2,7.4,0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_5c, power=power, at="points")
  mse_result <- c(mse_result,Metrics::mse(ppp_5c$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

for(i in 1:length(daymetfilestmax)){
  ppp_5c <- ppp(daymetfilestmax[[i]]$Longitude,daymetfilestmax[[i]]$Latitude,
                marks = daymetfilestmax[[i]]$tmax,window=obs_window_5c)
  daymetfilestmax[[i]]<- idw(ppp_5c, power=7.52, at="pixels")
  print(i)
}

daymetfilestmaxdf <- daymetfilestmax
daymetfilestmax <- lapply(daymetfilestmax,function(x){
  raster(x)
})

daymetfilestmax <- stack(daymetfilestmax)
target <- raster(resolution=res(daymetfilestmax)/4,crs=crs(cropLongLat5c_small),
                 ext=extent(cropLongLat5c_small), vals=0)
daymetfilestmax <- resample(daymetfilestmax,target)

res(daymetfilestmax)
crs(daymetfilestmax)
plot(daymetfilestmax[[1]])
daymetfilestmax5c <- mask(daymetfilestmax,cropLongLat5c_small)
plot(daymetfilestmax5c[[52]])

daymetfilestmaxzip <- crop(daymetfilestmax,cropLongLat_zip)
daymetfilestmaxzip <- mask(daymetfilestmaxzip, cropLongLat_zip)
plot(daymetfilestmaxzip[[1]])
target <-  raster(resolution=res(daymetfilestmaxzip)/2.5,crs=crs(daymetfilestmaxzip),
                   ext=extent(daymetfilestmaxzip), vals=0)
daymetfilestmaxzip <- resample(daymetfilestmaxzip,target)
plot(daymetfilestmaxzip[[1]])
daymetfilestmaxzip <- mask(daymetfilestmaxzip, cropLongLat_zip)
###Tmax PNGs

values(daymetfilestmax5c) <- (values(daymetfilestmax5c)*(9/5))+32
values(daymetfilestmaxzip) <- (values(daymetfilestmaxzip)*(9/5))+32
maplist <- list()
period <- names(daymetfilestmax5c)
period <- str_sub(period,8,14)
period <- paste0(str_sub(period,6,7),"/01/",str_sub(period,1,4))
period <- as.Date(period, "%m/%d/%Y")
period <- format(period, "%B %Y")
daymetfilestmax5c <- setZ(daymetfilestmax5c,period,"Period")
daymetfilestmaxzip <- setZ(daymetfilestmaxzip,period,"Period")

for(i in 1:nlayers(daymetfilestmax5c)){
  DaymetTmax5cPlot<- tm_shape(daymetfilestmax5c[[i]])+
    tm_raster(col=names(daymetfilestmax5c[[i]]),palette = tempcolorvector,
              breaks =c(seq(min(values(daymetfilestmax5c),na.rm=T),max(values(daymetfilestmax5c),na.rm=T),2),
                        max(values(daymetfilestmax5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties\n",getZ(daymetfilestmax5c[[i]]), " Maximum Air Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  DaymetTmaxZIPPlot<-tm_shape(daymetfilestmaxzip[[i]])+
    tm_raster(col=names(daymetfilestmaxzip[[i]]),palette=tempcolorvector ,
              breaks=c(seq(min(values(daymetfilestmax5c),na.rm=T),max(values(daymetfilestmax5c),na.rm=T),2),
                max(values(daymetfilestmax5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes\n",getZ(daymetfilestmaxzip[[i]]), " Maximum Air Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","TOP"))
  maplist[[i]]<-tmap_arrange(DaymetTmax5cPlot,DaymetTmaxZIPPlot)
}
names(maplist) <- period

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/Daymet Tmax")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," Tmax.png"),width=2500,height=2000)
}

maplist[[10]]


######tmean#####
for(i in 1:length(daymetfilestmean)){
  coords <- st_as_sf(daymetfilestmean[[i]][,c(1,2)], coords = c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")
  coords <- st_intersection(coords, st_difference(cropLongLat5c_small))
  coords <- st_coordinates(coords)
  coords <- as.data.frame(coords)
  names(coords) <- c("Longitude","Latitude")
  daymetfilestmean[[i]] <- merge(daymetfilestmean[[i]], coords, by = c("Longitude","Latitude"))
}

library(Metrics)
suppressMessages(library(spatstat))

daymetfilestmeandf <- daymetfilestmean
ppp_5c <- ppp(daymetfilestmean[[1]]$Longitude,daymetfilestmean[[1]]$Latitude,
              marks = daymetfilestmean[[1]]$tmean,window=obs_window_5c)

powers <- seq(7.2,7.4,0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_5c, power=power, at="points")
  mse_result <- c(mse_result,Metrics::mse(ppp_5c$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

for(i in 1:length(daymetfilestmean)){
  ppp_5c <- ppp(daymetfilestmean[[i]]$Longitude,daymetfilestmean[[i]]$Latitude,
                marks = daymetfilestmean[[i]]$tmean,window=obs_window_5c)
  daymetfilestmean[[i]]<- idw(ppp_5c, power=7.5, at="pixels")
  print(i)
}

daymetfilestmean <- lapply(daymetfilestmean,function(x){
  raster(x)
})

daymetfilestmean <- stack(daymetfilestmean)
target <- raster(resolution=res(daymetfilestmean)/4,crs=crs(cropLongLat5c_small),
                 ext=extent(cropLongLat5c_small), vals=0)
daymetfilestmean <- resample(daymetfilestmean,target)

res(daymetfilestmean)
crs(daymetfilestmean)
plot(daymetfilestmean[[1]])
daymetfilestmean5c <- mask(daymetfilestmean,cropLongLat5c_small)
plot(daymetfilestmean5c[[1]])

daymetfilestmeanzip <- crop(daymetfilestmean,cropLongLat_zip)
daymetfilestmeanzip <- mask(daymetfilestmeanzip, cropLongLat_zip)
plot(daymetfilestmeanzip[[1]])
target <-  raster(resolution=res(daymetfilestmeanzip)/2.5,crs=crs(daymetfilestmeanzip),
                   ext=extent(daymetfilestmeanzip), vals=0)
daymetfilestmeanzip <- resample(daymetfilestmeanzip,target)
plot(daymetfilestmeanzip[[1]])

###Tmean PNGs

values(daymetfilestmean5c) <- (values(daymetfilestmean5c)*(9/5))+32
values(daymetfilestmeanzip) <- (values(daymetfilestmeanzip)*(9/5))+32
maplist <- list()
period <- names(daymetfilestmean5c)
period <- str_sub(period,8,14)
period <- paste0(str_sub(period,6,7),"/01/",str_sub(period,1,4))
period <- as.Date(period, "%m/%d/%Y")
period <- format(period, "%B %Y")
daymetfilestmean5c <- setZ(daymetfilestmean5c,period,"Period")
daymetfilestmeanzip <- setZ(daymetfilestmeanzip,period,"Period")

tmean5cjune <- subset(daymetfilestmean5c,grep(".06",names(daymetfilestmean5c),value=T))
tmean5cjune<- tmean5cjune[[1:21]]
tmeanzipjune <- subset(daymetfilestmeanzip,grep(".06",names(daymetfilestmeanzip),value=T))
tmeanzipjune<- daymetfilestmeanzip[[1:21]]

maplist<-list()
for(i in 1:nlayers(daymetfilestmean5c)){
  DaymetTmean5cPlot<- tm_shape(daymetfilestmean5c[[i]])+
    tm_raster(col=names(daymetfilestmean5c[[i]]),palette = tempcolorvector,
              breaks =c(seq(min(values(daymetfilestmean5c),na.rm=T),max(values(daymetfilestmean5c),na.rm=T),1.5),
                        max(values(daymetfilestmean5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties\n",getZ(daymetfilestmean5c[[i]]), " Average Air Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  DaymetTmeanZIPPlot<- tm_shape(daymetfilestmeanzip[[i]])+
    tm_raster(col=names(daymetfilestmeanzip[[i]]),palette=tempcolorvector ,
              breaks =c(seq(min(values(daymetfilestmean5c),na.rm=T),max(values(daymetfilestmean5c),na.rm=T),1.5),
                        max(values(daymetfilestmean5c),na.rm=T)),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes\n",getZ(daymetfilestmeanzip[[i]]), " Average Air Temperature"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","TOP"))
  maplist[[i]]<-tmap_arrange(DaymetTmean5cPlot,DaymetTmeanZIPPlot)
}
names(maplist) <- period

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/Daymet Tmean")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," Tmean.png"),width=2500,height=2000)
}

maplist[[42]]
dev.off()

for(i in 1:length(period)){
  DaymetTmean5cPlot<- tm_shape(daymetfilestmean5c[[i]])+
    tm_raster(col=names(daymetfilestmean5c[[i]]),palette = tempcolorvector,
              breaks = seq(71.36575,83.86286,0.8),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties ",period[i], " Average Air Temperature"),main.title.size=1,legend.height=-0.32,
              legend.frame=F,legend.position=c("RIGHT","BOTTOM"),main.title.position="center")
  
  DaymetTmeanZIPPlot<- tm_shape(daymetfilestmeanzip[[i]])+
    tm_raster(col=names(daymetfilestmeanzip[[i]]),palette=tempcolorvector ,
              breaks = seq(71.36575,83.86286,0.8),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes ",period[i], " Average Air Temperature"),main.title.size=1,legend.height=-0.32,
              legend.frame=F,legend.position=c("RIGHT","TOP"),main.title.position="center")
  maplist[[i]]<-tmap_arrange(DaymetTmean5cPlot,DaymetTmeanZIPPlot)
}
names(maplist) <- period

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/Daymet Tmean")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," Tmean.png"),width=4000)
}

maplist[[42]]

#####Subset tmean to 75th percentile
meanvalues <- values(daymetfilestmean5c)
meanvalues <- as.data.frame(meanvalues)
apply(meanvalues[1:63],2,quantile,probs=.75,na.rm=T)

daymetfilestmean5c75 <- daymetfilestmean5c

for (i in 1:nlayers(daymetfilestmean5c)){
  cutoff<-(quantile(values(daymetfilestmean5c[[i]]),.9,na.rm=T))
  cutoff <- as.numeric(cutoff)
  daymetfilestmean5c75[[i]][daymetfilestmean5c75[[i]]<cutoff]<-NA
}

plot(daymetfilestmean5c75)
extent(daymetfilestmean5c75)
mean90df <- values(daymetfilestmean5c75)
mean90df <- as.data.frame(mean90df)
names(mean90df) <- getZ(daymetfilestmean5c75)
mean90df <- apply(mean90df, 2,mean,na.rm=T)
mean90df <- as.data.frame(mean90df)
mean90df$date <- rownames(mean90df)
mean90df$date <- paste0(mean90df$date," 01")
mean90df$date <- as.Date.character(mean90df$date,format = "%B %Y %d")
mean90df$formatdate <- format(mean90df$date,"%B %Y")
p <-
 ggplot() +
  geom_line(mean90df[c(1:21),], mapping= aes(x=date, y=mean90df))+
  geom_point(mean90df[c(1:21),], mapping= aes(x=date, y=mean90df))+
  geom_col(data = mean90df[c(1:21),], mapping=aes(x=date,y=mean90df),alpha=0.5,width=100)+
  scale_x_date(breaks= mean90df[c(1:21),2], date_labels = "%B %Y",guide = guide_axis(angle = 45))+
  xlab("Period")+
  ylab("Temperature °F")+
  ggtitle("Average 90th Percentile June Temperatures")+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

######RH#####
for(i in 1:length(daymetfilesRH)){
  coords <- st_as_sf(daymetfilesRH[[i]][,c(1,2)], coords = c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")
  coords <- st_intersection(coords, st_difference(cropLongLat5c_small))
  coords <- st_coordinates(coords)
  coords <- as.data.frame(coords)
  names(coords) <- c("Longitude","Latitude")
  daymetfilesRH[[i]] <- merge(daymetfilesRH[[i]], coords, by = c("Longitude","Latitude"))
}

ppp_5c <- ppp(daymetfilesRH[[1]]$Longitude,daymetfilesRH[[1]]$Latitude,
              marks = daymetfilesRH[[1]]$RH,window=obs_window_5c)

powers <- seq(5.9,6.1,0.01)
mse_result <- NULL
for(power in powers){
  CV_idw <- idw(ppp_5c, power=power, at="points")
  mse_result <- c(mse_result,Metrics::mse(ppp_5c$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result)]
optimal_power

for(i in 1:length(daymetfilesRH)){
  ppp_5c <- ppp(daymetfilesRH[[i]]$Longitude,daymetfilesRH[[i]]$Latitude,
                marks = daymetfilesRH[[i]]$RH,window=obs_window_5c)
  daymetfilesRH[[i]]<- idw(ppp_5c, power=6.3, at="pixels")
  print(i)
}

daymetfilesRH <- lapply(daymetfilesRH,function(x){
  raster(x)
})

daymetfilesRH <- stack(daymetfilesRH)
target <- raster(resolution=res(daymetfilesRH)/4,crs=crs(cropLongLat5c_small),
                 ext=extent(cropLongLat5c_small), vals=0)
daymetfilesRH <- resample(daymetfilesRH,target)

res(daymetfilesRH)
crs(daymetfilesRH)
plot(daymetfilesRH[[1]])
daymetfilesRH5c <- mask(daymetfilesRH,cropLongLat5c_small)
plot(daymetfilesRH5c[[1]])

daymetfilesRHzip <- crop(daymetfilesRH,cropLongLat_zip)
daymetfilesRHzip <- mask(daymetfilesRHzip, cropLongLat_zip)
plot(daymetfilesRHzip[[1]])
target <-  raster(resolution=res(daymetfilesRHzip)/2.5,crs=crs(daymetfilesRHzip),
                   ext=extent(daymetfilesRHzip), vals=0)
daymetfilesRHzip <- resample(daymetfilesRHzip,target)
plot(daymetfilesRHzip[[1]])

###Tmax PNGs
maplist <- list()

period <- names(daymetfilesRH5c)
period <- str_sub(period,8,14)
period <- paste0(str_sub(period,6,7),"/01/",str_sub(period,1,4))
period <- as.Date(period, "%m/%d/%Y")
period <- format(period, "%B %Y")
daymetfilesRH5c <- setZ(daymetfilesRH5c,period,"Period")
daymetfilesRHzip <- setZ(daymetfilesRHzip,period,"Period")

maplist<-list()
for(i in 1:nlayers(daymetfilesRH5c)){
  DaymetRH5cPlot<- tm_shape(daymetfilesRH5c[[i]])+
    tm_raster(col=names(daymetfilesRH5c[[i]]),palette = humiditycolorvector,
              breaks =c(seq(min(values(daymetfilesRH5c),na.rm=T),max(values(daymetfilesRH5c),na.rm=T),5),
                        max(values(daymetfilesRH5c),na.rm=T)),
              style = "cont",title = "RH(%)",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_5c)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Five Counties\n",getZ(daymetfilesRH5c[[i]]), " Median Relative Humidity"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  DaymetRHZIPPlot<- tm_shape(daymetfilesRHzip[[i]])+
    tm_raster(col=names(daymetfilesRHzip[[i]]),palette=humiditycolorvector ,
              breaks =c(seq(min(values(daymetfilesRH5c),na.rm=T),max(values(daymetfilesRH5c),na.rm=T),5),
                        max(values(daymetfilesRH5c),na.rm=T)),
              style = "cont",title = "RH(%)",legend.reverse=T)+
    #tm_facets(by="year")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="Black", lwd = 2)+
    #tm_text("NAME",col = "white",shadow=T)+
    tm_scale_bar(position = c(0.03,0.01))+
    tm_compass(position = c(0.03,0.07))+
    tm_grid(lines=FALSE)+
    tm_layout(main.title=paste0("Select ZIP Codes\n",getZ(daymetfilesRHzip[[i]]), " Median Relative Humidity"),
              main.title.size=1,
              main.title.position="center",
              legend.height=-0.4,
              legend.text.size = 0.8,
              legend.frame=F,
              legend.position=c("RIGHT","TOP"))
  maplist[[i]]<-tmap_arrange(DaymetRH5cPlot,DaymetRHZIPPlot)
}
names(maplist) <- period

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/workshop_posters/DayMet RH")
for(i in 1:length(maplist)){
  tmap_save(maplist[[i]],filename = paste0(names(maplist)[i]," RH.png"),width=2500,height=2000)
}

maplist[[21]]

#####Google Earth
zipshp <- st_combine(cropLongLatzip_small)
zipshp <- st_make_valid(zipshp)
atlshp <- st_combine(cropLongLat5c_small)
atlshp <- st_make_valid(atlshp)
zip_3857 <- st_transform(zipshp, 3857)
atl_3857 <- st_transform(atlshp, 3857)

ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

map <- get_map(location = "Atlanta",zoom=9)
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
map <- ggmap_bbox(map) # Use the function

a<- ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = atl_3857, aes(geometry=atl_3857), inherit.aes = FALSE,alpha=0,lwd=2)+
  geom_sf(data = zip_3857, aes(geometry=zip_3857), inherit.aes = FALSE,alpha=0,lwd=1)+
  scale_x_continuous(label = I) +
  scale_y_continuous(label = I) +
  xlab("Longitude")+
  ylab("Latitude") +
  ggtitle("Study Area in Relation to\nAtlanta and Surrounding Areas")+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel_
  
b <- north2(a,x=0.77,y=0.17,scale=0.11,symbol=1)
b+
  scalebar(x.min=-85.3,x.max=-83.5,y.min=33.0,y.max=34.5,transform=F,model= "WGS84",dist=20,dist_unit="km")
?ggsn





####DayMet Seasonal Time Series#####

setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
workshopDaymet <- list.files()[61:(length(list.files())-30)]
daymetfiles <- list()
for(i in 1:length(workshopDaymet)){
  daymetfiles[[i]]<-readRDS(workshopDaymet[i])
}

daymetfiles <- lapply(daymetfiles, function(x){
  subset(x,select=c("Latitude","Longitude","tmin","tmax"))
})

names(daymetfiles) <- str_sub(workshopDaymet,8,14)
unique(names(daymetfiles))

daymetfiles <-lapply(unique(names(daymetfiles)), function(x){
  bind_rows(daymetfiles[x==names(daymetfiles)])
} )

###Generate tmax
##
daymetfilestmax<- lapply(daymetfiles, function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),max))
})

daymetfilestmax<- lapply(daymetfilestmax, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmax")))
})

daymetfilestmax.seasonal <- list()
for(i in seq(1,length(daymetfilestmax), by=3)){
  daymetfilestmax.seasonal[[(i+2)/3]]<-bind_rows(daymetfilestmax[[i]],daymetfilestmax[[i+1]],daymetfilestmax[[i+2]])
  print(i)
}

daymetfilestmax.seasonal <- lapply(daymetfilestmax.seasonal,function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),max))
})

daymetfilestmax.seasonal<- lapply(daymetfilestmax.seasonal, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmax")))
})

names(daymetfilestmax.seasonal) <- (paste(rep(c("Spring","Summer","Fall","Winter"),21),rep(seq(2000,2020,by=1),each=4),sep=" "))[1:83]


###Generate tmin
##
daymetfilestmin<- lapply(daymetfiles, function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),min))
})

daymetfilestmin<- lapply(daymetfilestmin, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmin")))
})

daymetfilestmin.seasonal <- list()
for(i in seq(1,length(daymetfilestmin), by=3)){
  daymetfilestmin.seasonal[[(i+2)/3]]<-bind_rows(daymetfilestmin[[i]],daymetfilestmin[[i+1]],daymetfilestmin[[i+2]])
  print(i)
}

daymetfilestmin.seasonal <- lapply(daymetfilestmin.seasonal,function(x){
  summarise(group_by(x, Longitude, Latitude), across(everything(),min))
})

daymetfilestmin.seasonal<- lapply(daymetfilestmin.seasonal, function(x){
  as.data.frame(subset(x,select=c("Longitude","Latitude","tmin")))
})

names(daymetfilestmin.seasonal) <- (paste(rep(c("Spring","Summer","Fall","Winter"),21),rep(seq(2000,2020,by=1),each=4),sep=" "))[1:83]


rm(daymetfiles)



###Summer Tmin+tmax time series

#Tmin
mintempvector <- NULL
mintempvectorzip<-NULL

daymetfilestmin.seasonal <- lapply(daymetfilestmin.seasonal,function(x){
  st_as_sf(x,coords=c("Longitude","Latitude"),crs= "+proj=longlat +datum=WGS84 +no_defs")
})

for(i in 1:length(daymetfilestmin.seasonal)){
  daymetfilestmin.seasonal[[i]]$tmin <- daymetfilestmin.seasonal[[i]]$tmin*(9/5)+32
}

daymetfilestmin.seasonal.5c <- lapply(daymetfilestmin.seasonal,function(x){
  st_intersection(x,cropLongLat5c_small)
})

daymetfilestmin.seasonal.ZIP <- lapply(daymetfilestmin.seasonal,function(x){
  st_intersection(x,cropLongLatzip_small)
})


for(i in 1:length(daymetfilestmin.seasonal.5c)){
  mintempvector <- c(mintempvector, min(daymetfilestmin.seasonal.5c[[i]]$tmin,na.rm=T))
}

for(i in 1:length(daymetfilestmin.seasonal.ZIP)){
  mintempvectorzip <- c(mintempvectorzip, min(daymetfilestmin.seasonal.ZIP[[i]]$tmin,na.rm=T))
}

#Tmax
maxtempvector <- NULL
maxtempvectorzip<-NULL

daymetfilestmax.seasonal <- lapply(daymetfilestmax.seasonal,function(x){
  st_as_sf(x,coords=c("Longitude","Latitude"),crs= "+proj=longlat +datum=WGS84 +no_defs")
})

for(i in 1:length(daymetfilestmax.seasonal)){
  daymetfilestmax.seasonal[[i]]$tmax <- daymetfilestmax.seasonal[[i]]$tmax*(9/5)+32
}

daymetfilestmax.seasonal.5c <- lapply(daymetfilestmax.seasonal,function(x){
  st_intersection(x,cropLongLat5c_small)
})

daymetfilestmax.seasonal.ZIP <- lapply(daymetfilestmax.seasonal,function(x){
  st_intersection(x,cropLongLatzip_small)
})

for(i in 1:length(daymetfilestmax.seasonal.5c)){
  maxtempvector <- c(maxtempvector, max(daymetfilestmax.seasonal.5c[[i]]$tmax,na.rm=T))
}

for(i in 1:length(daymetfilestmax.seasonal.ZIP)){
  maxtempvectorzip <- c(maxtempvectorzip, max(daymetfilestmax.seasonal.ZIP[[i]]$tmax,na.rm=T))
}



periodvector <- NULL
periodvector <- names(daymetfilestmax.seasonal.5c)

seasonaltempdf <- as.data.frame(cbind(periodvector,mintempvector,maxtempvector))
names(seasonaltempdf) <- c("Season","Minimum Temperature","Maximum Temperature")
seasonaltempdf$`Minimum Temperature`<-as.numeric(seasonaltempdf$`Minimum Temperature`)
seasonaltempdf$`Maximum Temperature`<-as.numeric(seasonaltempdf$`Maximum Temperature`)
seasonaltempdf <- cbind(seasonaltempdf, seq(as.Date("2000-03-01"),as.Date("2020-9-01"),by="3 months"))
names(seasonaltempdf) <- c("Season","Minimum Temperature","Maximum Temperature","Season_as_Date")
seasonaltempdf$Period2<- rep(c("Spring","Summer","Fall","Winter"),21)[1:83]
names(seasonaltempdf) <- c("Season_temp","Minimum Temperature","Maximum Temperature","Season_as_Date","Season")
seasonaltempdfmelt <- melt(seasonaltempdf,id.var="Season_as_Date")
seasonaltempdfmin <- subset(seasonaltempdf,select = c("Season_temp","Minimum Temperature","Season_as_Date","Season"))
seasonaltempdfmax <- subset(seasonaltempdf,select = c("Season_temp","Maximum Temperature","Season_as_Date","Season"))
seasonaltempdfmax$Legend <- "Maximum"
seasonaltempdfmin$Legend <- "Minimum"

seasonaltempdfzip <- as.data.frame(cbind(periodvector,mintempvectorzip,maxtempvectorzip))
names(seasonaltempdfzip) <- c("Season","Minimum Temperature","Maximum Temperature")
seasonaltempdfzip$`Minimum Temperature`<-as.numeric(seasonaltempdfzip$`Minimum Temperature`)
seasonaltempdfzip$`Maximum Temperature`<-as.numeric(seasonaltempdfzip$`Maximum Temperature`)
seasonaltempdfzip <- cbind(seasonaltempdfzip, seq(as.Date("2000-03-01"),as.Date("2020-9-01"),by="3 months"))
names(seasonaltempdfzip) <- c("Season","Minimum Temperature","Maximum Temperature","Season_as_Date")
seasonaltempdfzip$Period2<- rep(c("Spring","Summer","Fall","Winter"),21)[1:83]
names(seasonaltempdfzip) <- c("Season_temp","Minimum Temperature","Maximum Temperature","Season_as_Date","Season")
seasonaltempdfzipmin <- subset(seasonaltempdfzip,select = c("Season_temp","Minimum Temperature","Season_as_Date","Season"))
seasonaltempdfzipmax <- subset(seasonaltempdfzip,select = c("Season_temp","Maximum Temperature","Season_as_Date","Season"))
seasonaltempdfzipmax$Legend <- "Maximum"
seasonaltempdfzipmin$Legend <- "Minimum"

lmfitmaxtemp <- lm()


ggplot()+
  geom_line(data=seasonaltempdfmax, aes(x=Season_as_Date, y=`Maximum Temperature`,linetype=Legend))+
  geom_point(data=seasonaltempdfmax, aes(x=Season_as_Date, y=`Maximum Temperature`,col=Season))+
  geom_smooth(data=seasonaltempdfmax, aes(x=Season_as_Date, y=`Maximum Temperature`),method=lm)+
  geom_line(data=seasonaltempdfmin, aes(x=Season_as_Date, y=`Minimum Temperature`,linetype=Legend))+
  geom_point(data=seasonaltempdfmin, aes(x=Season_as_Date, y=`Minimum Temperature`,col=Season))+
  geom_smooth(data=seasonaltempdfmin, aes(x=Season_as_Date, y=`Minimum Temperature`),method=lm)+
  
  labs(title = "Seasonal Temperature Maximums and Minimums\nSpring 2000-Fall 2022",
       caption = "Spring = March-May\nSummer = June-August\nFall = September-November\nWinter = December-Februrary")+
  ylab("Temperature °F")+
  xlab("Year")+
  
  scale_colour_discrete(name  ="",
                        breaks=c("Spring","Summer","Fall","Winter"),
                        labels=c("Spring","Summer","Fall","Winter")) +
 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))#transparent legend panel


#####NDVI Time Series####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")
list.files()
workshopNDVIfiles <- list.files()
NDVI <- stack(workshopNDVIfiles)

crs(NDVI)
res(NDVI)
str_sub(workshopNDVIfiles,-12,-9)
str_sub(workshopNDVIfiles,-7,-5)
NDVIdates <- format(as.Date(as.numeric(str_sub(workshopNDVIfiles,-7,-5))-1,
                            origin=paste0(str_sub(workshopNDVIfiles,-12,-9),"-01-01")),"%Y %b %d")
names(NDVI) <- NDVIdates
indices <- rep(c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4),21)[1:476]


####AOD Time Series
setwd("C:/Users/EJLI4/OneDrive - Emory University/Processed AOD files")
list.files()
