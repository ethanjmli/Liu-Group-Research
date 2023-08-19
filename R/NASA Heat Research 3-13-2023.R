#install.packages("rgdal")
#install.packages("gWidgetsRGtk2")
#install.packages("devtools")
#install.packages('stars')
#install.packages("dichromat")
#install.packages('cultevo')
#install.packages("rts")
#install.packages("ggnewscale")
#install.packages("patchwork")
library(MODIStsp) #For getting MODIS data
library(ggplot2) #For plotting maps
library(raster) #For reading rasters
library(rgdal) #readOGR()
library(sf) #For working with sf objects 
library(devtools)
library(stars)
library(stringr) #For working with strings
library(rasterVis)
library(tidyverse)
library(dplyr)
library(lubridate) #For working with dates
library(viridis)
library(ggnewscale)
library(patchwork)
library(rgeos)
library(tidycensus)
tempcolorvector <- c("grey","white","violet",
                     "blueviolet","blue","cadetblue2", "aquamarine",
                     "green","chartreuse","greenyellow","yellow",
                     "goldenrod","orange","orangered3",
                     "red","salmon4","black")
#####MODIS GUI#####
MODIStsp() #Load GUI for downloading specific areas/times of satellite data
#Note: MOD11a1 v6 day/night LST data is in units of Kelvin, scale factor = *50 
#multiply by 0.02 for real value(s) in Kelvin
#-273 for values in Celsius
#Yes R Rasterstack
#Yes Reprocess
#Projection = set by user: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0


#####Get shapefiles#####
crop_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
cropLongLat_5c <- st_transform(crop_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

crop_parameters_zip <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/SelectedZipCode_ATL.shp")
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

boundary_zip <- as(cropLongLat_zip, "sf")
boundary_5c <- as(cropLongLat_5c, "sf")

ggplot() +
  geom_sf(data=boundary_5c, aes(fill = as.factor(NAME)))+
  geom_sf(data=boundary_zip, aes(fill = as.factor(ZCTA5CE10)))



####2020 Daytime LST####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Surf_Temp_Daily_1Km_v6/Time_Series/RData/Terra/LST_Day_1km")
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

spring2020 <- 
  ggplot() + 
  geom_tile(
    aes(x, y, fill = value),
    filter(Weekday2020maskdf, season == "spring"))+
  facet_wrap(vars(week), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colours = tempcolorvector) +
  labs(fill = "Spring") +
  xlab("Latitude")+
  ylab("Longitude")+
  coord_equal() #+ 

summer2020 <- 
  ggplot() + 
  geom_tile(
    aes(x, y, fill = value),
    filter(Weekday2020maskdf, season == "summer"))+
  facet_wrap(vars(week), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colours = 
                         tempcolorvector) +
  labs(fill = "Summer") +
  coord_equal() #+ 
summer2020

fall2020 <- 
  ggplot() + 
  geom_tile(
    aes(x, y, fill = value),
    filter(Weekday2020maskdf, season == "fall"))+
  facet_wrap(vars(week), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colours = tempcolorvector) +
  labs(fill = "Fall") +
  coord_equal() #+ 


winter2020 + spring2020 + summer2020 + fall2020


####2020 Nighttime LST####
setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Surf_Temp_Daily_1Km_v6/Time_Series/RData/Terra/LST_Night_1km")
nightraster2020 <- get(load("MOD11A1_LST_Night_1km_1_2020_366_2020_RData.RData"))
values(nightraster2020) <- values(nightraster2020)*0.02-273
night2020crop <- crop(nightraster2020, cropLongLat)
night2020mask <- mask(night2020crop, cropLongLat)
night2020maskdf <- as.data.frame(night2020mask,xy=TRUE)
night2020maskpoints <- rasterToPoints(night2020mask)

#names(night2020mask)<-seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days")
#names(night2020mask)
names(night2020maskdf) <- c(names(night2020maskdf)[c(1,2)],
                            seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"))
Dates <- as.Date.numeric(as.numeric(colnames(night2020maskdf)[c(-1,-2)]),
                               origin = "1970-01-01")
                         

##Get statistics on mean, max, min, median temp for each day
Night_Mean_Temp_Celsius<-apply(night2020maskdf[,c(3:length(night2020maskdf))],2,FUN=mean,na.rm=TRUE)
nightstats<-as.data.frame(Night_Mean_Temp_Celsius)
Night_Max_Temp_Celsius<-apply(night2020maskdf[,c(3:length(night2020maskdf))],2,FUN=max,na.rm=TRUE)
nightmax <- as.data.frame(Night_Max_Temp_Celsius)
nightstats <- cbind(nightstats,nightmax)
Night_Min_Temp_Celsius<-apply(night2020maskdf[,c(3:length(night2020maskdf))],2,FUN=min,na.rm=TRUE)
nightmin <- as.data.frame(Night_Min_Temp_Celsius)
nightstats <- cbind(nightstats,nightmin)
Night_Median_Temp_Celsius<-apply(night2020maskdf[,c(3:length(night2020maskdf))],2,FUN=median,na.rm=TRUE)
nightmedian <- as.data.frame(Night_Median_Temp_Celsius)
nightstats <- cbind(nightstats,nightmedian)


Day <- seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days")
df<-cbind(Day,df)
ggplot(data=df,aes(x=Day,y=Mean_Temp_Celsius))+
  geom_point()+
  geom_smooth()
#spring <- seq(as.Date("2020/3/1"), as.Date("2020/5/31"), "days")

ggplot(data=df[1:31,],aes(x=Day,y=Mean_Temp_Celsius))+
  geom_point()+
  geom_smooth()

plot(yeardaymask, col = temperature.colors(30), nc = 6, maxnl = 24)

#test <- slice_max(night2020maskdf,n=1,night2020maskdf$MOD11A1_LST_Night_1km_2020_025)

##Get max temperature per Day, with x/y data
xynightmax <- matrix(nrow=0,ncol = 4)
xynightmax <- as.data.frame(xynightmax)
names(xynightmax) <- c("X","Y","NightMax","Day")
for (i in 3:length(night2020maskdf)){
  max_slice <- slice_max(night2020maskdf,n=1,night2020maskdf[,i])
  max_slice_subset <- max_slice[,c(1,2,i)]
  additional_rows <- matrix(nrow = nrow(max_slice_subset),ncol = 4)
  additional_rows <-as.data.frame(additional_rows)
  names(additional_rows)<-c("X","Y","NightMax","Day")
  additional_rows$X <- max_slice_subset$x
  additional_rows$Y <- max_slice_subset$y
  additional_rows$NightMax <- max_slice_subset[,3]
  tryCatch(additional_rows$Day <- colnames(max_slice_subset)[[3]], 
           error=function(e){print(colnames(max_slice_subset)[[3]])})
  xynightmax<-rbind(xynightmax,additional_rows)
  print(i)
}

xynightmax$Day<-as.numeric(xynightmax$Day)
xynightmax$Day<-as.Date.numeric(xynightmax$Day,origin="1970-01-01")
class(xynightmax$Day)

##Get min temperature per Day, with x/y data
xynightmin <- matrix(nrow=0,ncol = 4)
xynightmin <- as.data.frame(xynightmin)
names(xynightmin) <- c("X","Y","NightMin","Day")
for (i in 3:length(night2020maskdf)){
  min_slice <- slice_min(night2020maskdf,n=1,night2020maskdf[,i])
  min_slice_subset <- min_slice[,c(1,2,i)]
  additional_rows <- matrix(nrow = nrow(min_slice_subset),ncol = 4)
  additional_rows <-as.data.frame(additional_rows)
  names(additional_rows)<-c("X","Y","NightMin","Day")
  additional_rows$X <- min_slice_subset$x
  additional_rows$Y <- min_slice_subset$y
  additional_rows$NightMin <- min_slice_subset[,3]
  tryCatch(additional_rows$Day <- colnames(min_slice_subset)[[3]], 
           error=function(e){print(colnames(min_slice_subset)[[3]])})
  xynightmin<-rbind(xynightmin,additional_rows)
  print(i)
}
xynightmin$Day<-as.numeric(xynightmin$Day)
xynightmin$Day<-as.Date.numeric(xynightmin$Day,origin="1970-01-01")
class(xynightmin$Day)


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
#scale = 0.0001
crop_5c <- st_read("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Shapefiles/five_counties.shp")
cropLongLat_5c <- st_transform(crop_5c,crs("+proj=longlat +datum=WGS84 +no_defs"))


setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/VI_16Days_1Km_v6/Time_Series/RData/Terra/NDVI")
list.files()
NDVI_16day_2020 <- get(load("MOD13A2_NDVI_1_2020_353_2020_RData.RData"))
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

######Pair with 16-day increment Day LST#####
setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Surf_Temp_Daily_1Km_v6/Time_Series/RData/Terra/LST_Day_1km")
dayraster2020 <- get(load("MOD11A1_LST_Day_1km_1_2020_366_2020_RData.RData"))
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


#NDVI_16day_2020maskdf$date <- as.numeric(NDVI_16day_2020maskdf$date)
#LST2020_16daysmaskdf$date <- as.numeric(LST2020_16daysmaskdf$date)


plot1<- ggplot() + 
  geom_tile(data = NDVI_16day_2020maskdf[NDVI_16day_2020maskdf$date == "2020-09-29",],
            aes(x, y, fill = NDVI_value,)#,
           #filter(NDVI_16day_2020maskdf, date == 1)
           )+
  facet_wrap(vars(date), ncol=2) +
  scale_fill_gradientn(guide = "colourbar",colors = rev(terrain.colors(n=1000))) +
  labs(fill = "NDVI",title = "Five Counties 2020 16 Day NDVI") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal() +
  
  new_scale_fill()+
  
  geom_sf(data=sf_poly, size=2, colour="black",alpha=0)
  

  #new_scale_fill()+

plot2<- ggplot()+
  geom_tile(data = LST2020_16daysmaskdf[LST2020_16daysmaskdf$date == "2020-09-29",],
            aes(x, y, fill = temperature)#,
            #filter(LST2020_16daysmaskdf, date == "X2020.01.01")
            )+
    facet_wrap(vars(date), ncol=2) +
    scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
    labs(fill = "Temperature",title = "Five Counties 2020 16 Day Temperature") +
    xlab("Longitude")+
    ylab("Latitude")+
    coord_equal()+

    new_scale_fill()+
  
    geom_sf(data=sf_poly, size=2, colour="black",alpha=0)

plot1 + plot2

plot1
plot2

####Mapping Socioeconomic Data####
#install.packages("tidycensus")
library(tidycensus)

v20 <- load_variables(2020, "acs5", cache = TRUE)
#Census API key: 962254c4a7787dd62a33cbf37842dd5dcb8ff620

census_api_key("962254c4a7787dd62a33cbf37842dd5dcb8ff620",install=TRUE)
######Mapping Black population#####
five_counties_Blackpop <- get_acs(
  geography = "tract", 
  variables = "C02003_004",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2020,
  geometry = TRUE
)
five_counties_Blackpop

plot(five_counties_Blackpop)

five_counties_Totalpop <- get_acs(
  geography = "tract", 
  variables = "C02003_001",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2020,
  geometry = TRUE
)

plot(five_counties_Totalpop)

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
  variables = "B19001_001",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2020,
  geometry = TRUE
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
library(ggplot2)
available_features()

available_tags(feature = "highway")

class(cropLongLat_5c)

five_c_bb <- st_bbox(cropLongLat_5c)
five_c_major <- osmdata_sf(add_osm_feature(opq(five_c_bb), 
                    key = "highway", value = c("motorway")))

gwinnett_bb <- getbb("Gwinnett",format_out = "polygon")
gwinnett_major <- osmdata_sf(add_osm_feature(opq(gwinnett_bb), 
                    key = "highway", value = c("motorway", "primary", "secondary")))

class(five_c_major)

five_c_major <- trim_osmdata(five_c_major,five_c_bb,exclude=TRUE)


street_plot2 <- ggplot() +
  geom_tile(data = LST2020_16days_5cdf[LST2020_16days_5cdf$date == "2020-09-29",],
            aes(x, y, fill = temperature)
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(date), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Temperature",title = "Five Counties 2020 16 Day Temperature") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal() +
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=2,colour="black",alpha=0)+
  geom_sf(data = five_c_major$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.2) 
  #geom_sf(data = gwinnett_major$osm_lines,
          #inherit.aes = FALSE,
         # color = "red",
          #size = 0.5) +
  

street_plot2
####5 Counties + Zip code level mapping####
######Get shapefiles######
crop_5c <- st_read("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Shapefiles/five_counties.shp")
cropLongLat_5c <- st_transform(crop_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

crop_parameters_zip <- st_read("/Users/ethan_j_li/Documents/Emory/Liu Group Research/Shapefiles/SelectedZipCode_ATL.shp")
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

boundary_zip <- as(cropLongLat_zip, "sf")
boundary_5c <- as(cropLongLat_5c, "sf")

ggplot() +
  geom_sf(data=boundary_5c, aes(fill = as.factor(NAME)))+
  geom_sf(data=boundary_zip, aes(fill = as.factor(ZCTA5CE10)))

######Zip Code LST######

#Crop 2020 LST with selected zip codes#
setwd("C:/Users/EJLI4/Documents/Liu Group Research/Surf_Temp_Daily_1Km_v6/Time_Series/RData/Terra/LST_Day_1km")
dayraster2020 <- get(load("MOD11A1_LST_Day_1km_1_2020_366_2020_RData.RData"))
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



#Map zip code LST
zipLST <- ggplot()+
  geom_tile(data = LST2020_16days_zipdf[LST2020_16days_zipdf$date == "2020-09-29",],
            aes(x, y, fill = temperature)#,
            #filter(LST2020_16daysmaskdf, date == "X2020.01.01")
  )+
  facet_wrap(vars(date), ncol=2) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Temperature",title = "Select Zip 2020 16 Day Temperature") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal()+
  
  new_scale_fill()+
  
  geom_sf(data=zip_poly, size=2, colour="black",alpha=0)

#Map five counties LST
LST2020_16days_5cdf$period <- paste0(LST2020_16days_5cdf$date, " - ", LST2020_16days_5cdf$date+15)  
class(LST2020_16days_5cdf$period)

five_c_LST<- 
  ggplot() + 
  geom_tile(data = LST2020_16days_5cdf[LST2020_16days_5cdf$date == "2020-01-17" 
                                       & is.na(LST2020_16days_5cdf$temperature) == FALSE,],
            aes(x, y, fill = temperature),
            na.rm = TRUE
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(period), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Temperature °C",title = "Five Counties 2020 16 Day Temperature") +
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

######Zip Code NDVI######

setwd("/Users/ethan_j_li/Documents/Emory/Liu Group Research/VI_16Days_1Km_v6/Time_Series/RData/Terra/NDVI")
list.files()
NDVI_16day_2020 <- get(load("MOD13A2_NDVI_1_2020_353_2020_RData.RData"))
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
#Weekday2020maskdf <- rename(Weekday2020maskdf, week = variable) 
#Weekday2020maskdf$season <- ifelse(Weekday2020maskdf$week %in% c(1:12,52:53), "winter",
#                                  ifelse(Weekday2020maskdf$week %in% c(13:25),"spring",
#                                     ifelse(Weekday2020maskdf$week %in% c(26:39),"summer","fall")))

NDVI_16days_dates_zip <- rep(days_2020_by16, each = 432)
NDVI_16day_2020_zipdf <- cbind(NDVI_16day_2020_zipdf,NDVI_16days_dates_zip )
table(NDVI_16day_2020_zipdf$date, NDVI_16day_2020_zipdf$NDVI_16days_dates_zip)
NDVI_16day_2020_zipdf <- NDVI_16day_2020_zipdf[,-4]
colnames(NDVI_16day_2020_zipdf)[4]<-"date"

zipNDVI <- 
  ggplot() + 
  geom_tile(data = NDVI_16day_2020_zipdf[NDVI_16day_2020_zipdf$date == "2020-09-29",],
            aes(x, y, fill = NDVI_value)
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(date), ncol=4) +
  scale_fill_gradientn(guide = "colourbar",colors = rev(terrain.colors(n=1000))) +
  labs(fill = "NDVI",title = "Select Zip 2020 16 Day NDVI") +
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal()+ 
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=2, colour="black",alpha=0)

five_c_NDVI<- 
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
  coord_equal() +

  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=2,colour="black",alpha=0)

five_c_NDVI + zipNDVI

######Zip Code Black Pop######
#Absolute count#
five_counties_Blackpop <- get_acs(
  geography = "tract", 
  variables = "C02003_004",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2020,
  geometry = TRUE
)
class(five_counties_Blackpop)

plot(five_counties_Blackpop)

five_c_Blackpop <- 
  ggplot()+
  geom_sf(data=five_counties_Blackpop, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Black pop",title = "Five Counties 2020 Black Pop") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="red",alpha=0)

cropLongLat_zip2 <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_Blackpop <- st_intersection(five_counties_Blackpop,cropLongLat_zip2)

plot(zip_Blackpop)
zipBlackpop <-
  ggplot()+
  geom_sf(data=zip_Blackpop, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Black pop",title = "Select Zip 2020 Black Pop") +
  xlab("Longitude")+
  ylab("Latitude")+ 
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=5, colour="red",alpha=0)

five_c_Blackpop + zipBlackpop

#Percentile of total population#
five_c_Blackpercent <- 
  ggplot()+
  geom_sf(data=five_counties_Blackpercent, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Black percent",title = "Five Counties 2020 Black Percent") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="green",alpha=0)

cropLongLat_zip_NAD83 <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_Blackpercent <- st_intersection(five_counties_Blackpercent,cropLongLat_zip_NAD83)

plot(zip_Blackpercent)
zipBlackpercent <-
  ggplot()+
  geom_sf(data=zip_Blackpercent, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = -1)+
  labs(fill = "Black percent",title = "Select Zip 2020 Black Percent") +
  xlab("Longitude")+
  ylab("Latitude")+ 
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=5, colour="green",alpha=0)

five_c_Blackpercent + zipBlackpercent

######Zip Code Household Income#####
five_counties_12monthincome <- get_acs(
  geography = "tract", 
  variables = "B19001_001",
  state = "GA", 
  county = c("Gwinnett","Fulton","Dekalb","Cobb","Clayton"),
  year = 2020,
  geometry = TRUE
)

plot(five_counties_12monthincome)

five_c_12monthincome<-
  ggplot()+
  geom_sf(data=five_counties_12monthincome, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1)+
  labs(fill = "Income",title = "Five Counties 2020 Household Income") +
  xlab("Longitude")+
  ylab("Latitude")+
  
  new_scale_fill()+
  
  geom_sf(data=boundary_5c, size=2, colour="black",alpha=0)+
  geom_sf(data=boundary_zip,size=6,colour="black",alpha=0)

cropLongLat_zip2 <- st_transform(crop_parameters_zip,crs("+proj=longlat +datum=NAD83 +no_defs"))
zip_12monthincome <- st_intersection(five_counties_12monthincome,cropLongLat_zip2)

zip12monthincome <-
  ggplot()+
  geom_sf(data=zip_12monthincome, aes(fill=estimate))+
  scale_fill_distiller(palette = "RdYlBu", 
                       direction = 1)+
  labs(fill = "Income",title = "Select Zip 2020 Household Income") +
  xlab("Longitude")+
  ylab("Latitude")+ 
  
  new_scale_fill()+
  
  geom_sf(data=boundary_zip, size=5, colour="black",alpha=0)

five_c_12monthincome + zip12monthincome

####Map Model Data####
setwd("C:/Users/EJLI4/Documents/Liu Group Research/Georgia_subset_2020")

list.files()
list.files(pattern="ATL.*.tif$")
list.filenames<-list.files(pattern="ATL.*.tif$")
list.filenames

model2020_stack <- stack(list.filenames)

crs(model2020_stack)
plot(model2020_stack[[1]])
model2020_stack <- projectRaster(model2020_stack,crs = "+proj=longlat +datum=WGS84 +no_defs")
crs(model2020_stack)
plot(model2020_stack[[1]])

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
names(model2020_16days_5cdf) <- c('temperature', 'date')


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
  geom_tile(data = model2020_16days_5cdf[model2020_16days_5cdf$date == "2020-01-17" 
                                       & is.na(model2020_16days_5cdf$temperature) == FALSE,],
            aes(x, y, fill = temperature),
            na.rm = TRUE
            #filter(Weekday2020maskdf, season == "winter")
  )+
  facet_wrap(vars(period), ncol=3) +
  scale_fill_gradientn(guide = "colourbar",colors = tempcolorvector) +
  labs(fill = "Temperature °C",title = "2020 Modeled Air Temperature 16 Day Average") +
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
