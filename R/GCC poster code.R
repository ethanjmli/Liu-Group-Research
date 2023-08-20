library(stringr) #For working with strings
#library(rasterVis)
library(tidyverse)
library(dplyr)
library(lubridate) #For working with dates
#library(viridis)
#library(ggnewscale)
library(patchwork)
library(rgeos)
library(tidycensus)
library(osmdata)
library(tmap)
library(RColorBrewer)
library(gridExtra)
library(ggmap)
library(hydroTSM)

tempcolorvector <- colorRampPalette(brewer.pal(11,"RdYlBu"))(1000)
airpolcolorvector <- colorRampPalette(brewer.pal(11,"RdYlBu"))(1000)

NDVIcolorvector <- colorRampPalette(bias=0.8, 
                                    c("#A50026","#D73027","#F46D43","#FDAE61","#FEE08B", 
                                      "#FFFFBF","#D9EF8B","#A6D96A","#66BD63", 
                                      "#1A9850","#006837"))(1000)
humiditycolorvector <- colorRampPalette(brewer.pal(11,"BrBG"))(1000)
tmap_options(check.and.fix = TRUE)
#####MODIS GUI#####
#MODIStsp() #Load GUI for downloading specific areas/times of satellite data
#Note: MOD11a1 v6 day/night LST data is in units of Kelvin, scale factor = *50 
#multiply by 0.02 for real value(s) in Kelvin
#-273 for values in Celsius
#Yes R Rasterstack
#Yes Reprocess
#Projection = set by user: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

#####Get shapefiles and boundaries#####
crop_parameters_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
cropLongLat_5c <- st_make_valid(cropLongLat_5c)
cropLongLat5c_small <- cropLongLat_5c[,9]
ymod <- c(0,0,-4,0,0)
xmod <- c(0,0,-3,0,0)
cropLongLat_5c <- cbind(cropLongLat_5c,ymod, xmod)
rm(crop_parameters_5c)


crop_parameters_zip <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/SelectedZipCode_ATL.shp")
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
cropLongLatzip_small <- cropLongLat_zip[,10]
colnames(cropLongLat_zip)[1]<-"ZIP Code"
rm(crop_parameters_zip)

#####NDVI####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")

NDVI <- stack(list.files())
NDVI
names(NDVI)
NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
                            origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
NDVI <- setZ(NDVI,NDVIdates,"Date")
values(NDVI) <- values(NDVI)*0.0001
NDVIyears <- format(NDVIdates,"%Y")
NDVI <- stackApply(NDVI,NDVIyears,mean)

NDVI <- crop(NDVI,cropLongLat5c_small)
NDVI <- mask(NDVI, cropLongLat5c_small)
NDVI_df <- data.frame(rasterToPoints(NDVI))
colnames(NDVI_df)[3:ncol(NDVI_df)] <- unique(NDVIyears)
NDVI_df <- NDVI_df[,c(-1,-2)]
mean(NDVI_df$`2002`)
NDVI_df <- apply(NDVI_df,2,mean,na.rm=T)
NDVI_df <- as.data.frame(NDVI_df)
NDVI_df$date <- rownames(NDVI_df)
NDVI_df$date <- as.Date(paste0(NDVI_df$date,"-01-01")   # Convert Julian day to date
                                )

NDVI_df$season <- time2season(NDVI_df$date,out.fmt="seasons")
colnames(NDVI_df)[1]<-"NDVI"
NDVI_df$group = "5c"


NDVIzip <- crop(NDVI,cropLongLat_zip)
NDVIzip <- mask(NDVIzip, cropLongLat_zip)
NDVIzip_df <- data.frame(rasterToPoints(NDVIzip))
colnames(NDVIzip_df)[3:ncol(NDVIzip_df)] <- unique(NDVIyears)
NDVIzip_df <- NDVIzip_df[,c(-1,-2)]
mean(NDVIzip_df$`2002`)
NDVIzip_df <- apply(NDVIzip_df,2,mean,na.rm=T)
NDVIzip_df <- as.data.frame(NDVIzip_df)
NDVIzip_df$date <- rownames(NDVIzip_df)
NDVIzip_df$date <- as.Date(paste0(NDVI_df$date,"-01-01"))   # Convert Julian day to date

NDVIzip_df$season <- time2season(NDVIzip_df$date,out.fmt="seasons")
colnames(NDVIzip_df)[1]<-"NDVI"
NDVIzip_df$group = "ZIP"

NDVI_df <- NDVI_df[-22,]
NDVIzip_df <- NDVIzip_df[-22,]
combined_NDVI_df <- rbind(NDVI_df,NDVIzip_df)



NDVI_ts<-ggplot(data=combined_NDVI_df,aes(x=date, y=NDVI,group=group,col=group))+
  # geom_line(data=cleandata1_2,mapping=aes(x=time,y=meanPM,col=Group))+
  geom_point()+
  geom_line()+
  
  # geom_text(data=cleandata1_2,aes(x=time, y=meanPM,fill=Group,label=Location), vjust=1.6, color="black",
  #        position = position_dodge(0.9), size=3.5)+
  labs(title = "Time Series of Annual Mean NDVI\nMetro Atlanta vs Select ZIP Codes, 2002-2022",
       y = "NDVI",
       x = "Date")+
  scale_color_manual(name="Legend",
                     labels = c("Metro Atlanta","Select ZIP Codes"),
                     values=c("chartreuse3","saddlebrown"))+
  
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/GCC Poster Figures")
ggsave("NDVI time series.png",NDVI_ts,dpi=300)
write.csv(combined_NDVI_df,"16 day average NDVI datatable.csv")

####LST####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2002_01_01 - 2023_03_23/LST_Day_1km")
LST <- stack(list.files())
LSTdates <- as.Date(as.numeric(str_sub(names(LST),-3,-1))-1,
                                 origin=paste0(str_sub(names(LST),-8,-5),"-01-01"))
7579/16
LSTindices <- rep(1:474,each=16)
LSTindices <- LSTindices[1:7579]
#LSTmonths <- format(LSTdates,"%Y-%m")
#length(LSTmonths) == nlayers(LST)
#LST <- stackApply(LST,LSTmonths,mean)


LSTyears <- format(LSTdates,"%Y")
length(LSTyears) == nlayers(LST)
LST <- stackApply(LST,LSTyears,mean)


LST <- crop(LST,cropLongLat5c_small)
LST <- mask(LST,cropLongLat5c_small)
LST_df <- data.frame(rasterToPoints(LST))
colnames(LST_df)[3:ncol(LST_df)] <- unique(LSTyears)
LST_df <- LST_df[,c(-1,-2)]
mean(LST_df$`2002`)
LST_df <- apply(LST_df,2,mean,na.rm=T)
LST_df <- as.data.frame(LST_df)
LST_df
LST_df$date <- paste0(unique(LSTyears),"-01-01")
LST_df$date <- as.Date(LST_df$date,format="%Y")
LST_df$season <- time2season(LST_df$date,out.fmt="seasons")
colnames(LST_df)[1]<-"LST"
LST_df$group = "5c"
LST_df$LST <- LST_df$LST*0.02-273


LST_zip <- crop(LST,cropLongLatzip_small)
LST_zip <- mask(LST_zip,cropLongLatzip_small)
LSTzip_df <- data.frame(rasterToPoints(LST_zip))
colnames(LSTzip_df)[3:ncol(LSTzip_df)] <- unique(LSTyears)
LSTzip_df <- LSTzip_df[,c(-1,-2)]
mean(LSTzip_df$`2002`)
LSTzip_df <- apply(LSTzip_df,2,mean,na.rm=T)
LSTzip_df <- as.data.frame(LSTzip_df)
LSTzip_df
LSTzip_df$date <- paste0(unique(LSTyears),"-01-01")
LSTzip_df$date <- as.Date(LSTzip_df$date,format="%Y")
LSTzip_df$season <- time2season(LSTzip_df$date,out.fmt="seasons")
colnames(LSTzip_df)[1]<-"LST"
LSTzip_df$group = "ZIP"
LSTzip_df$LST <- LSTzip_df$LST*0.02-273

combind_LST_df <- rbind(LST_df,LSTzip_df)

LST_ts <- ggplot(data=combind_LST_df,aes(x=date, y=LST,group=group,col=group))+
  # geom_line(data=cleandata1_2,mapping=aes(x=time,y=meanPM,col=Group))+
  geom_point()+
  geom_line()+
  
  # geom_text(data=cleandata1_2,aes(x=time, y=meanPM,fill=Group,label=Location), vjust=1.6, color="black",
  #        position = position_dodge(0.9), size=3.5)+
  labs(title = "Time Series of Annual Mean Land Surface Temperature\nMetro Atlanta vs Select ZIP Codes, 2002-2022",
       y = "Land Surface Temperature (°C)",
       x = "Date")+
  scale_color_manual(name="Legend",
                     labels = c("Metro Atlanta","Select ZIP Codes"),
                     values=c("blue","red"))+
  
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/GCC Poster Figures")
ggsave("LST time series.png",LST_ts,dpi=300)


####AOD####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Raw AOD")
list.files()
AODfilenames <- list.files()
AODfilenames
AODfilenames <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames
AODfilenames2 <- substr(AODfilenames,1,8)
table(table(AODfilenames2))
table(AODfilenames2)
which(table(AODfilenames2)==1)

aodcrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
cropLongLat_hdf <- st_transform(crop_parameters_5c,crs = aodcrs)
cropLongLat_hdf <- cropLongLat_hdf[,9]
rm(crop_parameters_5c)
crs(cropLongLat_hdf)

for(i in seq(1,length(list.files()),2)){
  a <- sds(list.files()[i])
  b<- sds(list.files()[i+1])
  a <- a[2]
  b <- b[2]
  a <- mean(a,na.rm=TRUE)
  b<-mean(b,na.rm=TRUE)
  a <- crop(a, cropLongLat_hdf)
  b <- crop(b,cropLongLat_hdf)
  ab<-merge(a,b)
  ab <- project(ab,"+proj=longlat +datum=WGS84 +no_defs")
  writeRaster(ab,paste0("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Cleaned AOD/",substr(list.files()[i],10,16)," AOD.tif"))
  print(i)
}


#####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Cleaned AOD")
list.files()
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


######
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


AODts <- ggplot(data=combind_AOD_df,aes(x=date, y=AOD,group=group,col=group))+
  # geom_line(data=cleandata1_2,mapping=aes(x=time,y=meanPM,col=Group))+
  geom_point()+
  geom_line()+
  
  # geom_text(data=cleandata1_2,aes(x=time, y=meanPM,fill=Group,label=Location), vjust=1.6, color="black",
  #        position = position_dodge(0.9), size=3.5)+
  labs(title = "Time Series of Annual Mean Aerosol Optical Depth\nMetro Atlanta vs Select ZIP Codes, 2002-2022",
       y = "Aerosol Optical Depth",
       x = "Date")+
  scale_color_manual(name="Legend",
                     labels = c("Metro Atlanta","Select ZIP Codes"),
                     values=c("blue","red"))+
  
  theme(
    #plot.title = element_text(size=22),
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/GCC Poster Figures")
ggsave("AOD time series.png",AODts,dpi=300)

ggplot(data=combind_AOD_df,aes(y=AOD,group=group,col=group))+
  geom_boxplot()

setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
ggsave("Annual AOD time series.png",AODts,dpi=300)
ggsave("Annual LST time series.png",LST_ts,dpi=300)
ggsave("Annual NDVI time series.png",NDVI_ts,dpi=300)
