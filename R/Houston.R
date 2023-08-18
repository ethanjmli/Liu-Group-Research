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
library(inlmisc)

tempcolorvector <- colorRampPalette(brewer.pal(11,"Spectral"))(1000)
airpolcolorvector <- c(inferno(1000))

NDVIcolorvector <- colorRampPalette(bias=0.8, 
                                    c("#A50026","#D73027","#F46D43","#FDAE61","#FEE08B", 
                                      "#FFFFBF","#D9EF8B","#A6D96A","#66BD63", 
                                      "#1A9850","#006837"))(1000)
houston <- get_acs(
    geography = "county", 
    variables = "C02003_004",
    state = "TX", 
    county = c(201,"Chambers","Liberty","Fort Bend","Montgomery","Galveston","Brazoria"),
    year = 2021,
    geometry = TRUE)
houston<-houston[,c(2,6)]
houston$NAME <- c("Brazoria","Galveston","Chambers","Harris","Fort Bend","Montgomery","Liberty")
####Traffic####
crophouston <- st_read("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Shapefile/houston.shp")
library(osmdata)
available_features()

available_tags(feature = "highway")

class(crophouston)

houstonbb <- st_bbox(crophouston)
houstonmajor <- osmdata_sf(add_osm_feature(opq(houstonbb), 
                                           key = "highway", value = c("motorway")))

houstonhighways <- houstonmajor$osm_lines
st_crs(houstonhighways)
crophouston <- st_transform(crophouston,crs=st_crs("+proj=longlat +datum=WGS84 +no_defs"))
crophouston <- st_make_valid(crophouston)

houstonhighways <- st_intersection(houstonhighways, crophouston)
plot(houstonhighways$geometry)
####AOD####
crophouston <- st_read("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Shapefile/houston.shp")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Houston Clean AOD")
list.files()
AODfilenames <- list.files()
AODfilenames
AODfilenames <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames
AODfilenames2 <- substr(AODfilenames,1,8)
table(table(AODfilenames2))
table(AODfilenames2)
which(table(AODfilenames2)==1)
which(table(AODfilenames2)==3)
which(table(AODfilenames2)==4)


aodcrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
crophouston <- st_transform(crophouston,crs = aodcrs)
crophouston <- crophouston[,2]
crs(crophouston)

for(i in seq(1,length(list.files()),2)){
  a <- sds(list.files()[i])
  b<- sds(list.files()[i+1])
  a <- a[2]
  b <- b[2]
  a <- mean(a,na.rm=TRUE)
  b<-mean(b,na.rm=TRUE)
  a <- crop(a, crophouston)
  b <- crop(b,crophouston)
  ab<-merge(a,b)
  ab <- project(ab,"+proj=longlat +datum=WGS84 +no_defs")
  writeRaster(ab,paste0("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/",substr(list.files()[i],10,16)," Houston AOD.tif"))
  print(i)
}

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

####More AOD####
crophouston <- st_read("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Shapefile/houston.shp")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Houston Clean AOD")
houston2022AOD <- list.files()[str_sub(list.files(),1,4)=="2022"]
houstonAOD <- stack(houston2022AOD)
str_sub(houston2022AOD,1,4)
str_sub(houston2022AOD,5,7)
houstondates <- format(as.Date(as.numeric(str_sub(houston2022AOD,5,7))-1,
                               origin=paste0(str_sub(houston2022AOD,1,4),"-01-01")),"%Y %b %d")
houstonindices <- month(as.Date(as.numeric(str_sub(houston2022AOD,5,7))-1,
                                origin=paste0(str_sub(houston2022AOD,1,4),"-01-01")))
houstonAOD <- stackApply(houstonAOD,houstonindices,mean)
houstonAOD
names(houstonAOD)<- unique(format(as.Date(as.numeric(str_sub(houston2022AOD,5,7))-1,
                                          origin=paste0(str_sub(houston2022AOD,1,4),"-01-01")),"%Y %b"))
houstonAOD
houstonAOD <- crop(houstonAOD, crophouston)
houstonAOD <- mask(houstonAOD, crophouston)
plot(houstonAOD[[12]])

houstonAODlist <- list()
for(i in 1:nlayers(houstonAOD)){
  houstonAODlist[[i]]<-
tm_shape(houstonAOD[[i]])+
  tm_raster(col=names(houstonAOD[[i]]),palette = rev(airpolcolorvector),
            breaks =c(seq(min(values(houstonAOD),na.rm=T),max(values(houstonAOD),na.rm=T),0.08),
                      max(values(houstonAOD),na.rm=T)),
            # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "AOD",legend.reverse=T)+
  tm_shape(houstonhighways)+
  tm_lines(col = "highway", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(houston)+
    tm_text("NAME",shadow=T)+
    tm_borders(col="black",lwd=3)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 1km LST",
             position = c(0.5,0))+
  tm_layout(frame=F,
            main.title=paste("Houston",str_sub(names(houstonAOD[[i]]),2,-1),"Aerosol Optical Depth"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
}
houstonAODlist[[9]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Houston")
houstonAODlist[[1]]
for(i in 1:length(houstonAODlist)){
  tmap_save(houstonAODlist[[i]],paste("Houston",str_sub(names(houstonAOD[[i]]),2,-1),"AOD.png"),dpi=300,units="in",height = 12,width=19.1)
}
####LST####
crophouston <- st_read("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Shapefile/houston.shp")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Houston LST 1km 2002_01_01 - 2022_12_31/LST_Day_1km")
houston2022LST <- list.files()[grep("2022",list.files(),fixed=TRUE)]
houstonLST <- stack(houston2022LST)
str_sub(houston2022LST,-12,-9)
str_sub(houston2022LST,-7,-5)
houstondates <- format(as.Date(as.numeric(str_sub(houston2022LST,-7,-5))-1,
                            origin=paste0(str_sub(houston2022LST,-12,-9),"-01-01")),"%Y %b %d")
houstonindices <- month(as.Date(as.numeric(str_sub(houston2022LST,-7,-5))-1,
                                origin=paste0(str_sub(houston2022LST,-12,-9),"-01-01")))
houstonLST <- stackApply(houstonLST,houstonindices,mean)
houstonLST
names(houstonLST)<- unique(format(as.Date(as.numeric(str_sub(houston2022LST,-7,-5))-1,
                      origin=paste0(str_sub(houston2022LST,-12,-9),"-01-01")),"%Y %b"))
houstonLST
values(houstonLST) <- values(houstonLST)*0.02-273
houstonLST
houstonLST <- crop(houstonLST,crophouston)
houstonLST <- mask(houstonLST,crophouston)
plot(houstonLST[[1]])

houstonLSTlist <- list()
for(i in 1:nlayers(houstonLST)){
  houstonLSTlist[[i]]<-
tm_shape(houstonLST[[i]])+
  tm_raster(col=names(houstonLST[[i]]),palette = rev(tempcolorvector),
            breaks =c(seq(min(values(houstonLST),na.rm=T),max(values(houstonLST),na.rm=T),3.5),
                      max(values(houstonLST),na.rm=T)),
            # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "Temp °C",legend.reverse=T)+
  tm_shape(houstonhighways)+
  tm_lines(col = "highway", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
 tm_shape(houston)+
  tm_text("NAME",shadow=T)+
  tm_borders(col="black",lwd=3)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 1km LST",
             position = c(0.5,0))+
  tm_layout(frame=F,
            main.title=paste("Houston",str_sub(names(houstonLST[[i]]),2,-1),"Land Surface Temperature"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
}
houstonLSTlist[[11]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Houston")
houstonLSTlist[[1]]
for(i in 1:length(houstonLSTlist)){
  tmap_save(houstonLSTlist[[i]],paste("Houston",str_sub(names(houstonLST[[i]]),2,-1),"LST.png"),dpi=300,units="in",height = 12,width=19.1)
}
####NDVI####
crophouston <- st_read("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Shapefile/houston.shp")
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Houston NDVI 500m 2002_01_01 - 2022_12_31/NDVI")

houston2022NDVI <- list.files()[grep("2022",list.files(),fixed=TRUE)]
houstonNDVI <- stack(houston2022NDVI)
str_sub(houston2022NDVI,-12,-9)
str_sub(houston2022NDVI,-7,-5)
houstondates <- format(as.Date(as.numeric(str_sub(houston2022NDVI,-7,-5))-1,
                               origin=paste0(str_sub(houston2022NDVI,-12,-9),"-01-01")),"%Y %b %d")
#houstonindices <- month(as.Date(as.numeric(str_sub(houston2022LST,-7,-5))-1,
#                                origin=paste0(str_sub(houston2022LST,-12,-9),"-01-01")))
#houstonLST <- stackApply(houstonLST,houstonindices,mean)
#houstonLST
NDVIPeriods <- paste0(houstondates,"-",format(as.Date(houstondates,format = "%Y %b %d")+15,"%b %d"))
names(houstonNDVI)<- NDVIPeriods
values(houstonNDVI) <- values(houstonNDVI)*0.0001
houstonNDVI
houstonNDVI <- crop(houstonNDVI,crophouston)
houstonNDVI <- mask(houstonNDVI,crophouston)
plot(houstonNDVI[[1]])

houstonNDVIlist<-list()
for(i in 1:nlayers(houstonNDVI)){
  houstonNDVIlist[[i]]<-
tm_shape(houstonNDVI[[i]])+
  tm_raster(col=names(houstonNDVI[[i]]),palette = (NDVIcolorvector),
            breaks =c(seq(min(values(houstonNDVI),na.rm=T),max(values(houstonNDVI),na.rm=T),0.2),
                      max(values(houstonNDVI),na.rm=T)),
            # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "NDVI",legend.reverse=T)+
  tm_shape(houstonhighways)+
  tm_lines(col = "highway", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(houston)+
    tm_text("NAME",shadow=T)+
    tm_borders(col="black",lwd=3)+
  # tm_shape(cropLongLat_zip)+
  # tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD13A1v6 500m NDVI",
             position = c(0.5,0))+
  tm_layout(frame=F,
            main.title=paste("Houston",str_sub(names(houstonNDVI[[i]]),2,-1),"NDVI"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
}
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/Houston")
houstonNDVIlist[[1]]
for(i in 1:length(houstonNDVIlist)){
tmap_save(houstonNDVIlist[[i]],paste("Houston",str_sub(names(houstonNDVI[[i]]),2,-1),"NDVI.png"),dpi=300,units="in",height = 12,width=19.1)
}
