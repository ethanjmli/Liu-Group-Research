#library(raster) #For reading rasters
#library(rgdal) #readOGR()
library(sf) #For working with sf objects 
#library(terra)
#library(devtools)
#library(stars)
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

crop_parameters_neighborhood <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/Atlanta_Neighborhoods.shp")
crop_parameters_neighborhood <- st_make_valid(crop_parameters_neighborhood)
#cropLongLat_neighborhood <- st_transform(crop_parameters_neighborhood,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
#cropLongLat_neighborhood <- st_make_valid(cropLongLat_neighborhood)
zipshp <- st_combine(cropLongLatzip_small)
zipshp <- st_make_valid(zipshp)
st_bbox(zipshp)
cropLongLat_neighborhoodzip <- st_crop(crop_parameters_neighborhood,zipshp)

five_c_highways$Highways <- "Highway"
five_c_motorways <- subset(five_c_highways,`Highway Type`=="motorway")

zip_highways$Highways <- "Highway"
zip_motorways <- subset(zip_highways,`Highway Type` == "motorway")






####NDVI
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")
workshopNDVIfiles <- list.files()[c(grep("2002",list.files(),fixed=TRUE),grep("2022",list.files(),fixed=TRUE))]
NDVI <- stack(workshopNDVIfiles)
crs(NDVI)
res(NDVI)
str_sub(workshopNDVIfiles,-12,-9)
str_sub(workshopNDVIfiles,-7,-5)
NDVIdates <- format(as.Date(as.numeric(str_sub(workshopNDVIfiles,-7,-5))-1,
                            origin=paste0(str_sub(workshopNDVIfiles,-12,-9),"-01-01")),"%Y %b %d")
NDVIPeriods <- paste0(NDVIdates,"-",format(as.Date(NDVIdates,format = "%Y %b %d")+15,"%b %d"))
NDVIPeriods[23] <- "2002 Dec 19-Dec 31"
NDVIPeriods[46] <- "2022 Dec 19-Dec 31"
NDVI <- setZ(NDVI,NDVIPeriods,"Period")
names(NDVI) <- NDVIPeriods

plot(NDVI$X2002.Jan.01.Jan.16)
target <- raster(resolution=res(NDVI)/2,crs=proj4string(NDVI),ext=extent(NDVI))
res(target)
NDVI <- resample(NDVI,target)
res(NDVI)
crs(NDVI)
plot(NDVI$X2002.Jan.01.Jan.16)


NDVI5c <- crop(NDVI,cropLongLat5c_small)
NDVI5c <- mask(NDVI5c,cropLongLat5c_small)
NDVI5c <- setZ(NDVI5c,NDVIPeriods,"Period")
plot(NDVI5c$X2002.Jan.01.Jan.16)

NDVIZIP <- crop(NDVI,cropLongLat_zip)
NDVIZIP <- mask(NDVIZIP,cropLongLat_zip)
plot(NDVIZIP$X2002.Jan.01.Jan.16)
res(NDVIZIP)
crs(NDVIZIP)
target <- raster(resolution=res(NDVIZIP)/2.5,crs=proj4string(NDVIZIP),ext=extent(NDVIZIP))
res(target)
NDVIZIP <- resample(NDVIZIP,target)
res(NDVIZIP)
crs(NDVIZIP)
NDVIZIP <- mask(NDVIZIP,cropLongLat_zip)
NDVIZIP <- setZ(NDVIZIP,NDVIPeriods,"Period")
plot(NDVIZIP$X2002.Jan.01.Jan.16)



values(NDVIZIP) <- values(NDVIZIP)*0.0001
values(NDVI5c) <- values(NDVI5c)*0.0001
NDVIindices <- rep(1:2,each=23)
NDVI5cmean <- stackApply(NDVI5c,NDVIindices,mean)
NDVIzipmean <- stackApply(NDVIZIP,NDVIindices,mean)

NDVI5c2002<- NDVI5cmean[[1]]
NDVI5c2022<- NDVI5cmean[[2]]
NDVIZIP2002 <- NDVIzipmean[[1]]
NDVIZIP2022 <- NDVIzipmean[[2]]

plot(NDVI5c2002)
plot(NDVI5c2022)

tmap_mode("plot")


range(values(NDVI5c2022),na.rm=T)
range(values(NDVI5c2002),na.rm=T)


NDVI5c2002Plot<- tm_shape(NDVI5c2002)+
  tm_raster(col=names(NDVI5c2002),palette = NDVIcolorvector,
            breaks =c(seq(min(values(NDVI5c2002),na.rm=T),max(values(NDVI5c2022),na.rm=T),0.15),
                      max(values(NDVI5c2022),na.rm=T)),
           # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "ZIP 2022 NDVI",legend.reverse=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
    tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
    tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD13A2v6 NDVI",
             position = c(0.5,0))+
  tm_layout(frame=F,
            main.title=paste0("Study Area 2002 NDVI"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))



NDVI5c2022Plot<- tm_shape(NDVI5c2022)+
  tm_raster(col=names(NDVI5c2022),palette = NDVIcolorvector,
            breaks =c(seq(min(values(NDVI5c2002),na.rm=T),max(values(NDVI5c2022),na.rm=T),0.15),
                      max(values(NDVI5c2022),na.rm=T)),
           #labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "NDVI",legend.reverse=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
            )+
  
  tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
  tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD13A2v6 NDVI",
             position = c(0.5,0))+
  tm_layout(frame=F,
            main.title=paste0("Study Area 2022 NDVI"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))

tmap_save(NDVI5c2022Plot,"NDVI2022.png",dpi=300)


##
cropLongLat_neighborhoodzip <- st_cast(cropLongLat_neighborhoodzip)

ymod <- c(0,-0.5,-1,-0.5,0)
xmod <- c(3,0,0,0,0)
cropLongLat_zip<-cbind(cropLongLat_zip,ymod,xmod)



NDVIZIP2002Plot<- tm_shape(NDVIZIP2002)+
  tm_raster(col=names(NDVIZIP2002),palette = NDVIcolorvector,
            breaks =c(seq(min(values(NDVI5c2002),na.rm=T),max(values(NDVI5c2022),na.rm=T),0.15),
                      max(values(NDVI5c2022),na.rm=T)),
            labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "Green Space\n(amount of \nvegetation)",legend.reverse=T)+
  tm_shape(zip_motorways)+
    tm_lines(col = "Highways", 
          palette = "lightgrey",
          labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=3,lty="dotted")+
    tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  tm_shape(cropLongLat_neighborhoodzip)+
    tm_borders()+
    tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Green Spaces (amount of vegetation) in 2002"),
            main.title.size=1.4,
            main.title.position="center",
           legend.show=F,
            )

 

range(values(NDVIZIP2002),na.rm=T)
range(values(NDVIZIP2022),na.rm=T)


NDVIZIP2022Plot<- tm_shape(NDVIZIP2022)+
  tm_raster(col=names(NDVIZIP2022),palette = NDVIcolorvector,
            breaks =c(seq(min(values(NDVI5c2002),na.rm=T),max(values(NDVI5c2022),na.rm=T),0.15),
                      max(values(NDVI5c2022),na.rm=T)),
            #labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "NDVI",legend.reverse=T)+
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "lightgrey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3)+
  tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  #tm_shape(cropLongLat_neighborhoodzip)+
  #tm_borders()+
  #tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            main.title=paste0("ZIP 2022 NDVI"),
            main.title.size=1.4,
            main.title.position="center",
            legend.show=F,
  )
tmap_save(NDVIZIP2022Plot,"NDVI2022ZIP.png",dpi=300)

hist(values(NDVI5c))

NDVI2022 <- tmap_arrange(NDVI5c2022Plot,NDVIZIP2022Plot)
tmap_save(NDVI2022,"NDVI2022.pdf",dpi=300,units="in",height = 12,width=19.1)
NDVI2002 <- tmap_arrange(NDVI5c2002Plot,NDVIZIP2002Plot)
tmap_save(NDVI2002,"NDVI2002.pdf",dpi=300,units="in",height = 12,width=19.1)


#######AOD
AOD5c2002 <- AOD5c$X2002.August
AOD5c2022 <- AOD5c$X2022.August

AODZIP2002 <- AODZIP$X2002.August
AODZIP2022 <- AODZIP$X2022.August

range(values(AOD5c2002),na.rm=T)
range(values(AOD5c2022),na.rm=T)

AOD5c2002Plot<- tm_shape(AOD5c2002,raster.downsample = F)+
  tm_raster(col=names(AOD5c2002),palette = rev(airpolcolorvector),
            breaks =c(seq(min(values(AOD5c2022),na.rm=T),max(values(AOD5c2002),na.rm=T),0.06),
                      max(values(AOD5c2002),na.rm=T)),
           labels = c("Low\nPollution","","","","","","","High\nPollution"),
            style = "cont",title = "Air Pollution",legend.reverse=T, interpolate=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
  tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 LST\n2002 Aug",
             position = c(0.6,0))+
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Air Pollution in 2002"),
            main.title.size=1.4,
            main.title.position="center",
            legend.height=-0.4,
            legend.text.size=0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))



hist(values(AOD5c2002))
hist(values(AOD5c2022))
five_c_highways$Highways <- "Highway"


AOD5c2022Plot<- tm_shape(AOD5c2022)+
  tm_raster(col=names(AOD5c2022),palette = rev(airpolcolorvector),
            breaks =c(seq(min(values(AOD5c2022),na.rm=T),max(values(AOD5c2002),na.rm=T),0.06),
                      max(values(AOD5c2002),na.rm=T)),
            labels = c("Low\nPollution","","","","","","","High\nPollution"),
            style = "cont",title = "Air Pollution",legend.reverse=T, interpolate=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
    tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
    tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 LST\n2022 Aug",
             position = c(0.6,0))+
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Air Pollution in 2022"),
            main.title.size=1.4,
            main.title.position="center",
            legend.height=-0.4,
            legend.text.size=0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))

#Alt 2022 5c
plot(AOD5c)
AOD5c2022Plot<- tm_shape(AOD5c)+
  tm_raster(col=names(AOD5c),palette = rev(airpolcolorvector),
            breaks =c(seq(0.084,0.155,0.015),
                      max(values(AOD5c),na.rm=T)),
            #labels = c("Low\nPollution","","","","","","","High\nPollution"),
            style = "cont",title = "AOD",legend.reverse=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
  tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6",
             position = c(0.5,0))+
  tm_layout(frame=F,
            legend.height=-0.4,
            legend.text.size=0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
tmap_save(AOD5c2022Plot,"5c 2022 AOD.png",dpi=300)
#

###ZIP

AODZIP2002Plot<- tm_shape(AODZIP2002)+
  tm_raster(col=names(AODZIP2002),palette = rev(airpolcolorvector),
            breaks =c(seq(min(values(AOD5c2022),na.rm=T),max(values(AOD5c2002),na.rm=T),0.06),
                      max(values(AOD5c2002),na.rm=T)),
            labels=c("","","","",""),
            style = "cont",title = "Air Pollution",legend.reverse=T)+
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3,lty="dotted")+
  tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  tm_shape(cropLongLat_neighborhoodzip)+
  tm_borders()+
  tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Air Pollution in 2002"),
            main.title.size=1.4,
            main.title.position="center",
            legend.show=F,
  )



AODZIP2022Plot<- tm_shape(AODZIP2022)+
  tm_raster(col=names(AODZIP2022),palette = rev(airpolcolorvector),
            breaks =c(seq(min(values(AOD5c2022),na.rm=T),max(values(AOD5c2002),na.rm=T),0.06),
                      max(values(AOD5c2002),na.rm=T)),
            labels=c("","","","",""),
            style = "cont",title = "Air Pollution",legend.reverse=T)+
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "lightgrey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3,lty="dotted")+
  tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  tm_shape(cropLongLat_neighborhoodzip)+
  tm_borders()+
  tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Air Pollution in 2022"),
            main.title.size=1.4,
            main.title.position="center",
            legend.show=F,
  )
plot(AODZIP)

#Alt ZIP 2022
AODZIP2022Plot<- tm_shape(AODZIP)+
  tm_raster(col=names(AODZIP),palette = rev(airpolcolorvector),
            breaks =c(seq(0.084,0.155,0.015),
                      max(values(AOD5c),na.rm=T)),
            #labels=c("","","","",""),
            style =  "cont",title = "Air Pollution",legend.reverse=T
            )+
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "lightgrey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3)+
  tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  #tm_shape(cropLongLat_neighborhoodzip)+
  #tm_borders()+
  #tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            legend.show=F,
  )
tmap_save(AODZIP2022Plot,"ZIP 2022 AOD.png",dpi=300)

###

AOD2022 <- tmap_arrange(AOD5c2022Plot,AODZIP2022Plot)
AOD2002 <- tmap_arrange(AOD5c2002Plot,AODZIP2002Plot)
tmap_save(AOD2022,"AOD2022.pdf",dpi=300,units="in",height = 12,width=19.1)
tmap_save(AOD2002,"AOD2002.pdf",dpi=300,units="in",height = 12,width=19.1)




#######LST
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2002_01_01 - 2023_03_23/LST_Day_1km")
LSTfiles <- list.files()[c(grep("2002",list.files(),fixed=TRUE),grep("2022",list.files(),fixed=TRUE))]

LST <- stack(LSTfiles)
crs(LST)
res(LST)
str_sub(LSTfiles,-12,-9)
str_sub(LSTfiles,-7,-5)
LSTdates <- format(as.Date(as.numeric(str_sub(LSTfiles,-7,-5))-1,
                            origin=paste0(str_sub(LSTfiles,-12,-9),"-01-01")),"%Y %b %d")
LSTdates_asdates <- as.Date(as.numeric(str_sub(LSTfiles,-7,-5))-1,
                            origin=paste0(str_sub(LSTfiles,-12,-9),"-01-01"))

LST <- setZ(LST,LSTdates_asdates,"Date")
names(LST) <- LSTdates

#LST <- LST[[c(grep("Aug",names(LST),fixed=TRUE),grep("Sep",names(LST),fixed=TRUE))]]
LST <- LST[[grep("2022",names(LST),fixed=TRUE)]]
values(LST) <- (values(LST)*0.02)-273
#values(LST) <- (values(LST)*(9/5))+32
LSTdates<- str_sub(names(LST),2,12)
LSTdates <- as.Date(LSTdates,"%Y.%b.%d")
LST <- setZ(LST,LSTdates,"Date")

#LST <- LST[[order(LSTdates)]]
#LST2002 <- LST[[29:44]]
#LST2022 <- LST[[90:105]]

#indices <- rep(1,16)
indices <- rep(1,306)
LST2022 <- stackApply(LST,indices,mean)

hist(values(LST2002))
hist(values(LST2022))
LST2002 <- stackApply(LST2002,indices,mean)
LST2022 <- stackApply(LST2022,indices,mean)
  
plot(LST2002)
plot(LST2022)
target <- raster(resolution=res(LST2022)/5,crs=proj4string(LST2022),ext=extent(LST2022))
res(target)
LST2002 <- resample(LST2002,target)
LST2022 <- resample(LST2022,target)
res(LST2002)
crs(LST2002)
plot(LST2002)
plot(LST2022)

LST20225c <- mask(LST2022,cropLongLat5c_small)
LST20025c <- mask(LST2002,cropLongLat5c_small)
plot(LST20225c)
plot(LST20025c)


LST2022ZIP <- crop(LST2022,cropLongLatzip_small)
LST2022ZIP <- mask(LST2022ZIP,cropLongLatzip_small)

LST2002ZIP <- crop(LST2002,cropLongLatzip_small)
LST2002ZIP <- mask(LST2002ZIP,cropLongLatzip_small)

plot(LST2022ZIP)
plot(LST2002ZIP)

res(LST2022ZIP)
target <- raster(resolution=res(LST2022ZIP)/2,crs=proj4string(LST2022ZIP),ext=extent(LST2022ZIP))
res(target)
LST2022ZIP <- resample(LST2022ZIP,target)
LST2002ZIP <- resample(LST2002ZIP,target)

LST2022ZIP <- mask(LST2022ZIP,cropLongLatzip_small)
LST2002ZIP <- mask(LST2002ZIP,cropLongLatzip_small)

plot(LST2022ZIP)
plot(LST2002ZIP)


tmap_mode("plot")


range(values(LST20025c),na.rm=T)
range(values(LST20225c),na.rm=T)


####
LST5c2002Plot<- tm_shape(LST20025c)+
  tm_raster(col=names(LST20025c),palette = rev(tempcolorvector),
            breaks =c(seq(min(values(LST20225c),na.rm=T),max(values(LST20025c),na.rm=T),4),
                      max(values(LST20025c),na.rm=T)),
            labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
            style = "cont",title = "Land\nTemperature",legend.reverse=T, interpolate=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
  tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 LST\n2002 Aug 29-Sep 13",
             position = c(0.6,0))+
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Urban Heat in 2002"),
            main.title.size=1.4,
            main.title.position="center",
            legend.height=-0.4,
            legend.text.size=0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))



LST5c2022Plot<- tm_shape(LST20225c)+
  tm_raster(col=names(LST20225c),palette = rev(inferno),
            breaks =c(seq(min(values(LST20225c),na.rm=T),max(values(LST20225c),na.rm=T),2),
                      max(values(LST20225c),na.rm=T)),
            #labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
            style = "cont",title = "Temperature °C",legend.reverse=T, interpolate=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod")+
  tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 LST",
             position = c(0.6,0))+
  tm_layout(frame=F,
            main.title=paste0("Study Area 2022 LST"),
            main.title.size=1.4,
            main.title.position="center",
            legend.height=-0.4,
            legend.text.size=0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))


#Alternate 5c 2022
LST5c2022Plot<- tm_shape(LST20225c)+
  tm_raster(col=names(LST20225c),palette = rev(tempcolorvector),
            breaks =seq(19,29,2),
                      
            #labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
            style = "cont",title = "Temperature °C",legend.reverse=T, interpolate=T)+
  tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
  
  tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,col="black",ymod="ymod",xmod="xmod")+
  tm_borders(col="black",lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_credits("Data Source: NASA MOD11A1v6 LST",
             position = c(0.5,0))+
  tm_layout(frame=F,
            legend.height=-0.4,
            legend.text.size=0.8,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
#
tmap_save(LST5c2022Plot,"5c LST 2022.png",dpi=300)


###ZIP

LSTZIP2002Plot<- tm_shape(LST2002ZIP)+
  tm_raster(col=names(LST2002ZIP),palette = rev(tempcolorvector),
            breaks =c(seq(min(values(LST20225c),na.rm=T),max(values(LST20025c),na.rm=T),4),
                      max(values(LST20025c),na.rm=T)),
            labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
            style = "cont",title = "Land\nTemperature",legend.reverse=T, interpolate=T)+
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3,lty="dotted")+
  tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  tm_shape(cropLongLat_neighborhoodzip)+
  tm_borders()+
  tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Urban Heat in 2002"),
            main.title.size=1.4,
            main.title.position="center",
            legend.show=F,
  )



LSTZIP2022Plot<- tm_shape(LST2022ZIP)+
  tm_raster(col=names(LST2002ZIP),palette = rev(tempcolorvector),
            breaks =c(seq(min(values(LST20225c),na.rm=T),max(values(LST20025c),na.rm=T),4),
                      max(values(LST20025c),na.rm=T)),
            labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
            style = "cont",title = "Land\nTemperature",legend.reverse=T, interpolate=T)+
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "lightgrey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3,lty="dotted")+
  tm_text("ZIP.Code",shadow=T,xmod="xmod",ymod="ymod")+
  tm_shape(cropLongLat_neighborhoodzip)+
  tm_borders()+
  tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            main.title=paste0("NASA Maps - Urban Heat in 2022"),
            main.title.size=1.4,
            main.title.position="center",
            legend.show=F,
  )

#Alternate ZIP 2022
LSTZIP2022Plot<- tm_shape(LST2022ZIP)+
  tm_raster(col=names(LST2022ZIP),palette = rev(tempcolorvector),
            breaks =seq(19,29,2),
            
            #labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
            style = "cont",title = "Temperature °C",legend.reverse=T, interpolate=T)+
            #labels = c("Low\nTemperature","","","","","","","High\nTemperature"),
  tm_shape(zip_motorways)+
  tm_lines(col = "Highways", 
           palette = "lightgrey",
           labels = c(""),
           lwd=3)+
  tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=3)+
  tm_text("ZIP.Code",col="black",shadow=T,xmod="xmod",ymod="ymod")+
  #tm_shape(cropLongLat_neighborhoodzip)+
  #tm_borders()+
  #tm_text("NAME",size=0.65,shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+  # add a scale bar
  tm_layout(frame=F,
            legend.show=F,
  )
tmap_save(LSTZIP2022Plot,"ZIP LST 2022.png",dpi=300)
#

LST2022 <- tmap_arrange(LST5c2022Plot,LSTZIP2022Plot)
LST2002 <- tmap_arrange(LST5c2002Plot,LSTZIP2002Plot)
tmap_save(LST2022,"LST2022.pdf",dpi=300,units="in",height = 12,width=19.1)
tmap_save(LST2002,"LST2002.pdf",dpi=300,units="in",height = 12,width=19.1)

