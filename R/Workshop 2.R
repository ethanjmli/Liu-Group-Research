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
library(gifski)

tempcolorvector <- colorRampPalette(brewer.pal(11,"Spectral"))(1000)
airpolcolorvector <- c(inferno(1000))

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


crop_parameters_zip <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/expandedzip.shp")
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

#####Highways + Misc GIS######
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
zip_highways <- st_crop(five_c_highways, zipcrop)

five_c_highways <- st_make_valid(five_c_highways)
zip_highways <- st_make_valid(zip_highways)

colnames(five_c_highways)[73] <- "Highway Type"
colnames(zip_highways)[73] <- "Highway Type"
tmap_options(check.and.fix = TRUE)

five_c_highways$Highways <- "Highway"
five_c_motorways <- subset(five_c_highways,`Highway Type`=="motorway")

zip_highways$Highways <- "Highway"
zip_motorways <- subset(zip_highways,`Highway Type` == "motorway")

interstates <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/Expressways_Georgia.shp")
interstates <- st_intersection(interstates, cropLongLat5c_small)
interstateszip <- st_crop(interstates, zipcrop)

interstates <- st_make_valid(interstates)
interstateszip <- st_make_valid(interstateszip)

interstateszip$ROAD_NAME <- str_sub(interstateszip$ROAD_NAME,1,-3)
interstateszip$Highways <- "Highways"
#interstateszip <- aggregate(interstateszip,interstateszip$ROAD_NAME,mean)

tm_shape(interstateszip)+
  tm_lines()+
  tm_text("Label")

###MARTA###
MARTA <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/MARTA_Routes.geojson")
MARTA <- st_intersection(MARTA,cropLongLat5c_small)
MARTAzip <- st_crop(MARTA,zipcrop)
MARTA<- st_make_valid(MARTA)
MARTAzip <- st_make_valid(MARTAzip)

MARTAzip$MARTA <- "MARTA"

tm_shape(MARTA)+
  tm_lines()
tm_shape(MARTAzip)+
  tm_lines()

###Parks###
#temp <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/test2.shp")
Parks <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/test2.shp")
Parks <- st_make_valid(Parks)
Parks <- st_intersection(Parks,cropLongLat5c_small)
Parkszip <- Parks
#Parkszip <- subset(Parks, NAME %in% c("WESTSIDE PARK",
 #                                     "Grant Park",
  #                                    "Perkerson Park",
   #                                   "Lakewood Fairgrounds & HiFi Buys Amphitheater"))
#Parkszip <- st_intersection(Parks,cropLongLatzip_small)
#Parkszip <- st_make_valid(Parkszip)
#Parkszip <- subset(Parkszip,fclass == "park")
colnames(Parkszip)[3] <- "Parks"
Parkszip$Parks <- " "
Parkszip <- st_cast(Parkszip,"MULTILINESTRING")
#st_write(Parkszip,"C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/Workshop2Parks.shp")

tm_shape(Parks)+
  tm_dots()

tm_shape(Parkszip)+
  tm_polygons()

###Industry###


####Other POI####
par(lheight = 0.7)
library(osmdata)
available_features()


available_tags(feature = "tourism")

zip_aquarium <- osmdata_sf(add_osm_feature(opq(zipcrop),#Georgia Aquarium
                                      key = "tourism", value = c("aquarium")))
zip_aquarium2 <- zip_aquarium$osm_polygons
zip_aquarium2 <- subset(zip_aquarium2, name == "Georgia Aquarium")

#zip_postoffice <-  osmdata_sf(add_osm_feature(opq(zipcrop),#Post Office
 #                                       key = "amenity", value = c("post_office")))
#zip_postoffice2 <- zip_postoffice$osm_points
#zip_postoffice2 <- subset(zip_postoffice2, name == "Atlanta Post Office")

available_tags(feature = "leisure")

zip_parks <- osmdata_sf(add_osm_feature(opq(zipcrop), #Westside, Grant, Mercedes-Benz, Rodney Cook
                                           key = "leisure", value = c("stadium","park"))) 
zip_parks2 <- zip_parks$osm_polygons
zip_parks2 <- subset(zip_parks2, name %in% c("Mercedes-Benz Stadium",
                                             "Grant Park", 
                                             "Westside Park",
                                             "Rodney Cook Sr. Park in Historic Vine City"
                                            ))
zip_parks3 <- zip_parks$osm_points #Lake Charlotte, Perkerson
zip_parks3 <- subset(zip_parks3, name %in% c("Lake Charlotte Nature Preserve",
                                             "Perkerson Park"))

zip_parks4 <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/Atlanta_Parks.shp")
zip_parks4 <- subset(zip_parks4,NAME == "Beecher Hills") #Beecher Hills



available_tags(feature = "amenity")

zip_college <- osmdata_sf(add_osm_feature(opq(zipcrop), #Spelman, Georgia Tech, Atlanta Technical, Lakewood Amphitheatre
                                        key = "amenity", value = c("college","school","university","theatre"))) 
zip_college2 <- zip_college$osm_polygons
zip_college2 <- subset(zip_college2, name %in% c("Spelman College",
                                                 "Georgia Tech",
                                                 "Atlanta Technical College",
                                                 "Cellairis Amphitheatre at Lakewood"))
zip_college2 <- zip_college2[c(1,2,4,5),]



available_tags(feature = "railway")
zip_train <- osmdata_sf(add_osm_feature(opq(zipcrop), #Inman Norfolk Southern Yard
                                        key = "railway", value = c("yard"))) 
zip_train2 <- zip_train$osm_points
zip_train2 <- subset(zip_train2, name == "NS Inman Yard")



available_tags(feature = "aeroway")
zip_airport <- osmdata_sf(add_osm_feature(opq(zipcrop), #Spelman
                                          key = "aeroway", value = c("aerodrome"))) 
zip_airport2 <- zip_airport$osm_polygons



available_tags(feature = "name")
zip_beltline <- osmdata_sf(add_osm_feature(opq(zipcrop), #Beltline
                                          key = "name", value = c("Atlanta Beltline Trail")))
zip_beltline2 <- zip_beltline$osm_multilines
zip_beltline2 <- st_crop(zip_beltline2,zipcrop)
tm_shape(zip_beltline2)+
  tm_lines()
colnames(zip_beltline2)[2]<-"Beltline"


#available_tags(feature= "building")
#zip_stations <- osmdata_sf(add_osm_feature(opq(zipcrop), #Beltline
 #                                          key = "building", value = c("train_station")))
#zip_stations2 <- zip_stations$osm_polygons

zip_stations <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/MARTA_Stops.shp")
zip_stations <- st_intersection(zip_stations,cropLongLatzip_small)
zip_stations <- st_make_valid(zip_stations)
zip_stations <- subset(zip_stations, stop_name %in% c("WEST END STATION",
                                                "OAKLAND CITY STATION",
                                                "VINE CITY STATION",
                                                "ASHBY STATION",
                                                "WEST LAKE STATION",
                                                "BANKHEAD STATION"
                                                ))
zip_stations <- zip_stations[order(zip_stations$stop_name),]
zip_stations <- zip_stations[c(1,5,10,15,19,24),]
colnames(zip_stations)[4]<-"name"
#setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research")
#setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/Georgia OSM Shapefiles")

#pois <- st_read("gis_osm_pois_free_1.shp") #Brownsmill park
#table(pois$fclass)
#pois <- subset(pois, name == "Brownsmill Park")

zip_pois <- bind_rows(zip_aquarium2,
                      zip_parks2,
                      zip_parks3,
                      zip_parks4,
                      zip_airport2,
                      zip_train2,
                      zip_stations,
                      zip_college2)
zip_pois <- zip_pois[-8,c(1,2,145)]

zip_pois_HJ <- zip_pois[8,]
zip_pois <- st_cast(zip_pois,"POINT")
zip_pois <- rbind(zip_pois,zip_pois_HJ)
zip_pois[5,2]<-"Westside Reservoir\nPark"
zip_pois[16,2]<-"Atlanta University\nCenter"
#zip_pois$shape <- c(1,4,7,10,16,20,25)

#tm_shape(zip_pois)+
#  tm_borders()+
#tm_dots(size=0.2)+
#tm_shape(Parkszip)+
#  tm_borders()
  #tm_text("name")

colnames(zip_pois)[2] <- "Points of\nInterest"
zip_pois[8,2] <- "Hartsfield-Jackson Atlanta\nInternational Airport"
zip_pois[7,2] <- "Lake Charlotte\nNature Preserve"
zip_pois[12,2] <- "Oakland City\nStation"
zip_pois[11,2] <- "Bankhead\nStation"
zip_pois[14,2] <- "West End\nStation"
zip_pois[13,2] <- "Vine City\nStation"
zip_pois[10,2] <- "Ashby\nStation"
zip_pois<-zip_pois[-8,]
zip_pois[19,2] <- "Hartsfield-Jackson Atlanta\nInternational Airport"
zip_pois[4,2] <- "Rodney Cook\nPark"
zip_pois[3,2] <- "Mercedes-Benz\nStadium"
zip_pois[17,2] <- "Lakewood\nAmphitheatre"
zip_pois[18,2] <- "Atlanta Technical\nCollege"
zip_pois[14,2] <- "West Lake\nStation"
zip_pois[1,2] <- "Georgia\nAquarium"
zip_pois$xmod <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
zip_pois$ymod <- 0.4
zip_pois[18,4]<-0
zip_pois[12,5] <- 0.3
zip_pois[3,4] <- 0.9
zip_pois[3,5] <- 0
zip_pois[1,4]<-0.1
zip_pois[16,5] <- 0.3
zip_pois[4,4] <-0
zip_pois[4,5]<-0.3
zip_pois[17,4] <- 0
zip_pois[6,2]<- "Perkerson\nPark"
zip_pois[6,4] <- 0
zip_pois[6,5] <- -0.3
zip_pois[8,5] <- 0.3
zip_pois[10,5] <- 0.3
zip_pois[5,5] <- -0.4
zip_pois[2,5] <- -0.15
zip_pois[13,5] <- -0.4
zip_pois$POIs <- " "
zip_pois <- zip_pois[-19,]


#####Expand ZIP####
library(tidycensus)
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April")
newzip <- get_acs(
  geography = "zcta", 
  variables = "B01003_001",
  #state = "GA", 
  zcta = c("30318","30314","30310","30315","30354","30312"),
  year = 2021,
  geometry = TRUE
)
newzip <- newzip[,c(1,2,6)]
st_write(newzip,"expandedzip.shp")

tm_shape(cropLongLatzip_small)+
  tm_borders()

#####NDVI####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")

NDVI <- stack(list.files())
NDVI
res(NDVI)
names(NDVI)
NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
                     origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
values(NDVI) <- values(NDVI)*0.0001
NDVI <- subset(NDVI,c(grep('113',names(NDVI),fixed=T),
                     grep('129',names(NDVI),fixed=T),
                     grep('145',names(NDVI),fixed=T),
                     grep('161',names(NDVI),fixed=T),
                     grep('177',names(NDVI),fixed=T),
                     grep('193',names(NDVI),fixed=T),
                     grep('209',names(NDVI),fixed=T),
                     grep('225',names(NDVI),fixed=T),
                     grep('241',names(NDVI),fixed=T)
                     ))
NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
                     origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
NDVI <- NDVI[[order(NDVIdates)]]
NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
                     origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
NDVIyears <- format(NDVIdates,"%Y")
NDVI <- stackApply(NDVI,NDVIyears,mean)

NDVI <- crop(NDVI,cropLongLat5c_small)
NDVI <- mask(NDVI, cropLongLat5c_small)
#NDVI_df <- data.frame(rasterToPoints(NDVI))
#colnames(NDVI_df)[3:ncol(NDVI_df)] <- unique(NDVIyears)
#NDVI_df <- NDVI_df[,c(-1,-2)]
#mean(NDVI_df$`2002`)
#NDVI_df <- apply(NDVI_df,2,mean,na.rm=T)
#NDVI_df <- as.data.frame(NDVI_df)
#NDVI_df$date <- rownames(NDVI_df)
#NDVI_df$date <- as.Date(paste0(NDVI_df$date,"-01-01")   # Convert Julian day to date
#)

N#DVI_df$season <- time2season(NDVI_df$date,out.fmt="seasons")
c#olnames(NDVI_df)[1]<-"NDVI"
N#DVI_df$group = "5c"


NDVIzip <- crop(NDVI,zipcrop)
NDVIzip <- mask(NDVIzip, cropLongLat_zip)
#NDVIzip_df <- data.frame(rasterToPoints(NDVIzip))
#colnames(NDVIzip_df)[3:ncol(NDVIzip_df)] <- unique(NDVIyears)
#NDVIzip_df <- NDVIzip_df[,c(-1,-2)]
#mean(NDVIzip_df$`2002`)
#NDVIzip_df <- apply(NDVIzip_df,2,mean,na.rm=T)
#NDVIzip_df <- as.data.frame(NDVIzip_df)
#NDVIzip_df$date <- rownames(NDVIzip_df)
#NDVIzip_df$date <- as.Date(paste0(NDVI_df$date,"-01-01"))   # Convert Julian day to date

#NDVIzip_df$season <- time2season(NDVIzip_df$date,out.fmt="seasons")
#colnames(NDVIzip_df)[1]<-"NDVI"
#NDVIzip_df$group = "ZIP"

#NDVI_df <- NDVI_df[-22,]
#NDVIzip_df <- NDVIzip_df[-22,]
#combined_NDVI_df <- rbind(NDVI_df,NDVIzip_df)

target <- raster(resolution=res(NDVI)/2,crs=proj4string(NDVI),ext=extent(NDVI))
res(target)
NDVI <- resample(NDVI,target)

targetzip <- raster(resolution=res(NDVIzip)/5,crs=proj4string(NDVIzip),ext=extent(NDVIzip))
res(targetzip)
NDVIzip <- resample(NDVIzip,targetzip)
NDVIzip <- mask(NDVIzip, cropLongLat_zip)

NDVIlist <- list()
for(i in 1:nlayers(NDVI)){
  
  
  NDVIlist[[i]]<-
    tm_shape(NDVI[[i]])+
    tm_raster(col=names(NDVI[[i]]),palette = NDVIcolorvector,
              breaks =c(seq(min(values(NDVI),na.rm=T),max(values(NDVI),na.rm=T),0.1),
                        max(values(NDVI),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Greenspace",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MOD13A2v6 500m NDVI",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area",str_sub(names(NDVI[[i]]),-4,-1),"Greenspace\nMay-August"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(NDVI[[4]])
NDVIlist[[4]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(NDVIlist,"2002-2022_MayAugustNDVI.gif",delay=150,width=800,height=800)



NDVIziplist <- list()
for(i in 1:nlayers(NDVIzip)){
  
  
  NDVIziplist[[i]]<-
    tm_shape(NDVIzip[[i]])+
    tm_raster(col=names(NDVIzip[[i]]),palette = NDVIcolorvector,
              breaks =c(seq(min(values(NDVIzip),na.rm=T),max(values(NDVIzip),na.rm=T),0.1),
                        max(values(NDVIzip),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Greenspace",legend.reverse=T)+
    tm_shape(zip_motorways)+
      tm_lines(col = "Highways", 
             palette = "grey",
             labels = "",
             lwd=2
      )+
    
    tm_shape(MARTAzip)+
      tm_lines(col = "MARTA",
               palette="blue",
               labels="",
               lwd=1.5)+
    tm_shape(Parkszip)+
      tm_polygons(col="Parks",
                  palette = "green",
                  labels=""
                  )+
    tm_shape(cropLongLat_zip)+
      tm_borders(col="black",lwd=2,lty="solid")+
      tm_text("GEOID",col="black",shadow=T,xmod = "xmod")+
    tm_scale_bar(position = c(0.03,0.02))+
    tm_compass(position = c(0.03,0.07))+
    tm_credits("Data Source: NASA MOD13A2v6 500m NDVI",
               position = c(0,0))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes",str_sub(names(NDVIzip[[i]]),-4,-1),"Greenspace\nMay-August"),
              main.title.size=1.4,
              main.title.position="center",
              legend.outside=T,
              
              legend.outside.size=0.1
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33,
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
              )
  
}
plot(NDVI[[4]])
NDVIlist[[4]]
NDVIziplist[[4]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(NDVIziplist,"2002-2022_MayAugustNDVI_ZIP.gif",delay=150,width=800,height=800)
unique(NDVIyears)
for(i in 1:length(NDVIziplist)){
  tmap_save(NDVIziplist[[i]],paste0(unique(NDVIyears)[i],"_MayAugustNDVI_ZIP.png"),dpi=300)
}

####LST####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD11A1v6 1km LST 2002_01_01 - 2023_03_23/LST_Day_1km")
LST <- stack(list.files())
LSTdates <- as.Date(as.numeric(str_sub(names(LST),-3,-1))-1,
                    origin=paste0(str_sub(names(LST),-8,-5),"-01-01"))
#7579/16#
#LSTindices <- rep(1:474,each=16)
#LSTindices <- LSTindices[1:7579]
LSTmonths <- format(LSTdates,"%Y-%m")
LST <- stackApply(LST,LSTmonths,mean)


#LSTyears <- format(LSTdates,"%Y")
#LST <- stackApply(LST,LSTyears,mean)


LST <- crop(LST,cropLongLat5c_small)
LST <- mask(LST,cropLongLat5c_small)
#LST_df <- data.frame(rasterToPoints(LST))
#colnames(LST_df)[3:ncol(LST_df)] <- unique(LSTmonths)
#LST_df <- LST_df[,c(-1,-2)]
#mean(LST_df$`2002-01`)
#LST_df <- apply(LST_df,2,mean,na.rm=T)
#LST_df <- as.data.frame(LST_df)
#LST_df
#LST_df$date <- paste0(unique(LSTmonths),"-01")
#LST_df$date <- as.Date(LST_df$date)
#LST_df$season <- time2season(LST_df$date,out.fmt="seasons")
#LST_df$month <- format(LST_df$date,"%B")
#colnames(LST_df)[1]<-"LST"
#LST_df$group = "5c"
#LST_df$LST <- LST_df$LST*0.02-273
#LST_monthly_df <- LST_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


LST_zip <- crop(LST,zipcrop)
LST_zip <- mask(LST_zip,cropLongLatzip_small)
#LSTzip_df <- data.frame(rasterToPoints(LST_zip))
#colnames(LSTzip_df)[3:ncol(LSTzip_df)] <- unique(LSTmonths)
#LSTzip_df <- LSTzip_df[,c(-1,-2)]
#mean(LSTzip_df$`2002-01`)
#LSTzip_df <- apply(LSTzip_df,2,mean,na.rm=T)
#LSTzip_df <- as.data.frame(LSTzip_df)
#LSTzip_df
#LSTzip_df$date <- paste0(unique(LSTmonths),"-01")
#LSTzip_df$date <- as.Date(LSTzip_df$date)
#LSTzip_df$season <- time2season(LSTzip_df$date,out.fmt="seasons")
#LSTzip_df$month <- format(LST_df$date,"%B")
#colnames(LSTzip_df)[1]<-"LST"
#LSTzip_df$group = "ZIP"
#LSTzip_df$LST <- LSTzip_df$LST*0.02-273
#LSTzip_monthly_df <- LSTzip_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


LSThottestmonths <- subset(LST,c(grep('.06',names(LST),fixed=T),
                                 grep('.07',names(LST),fixed=T),
                                 grep('.08',names(LST),fixed=T)))
hottestmonths <- names(LSThottestmonths)
hottestmonths <- paste0(str_sub(hottestmonths,7,10),"-",str_sub(hottestmonths,12,13),"-01")
hottestmonths <- as.Date.character(hottestmonths)
LSThottestmonths <- LSThottestmonths[[order(hottestmonths)]]
names(LSThottestmonths)


LSTcoldestmonths <- subset(LST,c(grep('.12',names(LST),fixed=T),
                                 grep('.01',names(LST),fixed=T),
                                 grep('.02',names(LST),fixed=T)))
coldestmonths <- names(LSTcoldestmonths)
coldestmonths <- paste0(str_sub(coldestmonths,7,10),"-",str_sub(coldestmonths,12,13),"-01")
coldestmonths <- as.Date.character(coldestmonths)
LSTcoldestmonths <- LSTcoldestmonths[[order(coldestmonths)]]
names(LSTcoldestmonths)

hottestindices <- rep(1:21,each=3)
coldestindices <- rep(1:21,each=3)[-63]

LSThottestmonths <- stackApply(LSThottestmonths,hottestindices,mean)
LSTcoldestmonths <- stackApply(LSTcoldestmonths,coldestindices,mean)

values(LSThottestmonths)<-values(LSThottestmonths)*0.02-273
values(LSTcoldestmonths)<-values(LSTcoldestmonths)*0.02-273

values(LSThottestmonths)<-values(LSThottestmonths)*(9/5)+32
values(LSTcoldestmonths)<-values(LSTcoldestmonths)*(9/5)+32

target <- raster(resolution=res(LSThottestmonths)/4,crs=proj4string(LSThottestmonths),ext=extent(LSThottestmonths))
res(target)
LSThottestmonths <- resample(LSThottestmonths,target)
LSTcoldestmonths <- resample(LSTcoldestmonths,target)

plot(LSThottestmonths[[5]])
plot(LSTcoldestmonths[[5]])

names(LSThottestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")
names(LSTcoldestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")

hottestmonthlist <- list()

for(i in 1:nlayers(LSThottestmonths)){


hottestmonthlist[[i]]<-
  tm_shape(LSThottestmonths[[i]])+
  tm_raster(col=names(LSThottestmonths[[i]]),palette = rev(tempcolorvector),
            breaks =c(seq(min(values(LSThottestmonths),na.rm=T),max(values(LSThottestmonths),na.rm=T),5),
                      max(values(LSThottestmonths),na.rm=T)),
            # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
            style = "cont",title = "Temp °F",legend.reverse=T)+
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
  tm_credits("Data Source: NASA MOD11A1v6 1km LST",
             position = c(0.5,0))+
  tm_layout(frame=F,
            main.title=paste("Study Area",str_sub(names(LSThottestmonths[[i]]),-4,-1),"Land Surface Temperature\nJune-August"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))

}
plot(LSThottestmonths[[4]])
hottestmonthlist[[4]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(hottestmonthlist,"2002-2022_SummerMonthsLST.gif",delay=150,width=800,height=800)


coldestmonthlist <- list()

for(i in 1:nlayers(LSTcoldestmonths)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(LSTcoldestmonths[[i]])+
    tm_raster(col=names(LSTcoldestmonths[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(LSTcoldestmonths),na.rm=T),max(values(LSTcoldestmonths),na.rm=T),3.5),
                        max(values(LSTcoldestmonths),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area",str_sub(names(LSThottestmonths[[i]]),-4,-1),"Land Surface Temperature\nJanuary-February, December"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(LSTcoldestmonths[[4]])
names(LSTcoldestmonths)
coldestmonthlist[[13]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(coldestmonthlist,"2002-2022_WinterMonthsLST.gif",delay=150,width=800,height=800)


##########

LSTziphottestmonths <- subset(LST_zip,c(grep('.06',names(LST_zip),fixed=T),
                                 grep('.07',names(LST_zip),fixed=T),
                                 grep('.08',names(LST_zip),fixed=T)))
hottestmonths <- names(LSTziphottestmonths)
hottestmonths <- paste0(str_sub(hottestmonths,7,10),"-",str_sub(hottestmonths,12,13),"-01")
hottestmonths <- as.Date.character(hottestmonths)
LSTziphottestmonths <- LSTziphottestmonths[[order(hottestmonths)]]
names(LSTziphottestmonths)


LSTzipcoldestmonths <- subset(LST_zip,c(grep('.12',names(LST_zip),fixed=T),
                                 grep('.01',names(LST_zip),fixed=T),
                                 grep('.02',names(LST_zip),fixed=T)))
coldestmonths <- names(LSTzipcoldestmonths)
coldestmonths <- paste0(str_sub(coldestmonths,7,10),"-",str_sub(coldestmonths,12,13),"-01")
coldestmonths <- as.Date.character(coldestmonths)
LSTzipcoldestmonths <- LSTzipcoldestmonths[[order(coldestmonths)]]
names(LSTzipcoldestmonths)

hottestindices <- rep(1:21,each=3)
coldestindices <- rep(1:21,each=3)[-1]

LSTziphottestmonths <- stackApply(LSTziphottestmonths,hottestindices,mean)
LSTzipcoldestmonths <- stackApply(LSTzipcoldestmonths,coldestindices,mean)

values(LSTziphottestmonths)<-values(LSTziphottestmonths)*0.02-273
values(LSTzipcoldestmonths)<-values(LSTzipcoldestmonths)*0.02-273

values(LSTziphottestmonths)<-values(LSTziphottestmonths)*(9/5)+32
values(LSTzipcoldestmonths)<-values(LSTzipcoldestmonths)*(9/5)+32

target <- raster(resolution=res(LSTzipcoldestmonths)/10,crs=proj4string(LSTzipcoldestmonths),ext=extent(LSTzipcoldestmonths))
res(target)
LSTziphottestmonths <- resample(LSTziphottestmonths,target)
LSTzipcoldestmonths <- resample(LSTzipcoldestmonths,target)

plot(LSTziphottestmonths[[5]])
plot(LSTzipcoldestmonths[[5]])

names(LSTziphottestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")
names(LSTzipcoldestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")

LSTziphottestmonths <- mask(LSTziphottestmonths,cropLongLatzip_small)
LSTzipcoldestmonths <- mask(LSTzipcoldestmonths,cropLongLatzip_small)

plot(LSTziphottestmonths[[5]])
plot(LSTzipcoldestmonths[[5]])
##

hottestmonthlist <- list()

for(i in 1:nlayers(LSTziphottestmonths)){
  
  
  hottestmonthlist[[i]]<-
    tm_shape(LSTziphottestmonths[[i]])+
    tm_raster(col=names(LSTziphottestmonths[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(LSTziphottestmonths),na.rm=T),max(values(LSTziphottestmonths),na.rm=T),4),
                        max(values(LSTziphottestmonths),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    tm_shape(interstateszip)+
    tm_lines(col="Highways", 
             palette = "blue",
             labels = "",
             lwd=3
    )+
    #tm_text("Label")+
    
    #tm_shape(MARTAzip)+
    #tm_lines(col = "MARTA",
    #         palette="blue",
    #         labels="",
    #         lwd=1.5)+
    tm_shape(Parkszip)+
    tm_polygons(col="Parks",
                palette = "green",
                labels=""
    )+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("ZIP Code",col="black",shadow=T,xmod = "xmod",size=1.4)+
    tm_scale_bar(position = c(0.03,0.02))+
    tm_compass(position = c(0.03,0.07))+
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0,0))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes",str_sub(names(LSTziphottestmonths[[i]]),-4,-1),"Temperature\nJune-August Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.outside=T,
              
              legend.outside.size=0.1
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33,
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(LSTziphottestmonths[[11]])
hottestmonthlist[[11]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/June-August LST GIF+Individual Maps")
tmap_animation(hottestmonthlist,"2002-2022_SummerMonthsLST_zip.gif",delay=150,width=800,height=800)
names(LSTziphottestmonths)
for(i in 1:length(hottestmonthlist)){
  tmap_save(hottestmonthlist[[i]],paste0(str_sub(names(LSTziphottestmonths[[i]]),-4,-1),"_JuneAugust_LST_ZIP.png"),dpi=300)
}

coldestmonthlist <- list()

for(i in 1:nlayers(LSTzipcoldestmonths)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(LSTzipcoldestmonths[[i]])+
      tm_raster(col=names(LSTzipcoldestmonths[[i]]),palette = rev(tempcolorvector),
              breaks =round(
                        c(seq(min(values(LSTzipcoldestmonths),na.rm=T),max(values(LSTzipcoldestmonths),na.rm=T),3),
                        max(values(LSTzipcoldestmonths),na.rm=T)),
                        digits=0),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    tm_shape(cropLongLat_zip)+
      tm_borders(col="black",lwd=2,lty="solid")+
      tm_text("ZIP Code",col="black",shadow=T,xmod ="xmod",size=1.2)+
    tm_shape(interstateszip)+
      tm_lines(col = "Highways", 
             palette = "blue",
             labels = "",
             lwd=3
    )+
    tm_shape(Parkszip)+
      tm_lines(alpha = 1, col = "Parks",palette="green",lwd=2)+
    
    #tm_shape(MARTAzip)+
   # tm_lines(col = "MARTA",
    #        palette="blue",
    #         labels="",
    #         lwd=1.5)+
    tm_shape(zip_beltline2)+
      tm_lines(col="Beltline",lwd=2,palette="purple")+
   
    tm_shape(zip_pois[c(-12,-1),])+
      tm_dots(size = 0.1,
             col = "POIs",
             title = "Points of Interest",
             palette = "red",
             shape = 6,
             )+
      tm_text("Points of\nInterest",size=0.5,col="black",shadow=T,xmod="xmod",ymod="ymod")+
    tm_shape(zip_pois_HJ)+
      tm_borders()+
      tm_dots(size = 0.1,
            col = "red",
            shape = 6,
      )+
     tm_text("name",size=0.5,col="black",ymod=0.4)+
   
    tm_scale_bar(position = c(0.03,0.17))+
    tm_compass(position = c(0.01,0.24))+
    tm_credits("Data Source: NASA\nMOD11A1v6 1km LST",
               position = c(0.7,0))+
    tm_layout(frame=F,
              main.title=paste0("Select ZIP Codes Temperature\nDecember ",
                               as.numeric(str_sub(names(LSTzipcoldestmonths[[i]]),-4,-1))-1,
                                "-February ",
                               str_sub(names(LSTzipcoldestmonths[[i]]),-4,-1),
                                " Average"),
              main.title.size=1.4,
              main.title.position="left",
              legend.outside=T,
              legend.text.size=0.8,
              legend.outside.size=0.2
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(LSTzipcoldestmonths[[4]])
coldestmonthlist[[4]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/January-February, December LST GIF+Individual Maps")
tmap_animation(coldestmonthlist,"2002-2022_WinterMonthsLST_zip.gif",delay=150,width=800,height=800)
names(LSTzipcoldestmonths)
for(i in 1:length(coldestmonthlist)){
  tmap_save(coldestmonthlist[[i]],paste0(str_sub(names(LSTzipcoldestmonths[[i]]),-4,-1),"_JanFebDec_LST_ZIP.png"),dpi=300)
}


####Gap Filling Summer LST####
setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Georgia_subset_NAT")
list.files()
c(grep('-06-',list.files(),fixed=T),
  grep('-07-',list.files(),fixed=T),
  grep('-08-',list.files(),fixed=T))
list.files()[grep('-06-',list.files(),fixed=T)]
list.files()[grep('-07-',list.files(),fixed=T)]
list.files()[grep('-08-',list.files(),fixed=T)]
gapfillsummer <- stack(list.files()[c(grep('-06-',list.files(),fixed=T),
                                      grep('-07-',list.files(),fixed=T),
                                      grep('-08-',list.files(),fixed=T))])
gapfilldates <- names(gapfillsummer)
gapfilldates <- str_sub(gapfilldates,7,14)
gapfilldates <- as.Date(as.numeric(str_sub(gapfilldates,-3,-1))-1,
                    origin=paste0(str_sub(gapfilldates,1,4),"-01-01"))
order(gapfilldates)
names(gapfillsummer)
gapfillsummer <- gapfillsummer[[order(gapfilldates)]]
gapfilldates <- gapfilldates[order(gapfilldates)]
#7579/16#
#LSTindices <- rep(1:474,each=16)
#LSTindices <- LSTindices[1:7579]
gapfillsummeryears <- format(gapfilldates,"%Y")
gapfillsummer <- stackApply(gapfillsummer,gapfillsummeryears,mean)


#LSTyears <- format(LSTdates,"%Y")
#LST <- stackApply(LST,LSTyears,mean)
proj4string(LST)
gapfillsummer <- projectRaster(gapfillsummer,crs="+proj=longlat +datum=WGS84 +no_defs")
gapfillsummer <- crop(gapfillsummer,cropLongLat5c_small)
gapfillsummer <- mask(gapfillsummer,cropLongLat5c_small)
#LST_df <- data.frame(rasterToPoints(LST))
#colnames(LST_df)[3:ncol(LST_df)] <- unique(LSTmonths)
#LST_df <- LST_df[,c(-1,-2)]
#mean(LST_df$`2002-01`)
#LST_df <- apply(LST_df,2,mean,na.rm=T)
#LST_df <- as.data.frame(LST_df)
#LST_df
#LST_df$date <- paste0(unique(LSTmonths),"-01")
#LST_df$date <- as.Date(LST_df$date)
#LST_df$season <- time2season(LST_df$date,out.fmt="seasons")
#LST_df$month <- format(LST_df$date,"%B")
#colnames(LST_df)[1]<-"LST"
#LST_df$group = "5c"
#LST_df$LST <- LST_df$LST*0.02-273
#LST_monthly_df <- LST_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


gapfillsummer_zip <- crop(gapfillsummer,zipcrop)
gapfillsummer_zip <- mask(gapfillsummer_zip,cropLongLatzip_small)
plot(gapfillsummer_zip[[1]])
#LSTzip_df <- data.frame(rasterToPoints(LST_zip))
#colnames(LSTzip_df)[3:ncol(LSTzip_df)] <- unique(LSTmonths)
#LSTzip_df <- LSTzip_df[,c(-1,-2)]
#mean(LSTzip_df$`2002-01`)
#LSTzip_df <- apply(LSTzip_df,2,mean,na.rm=T)
#LSTzip_df <- as.data.frame(LSTzip_df)
#LSTzip_df
#LSTzip_df$date <- paste0(unique(LSTmonths),"-01")
#LSTzip_df$date <- as.Date(LSTzip_df$date)
#LSTzip_df$season <- time2season(LSTzip_df$date,out.fmt="seasons")
#LSTzip_df$month <- format(LST_df$date,"%B")
#colnames(LSTzip_df)[1]<-"LST"
#LSTzip_df$group = "ZIP"
#LSTzip_df$LST <- LSTzip_df$LST*0.02-273
#LSTzip_monthly_df <- LSTzip_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


values(gapfillsummer)<-values(gapfillsummer)/10
values(gapfillsummer_zip)<-values(gapfillsummer_zip)/10

values(gapfillsummer)<-values(gapfillsummer)*(9/5)+32
values(gapfillsummer_zip)<-values(gapfillsummer_zip)*(9/5)+32

target <- raster(resolution=res(gapfillsummer)/4,crs=proj4string(gapfillsummer),ext=extent(gapfillsummer))
res(target)
gapfillsummer <- resample(gapfillsummer,target)


plot(gapfillsummer[[5]])

names(gapfillsummer) <- format(seq(as.Date("2003-06-01"),as.Date("2019-06-01"),"years"),"%Y")


gapfillsummerlist <- list()

for(i in 1:nlayers(gapfillsummer)){
  
  
  gapfillsummerlist[[i]]<-
    tm_shape(gapfillsummer[[i]])+
    tm_raster(col=names(gapfillsummer[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(gapfillsummer),na.rm=T),max(values(gapfillsummer),na.rm=T),5),
                        max(values(gapfillsummer),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area",str_sub(names(gapfillsummer[[i]]),-4,-1),"Modeled Land Surface Temperature\nJune-August Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(gapfillsummer[[4]])
gapfillsummerlist[[4]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/June-August LST GIF+Individual Maps")
tmap_animation(gapfillsummerlist,"2003-2019_Modeled_SummerMonthsLST.gif",delay=150,width=800,height=800)


##########

target <- raster(resolution=res(gapfillsummer_zip)/10,crs=proj4string(gapfillsummer_zip),ext=extent(gapfillsummer_zip))
res(target)
gapfillsummer_zip <- resample(gapfillsummer_zip,target)


plot(gapfillsummer_zip[[5]])
#plot(LSTzipcoldestmonths[[5]])

names(gapfillsummer_zip) <- format(seq(as.Date("2003-06-01"),as.Date("2019-06-01"),"years"),"%Y")
#names(LSTzipcoldestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")

gapfillsummer_zip <- mask(gapfillsummer_zip,cropLongLatzip_small)
LSTzipcoldestmonths <- mask(LSTzipcoldestmonths,cropLongLatzip_small)

plot(gapfillsummer_zip[[5]])
plot(LSTzipcoldestmonths[[5]])
##

gapfillsummerlist <- list()

for(i in 1:nlayers(gapfillsummer_zip)){
  
  
  gapfillsummerlist[[i]]<-
    tm_shape(gapfillsummer_zip[[i]])+
    tm_raster(col=names(gapfillsummer_zip[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(gapfillsummer_zip),na.rm=T),max(values(gapfillsummer_zip),na.rm=T),4),
                        max(values(gapfillsummer_zip),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    tm_shape(interstateszip)+
    tm_lines(col="red", 
             #palette = "red",
             labels = "",
             lwd=2
    )+
    #tm_text("Label")+
    
    tm_shape(MARTAzip)+
    tm_lines(col = "MARTA",
             palette="blue",
             labels="",
             lwd=1.5)+
    tm_shape(Parkszip)+
    tm_polygons(col="Parks",
                palette = "green",
                labels=""
    )+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("ZIP Code",col="black",shadow=T,xmod = "xmod",size=1.4)+
    tm_scale_bar(position = c(0.03,0.02))+
    tm_compass(position = c(0.03,0.07))+
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0,0))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes",str_sub(names(gapfillsummer_zip[[i]]),-4,-1),"Modeled Temperature\nJune-August Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.outside=T,
              
              legend.outside.size=0.1
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33,
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(gapfillsummer_zip[[15]])
gapfillsummerlist[[15]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/June-August LST GIF+Individual Maps")
tmap_animation(hottestmonthlist,"2002-2022_Modeled_SummerMonthsLST_zip.gif",delay=150,width=800,height=800)
names(gapfillsummer_zip)
for(i in 1:length(gapfillsummerlist)){
  tmap_save(gapfillsummerlist[[i]],paste0(str_sub(names(gapfillsummer_zip)[[i]],-4,-1),"_Modeled_JuneAugust_LST_ZIP.png"),dpi=300)
}


####Gap Filling Winter LST####

setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Georgia_subset")
list.files()
c(grep('-01-',list.files(),fixed=T),
  grep('-02-',list.files(),fixed=T),
  grep('-12-',list.files(),fixed=T))
list.files()[grep('-01-',list.files(),fixed=T)]
list.files()[grep('-02-',list.files(),fixed=T)]
list.files()[grep('-12-',list.files(),fixed=T)]
gapfillwinter <- stack(list.files()[c(grep('-01-',list.files(),fixed=T),
                                      grep('-02-',list.files(),fixed=T),
                                      grep('-12-',list.files(),fixed=T))])
gapfilldates <- names(gapfillwinter)
gapfilldates <- str_sub(gapfilldates,7,14)
gapfilldates <- as.Date(as.numeric(str_sub(gapfilldates,-3,-1))-1,
                        origin=paste0(str_sub(gapfilldates,1,4),"-01-01"))
order(gapfilldates)
names(gapfillwinter)
gapfillwinter <- gapfillwinter[[order(gapfilldates)]]
gapfilldates <- gapfilldates[order(gapfilldates)]
#7579/16#
#LSTindices <- rep(1:474,each=16)
#LSTindices <- LSTindices[1:7579]
gapfillwinteryears <- format(gapfilldates,"%Y")
gapfillwinter <- stackApply(gapfillwinter,gapfillwinteryears,mean)


#LSTyears <- format(LSTdates,"%Y")
#LST <- stackApply(LST,LSTyears,mean)
proj4string(LST)
gapfillwinter <- projectRaster(gapfillwinter,crs="+proj=longlat +datum=WGS84 +no_defs")
gapfillwinter <- crop(gapfillwinter,cropLongLat5c_small)
gapfillwinter <- mask(gapfillwinter,cropLongLat5c_small)
#LST_df <- data.frame(rasterToPoints(LST))
#colnames(LST_df)[3:ncol(LST_df)] <- unique(LSTmonths)
#LST_df <- LST_df[,c(-1,-2)]
#mean(LST_df$`2002-01`)
#LST_df <- apply(LST_df,2,mean,na.rm=T)
#LST_df <- as.data.frame(LST_df)
#LST_df
#LST_df$date <- paste0(unique(LSTmonths),"-01")
#LST_df$date <- as.Date(LST_df$date)
#LST_df$season <- time2season(LST_df$date,out.fmt="seasons")
#LST_df$month <- format(LST_df$date,"%B")
#colnames(LST_df)[1]<-"LST"
#LST_df$group = "5c"
#LST_df$LST <- LST_df$LST*0.02-273
#LST_monthly_df <- LST_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


gapfillwinter_zip <- crop(gapfillwinter,zipcrop)
gapfillwinter_zip <- mask(gapfillwinter_zip,cropLongLatzip_small)
plot(gapfillwinter_zip[[1]])
#LSTzip_df <- data.frame(rasterToPoints(LST_zip))
#colnames(LSTzip_df)[3:ncol(LSTzip_df)] <- unique(LSTmonths)
#LSTzip_df <- LSTzip_df[,c(-1,-2)]
#mean(LSTzip_df$`2002-01`)
#LSTzip_df <- apply(LSTzip_df,2,mean,na.rm=T)
#LSTzip_df <- as.data.frame(LSTzip_df)
#LSTzip_df
#LSTzip_df$date <- paste0(unique(LSTmonths),"-01")
#LSTzip_df$date <- as.Date(LSTzip_df$date)
#LSTzip_df$season <- time2season(LSTzip_df$date,out.fmt="seasons")
#LSTzip_df$month <- format(LST_df$date,"%B")
#colnames(LSTzip_df)[1]<-"LST"
#LSTzip_df$group = "ZIP"
#LSTzip_df$LST <- LSTzip_df$LST*0.02-273
#LSTzip_monthly_df <- LSTzip_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


values(gapfillwinter)<-values(gapfillwinter)/10
values(gapfillwinter_zip)<-values(gapfillwinter_zip)/10

values(gapfillwinter)<-values(gapfillwinter)*(9/5)+32
values(gapfillwinter_zip)<-values(gapfillwinter_zip)*(9/5)+32

target <- raster(resolution=res(gapfillwinter)/4,crs=proj4string(gapfillwinter),ext=extent(gapfillwinter))
res(target)
gapfillwinter <- resample(gapfillwinter,target)


plot(gapfillwinter[[5]])

names(gapfillwinter) <- format(seq(as.Date("2003-06-01"),as.Date("2020-06-01"),"years"),"%Y")


gapfillwinterlist <- list()

for(i in 1:nlayers(gapfillwinter)){
  
  
  gapfillwinterlist[[i]]<-
    tm_shape(gapfillwinter[[i]])+
    tm_raster(col=names(gapfillwinter[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(gapfillwinter),na.rm=T),max(values(gapfillwinter),na.rm=T),5),
                        max(values(gapfillwinter),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    tm_shape(five_c_motorways)+
    tm_lines(col = "Highways", 
             palette = "blue",
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
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area",str_sub(names(gapfillwinter[[i]]),-4,-1),"Modeled Land Surface Temperature\nJanuary-February, December Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(gapfillwinter[[4]])
gapfillwinterlist[[1]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/January-February, December LST GIF+Individual Maps")
tmap_animation(gapfillwinterlist,"2003-2020_Modeled_WinterMonthsLST.gif",delay=150,width=800,height=800)


##########

target <- raster(resolution=res(gapfillwinter_zip)/10,crs=proj4string(gapfillwinter_zip),ext=extent(gapfillwinter_zip))
res(target)
gapfillwinter_zip <- resample(gapfillwinter_zip,target)


plot(gapfillwinter_zip[[5]])
#plot(LSTzipcoldestmonths[[5]])

names(gapfillwinter_zip) <- format(seq(as.Date("2003-06-01"),as.Date("2020-06-01"),"years"),"%Y")
#names(LSTzipcoldestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")

gapfillwinter_zip <- mask(gapfillwinter_zip,cropLongLatzip_small)
#LSTzipcoldestmonths <- mask(LSTzipcoldestmonths,cropLongLatzip_small)

plot(gapfillwinter_zip[[5]])
plot(LSTzipcoldestmonths[[5]])
##

gapfillwinterlist <- list()

for(i in 1:nlayers(gapfillwinter_zip)){
  
  
  gapfillwinterlist[[i]]<-
    tm_shape(gapfillwinter_zip[[i]])+
    tm_raster(col=names(gapfillwinter_zip[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(gapfillwinter_zip),na.rm=T),max(values(gapfillwinter_zip),na.rm=T),4),
                        max(values(gapfillwinter_zip),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    tm_shape(interstateszip)+
    tm_lines(col="red", 
             #palette = "red",
             labels = "",
             lwd=2
    )+
    #tm_text("Label")+
    
    tm_shape(MARTAzip)+
    tm_lines(col = "MARTA",
             palette="blue",
             labels="",
             lwd=1.5)+
    tm_shape(Parkszip)+
    tm_polygons(col="Parks",
                palette = "green",
                labels=""
    )+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("ZIP Code",col="black",shadow=T,xmod = "xmod",size=1.4)+
    tm_scale_bar(position = c(0.03,0.02))+
    tm_compass(position = c(0.03,0.07))+
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0,0))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes",str_sub(names(gapfillwinter_zip[[i]]),-4,-1),"Modeled Temperature\nJanuary-February, December Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.outside=T,
              
              legend.outside.size=0.1
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33,
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(gapfillwinter_zip[[15]])
gapfillwinterlist[[15]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results/January-February, December LST GIF+Individual Maps")
tmap_animation(gapfillwinterlist,"2003-2020_Modeled_WinterMonthsLST_zip.gif",delay=150,width=800,height=800)
names(gapfillwinter_zip)
for(i in 1:length(gapfillwinterlist)){
  tmap_save(gapfillwinterlist[[i]],paste0(str_sub(names(gapfillwinter_zip)[[i]],-4,-1),"_Modeled_JanFebDec_LST_ZIP.png"),dpi=300)
}



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

####

setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/Masked AOD")
list.files()
AOD <- stack(list.files())

AODdates <- as.Date(as.numeric(substr(list.files(),11,13))-1,
                    origin = as.Date(paste0(substr(list.files(),7,10),"-01-01")))

7579/16
AODmonths <- format(AODdates,"%Y-%m")
AOD <- stackApply(AOD,AODmonths,mean)


AOD <- crop(AOD,cropLongLat5c_small)
AOD <- mask(AOD,cropLongLat5c_small)
LST_df <- data.frame(rasterToPoints(LST))
colnames(LST_df)[3:ncol(LST_df)] <- unique(LSTmonths)
LST_df <- LST_df[,c(-1,-2)]
mean(LST_df$`2002-01`)
LST_df <- apply(LST_df,2,mean,na.rm=T)
LST_df <- as.data.frame(LST_df)
LST_df
LST_df$date <- paste0(unique(LSTmonths),"-01")
LST_df$date <- as.Date(LST_df$date)
LST_df$season <- time2season(LST_df$date,out.fmt="seasons")
LST_df$month <- format(LST_df$date,"%B")
colnames(LST_df)[1]<-"LST"
LST_df$group = "5c"
LST_df$LST <- LST_df$LST*0.02-273
LST_monthly_df <- LST_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


AOD_zip <- crop(AOD,cropLongLatzip_small)
AOD_zip <- mask(AOD_zip,cropLongLatzip_small)
LSTzip_df <- data.frame(rasterToPoints(LST_zip))
colnames(LSTzip_df)[3:ncol(LSTzip_df)] <- unique(LSTmonths)
LSTzip_df <- LSTzip_df[,c(-1,-2)]
mean(LSTzip_df$`2002-01`)
LSTzip_df <- apply(LSTzip_df,2,mean,na.rm=T)
LSTzip_df <- as.data.frame(LSTzip_df)
LSTzip_df
LSTzip_df$date <- paste0(unique(LSTmonths),"-01")
LSTzip_df$date <- as.Date(LSTzip_df$date)
LSTzip_df$season <- time2season(LSTzip_df$date,out.fmt="seasons")
LSTzip_df$month <- format(LST_df$date,"%B")
colnames(LSTzip_df)[1]<-"LST"
LSTzip_df$group = "ZIP"
LSTzip_df$LST <- LSTzip_df$LST*0.02-273
LSTzip_monthly_df <- LSTzip_df %>% group_by(month) %>% summarise(mean_temp = mean(LST),.groups="drop") %>% as.data.frame()


AODhottestmonths <- subset(AOD,c(grep('.06',names(AOD),fixed=T),
                                 grep('.07',names(AOD),fixed=T),
                                 grep('.08',names(AOD),fixed=T)))
hottestmonths <- names(AODhottestmonths)
hottestmonths <- paste0(str_sub(hottestmonths,7,10),"-",str_sub(hottestmonths,12,13),"-01")
hottestmonths <- as.Date.character(hottestmonths)
AODhottestmonths <- AODhottestmonths[[order(hottestmonths)]]
names(AODhottestmonths)


AODcoldestmonths <- subset(AOD,c(grep('.12',names(AOD),fixed=T),
                                 grep('.01',names(AOD),fixed=T),
                                 grep('.02',names(AOD),fixed=T)))
coldestmonths <- names(AODcoldestmonths)
coldestmonths <- paste0(str_sub(coldestmonths,7,10),"-",str_sub(coldestmonths,12,13),"-01")
coldestmonths <- as.Date.character(coldestmonths)
AODcoldestmonths <- AODcoldestmonths[[order(coldestmonths)]]
names(AODcoldestmonths)

hottestindices <- rep(1:21,each=3)
coldestindices <- rep(1:21,each=3)

AODhottestmonths <- stackApply(AODhottestmonths,hottestindices,mean)
AODcoldestmonths <- stackApply(AODcoldestmonths,coldestindices,mean)

target <- raster(resolution=res(AODhottestmonths)/5,crs=proj4string(AODhottestmonths),ext=extent(AODhottestmonths))
res(target)
AODhottestmonths <- resample(AODhottestmonths,target)
AODcoldestmonths <- resample(AODcoldestmonths,target)

plot(AODhottestmonths[[13]])
plot(AODcoldestmonths[[5]])

names(AODhottestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")
names(AODcoldestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")

hottestmonthlist <- list()

for(i in 1:nlayers(AODhottestmonths)){
  
  
  hottestmonthlist[[i]]<-
    tm_shape(AODhottestmonths[[i]])+
    tm_raster(col=names(AODhottestmonths[[i]]),palette = rev(airpolcolorvector),
              breaks =c(seq(min(values(AODhottestmonths),na.rm=T),max(values(AODhottestmonths),na.rm=T),0.1),
                        max(values(AODhottestmonths),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air\nPollution",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MCD19A2 v6 1km AOD",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area",str_sub(names(AODhottestmonths[[i]]),-4,-1),"Air Pollution\nJune-August"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(LSThottestmonths[[4]])
hottestmonthlist[[4]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(hottestmonthlist,"2002-2022_SummerMonthsAOD.gif",delay=150,width=800,height=800)


coldestmonthlist <- list()

for(i in 1:nlayers(AODcoldestmonths)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(AODcoldestmonths[[i]])+
    tm_raster(col=names(AODcoldestmonths[[i]]),palette = rev(airpolcolorvector),
              breaks =c(seq(min(values(AODcoldestmonths),na.rm=T),max(values(AODcoldestmonths),na.rm=T),0.015),
                        max(values(AODcoldestmonths),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air\nPollution",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MCD19A2 v6 1km AOD",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area",str_sub(names(AODcoldestmonths[[i]]),-4,-1),"Air Pollution\nJanuary-February, December"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(AODcoldestmonths[[4]])
names(coldestmonthlist)
coldestmonthlist[[13]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(coldestmonthlist,"2002-2022_WinterMonthsAOD.gif",delay=150,width=800,height=800)


#########

AODziphottestmonths <- subset(AOD_zip,c(grep('.06',names(AOD_zip),fixed=T),
                                 grep('.07',names(AOD_zip),fixed=T),
                                 grep('.08',names(AOD_zip),fixed=T)))
hottestmonths <- names(AODziphottestmonths)
hottestmonths <- paste0(str_sub(hottestmonths,7,10),"-",str_sub(hottestmonths,12,13),"-01")
hottestmonths <- as.Date.character(hottestmonths)
AODziphottestmonths <- AODziphottestmonths[[order(hottestmonths)]]
names(AODziphottestmonths)


AODzipcoldestmonths <- subset(AOD_zip,c(grep('.12',names(AOD_zip),fixed=T),
                                 grep('.01',names(AOD_zip),fixed=T),
                                 grep('.02',names(AOD_zip),fixed=T)))
coldestmonths <- names(AODzipcoldestmonths)
coldestmonths <- paste0(str_sub(coldestmonths,7,10),"-",str_sub(coldestmonths,12,13),"-01")
coldestmonths <- as.Date.character(coldestmonths)
AODzipcoldestmonths <- AODzipcoldestmonths[[order(coldestmonths)]]
names(AODzipcoldestmonths)

hottestindices <- rep(1:21,each=3)
coldestindices <- rep(1:21,each=3)

AODziphottestmonths <- stackApply(AODziphottestmonths,hottestindices,mean)
AODzipcoldestmonths <- stackApply(AODzipcoldestmonths,coldestindices,mean)

target <- raster(resolution=res(AODziphottestmonths)/10,crs=proj4string(AODziphottestmonths),ext=extent(AOziphottestmonths))
res(target)
AODziphottestmonths <- resample(AODziphottestmonths,target)
AODzipcoldestmonths <- resample(AODzipcoldestmonths,target)

plot(AODziphottestmonths[[13]])
plot(AODzipcoldestmonths[[5]])

AODziphottestmonths <- mask(AODziphottestmonths,cropLongLatzip_small)
AODzipcoldestmonths <- mask(AODzipcoldestmonths,cropLongLatzip_small)

names(AODziphottestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")
names(AODzipcoldestmonths) <- format(seq(as.Date("2002-06-01"),as.Date("2022-06-01"),"years"),"%Y")

###


hottestmonthlist <- list()

for(i in 1:nlayers(AODziphottestmonths)){
  
  
  hottestmonthlist[[i]]<-
    tm_shape(AODziphottestmonths[[i]])+
      tm_raster(col=names(AODziphottestmonths[[i]]),palette = rev(airpolcolorvector),
              breaks =c(seq(min(values(AODziphottestmonths),na.rm=T),max(values(AODziphottestmonths),na.rm=T),0.1),
                        max(values(AODziphottestmonths),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air\nPollution",legend.reverse=T)+
    tm_shape(zip_motorways)+
    tm_lines(col = "Highways", 
             palette = "grey",
             labels = "",
             lwd=2
    )+
    
    tm_shape(MARTAzip)+
    tm_lines(col = "MARTA",
             palette="blue",
             labels="",
             lwd=1.5)+
    tm_shape(Parkszip)+
    tm_polygons(col="Parks",
                palette = "green",
                labels=""
    )+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("GEOID",col="black",shadow=T,xmod = "xmod")+
    tm_scale_bar(position = c(0.03,0.02))+
    tm_compass(position = c(0.03,0.07))+
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0,0))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes",str_sub(names(NDVIzip[[i]]),-4,-1),"Air Pollution\nJune-August"),
              main.title.size=1.4,
              main.title.position="center",
              legend.outside=T,
              
              legend.outside.size=0.1
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33,
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(AODziphottestmonths[[11]])
hottestmonthlist[[11]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(hottestmonthlist,"2002-2022_SummerMonthsAOD_zip.gif",delay=150,width=800,height=800)


coldestmonthlist <- list()

for(i in 1:nlayers(AODzipcoldestmonths)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(AODzipcoldestmonths[[i]])+
    tm_raster(col=names(AODzipcoldestmonths[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(AODzipcoldestmonths),na.rm=T),max(values(AODzipcoldestmonths),na.rm=T),2),
                        max(values(AODzipcoldestmonths),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °C",legend.reverse=T)+
    tm_shape(zip_motorways)+
    tm_lines(col = "Highways", 
             palette = "grey",
             labels = "",
             lwd=2
    )+
    
    tm_shape(MARTAzip)+
    tm_lines(col = "MARTA",
             palette="blue",
             labels="",
             lwd=1.5)+
    tm_shape(Parkszip)+
    tm_polygons(col="Parks",
                palette = "green",
                labels=""
    )+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("GEOID",col="black",shadow=T,xmod = "xmod")+
    tm_scale_bar(position = c(0.03,0.02))+
    tm_compass(position = c(0.03,0.07))+
    tm_credits("Data Source: NASA MOD11A1v6 1km LST",
               position = c(0,0))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes",str_sub(names(NDVIzip[[i]]),-4,-1),"Air Pollution\nJanuary-February, December"),
              main.title.size=1.4,
              main.title.position="center",
              legend.outside=T,
              
              legend.outside.size=0.1
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33,
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(AODzipcoldestmonths[[4]])
names(AODzipcoldestmonths)
coldestmonthlist[[13]]
setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(coldestmonthlist,"2002-2022_WinterMonthsAOD_zip.gif",delay=150,width=800,height=800)


