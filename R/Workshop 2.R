#####Packages######
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
library(gifski)
#library(gstat)
#library(sp)


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
#Set to v061 as of 09/19/2023: v006 deprecated
#multiply by 0.02 for real value(s) in Kelvin
#-273 for values in Celsius
#Yes R Rasterstack
#Yes Reprocess
#Projection = set by user: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

####Year and Season Function#####
year_and_season <- function(Date,Season){
  month <- lubridate::month(Date,label=TRUE)
  year <- lubridate::year(Date)
  year <- ifelse(month %in% c("Jan","Feb"),year-1,year)
  season <- paste(year,Season)
  return(season)
}

#####Get shapefiles and boundaries#####
crop_parameters_5c <- st_read(here::here("Data","ATL_shps_for_April","five_counties.shp"))
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = 4269)
cropLongLat_5c <- st_make_valid(cropLongLat_5c)
cropLongLat5c_small <- cropLongLat_5c[,9]
ymod <- c(0,0,-4,0,0)
xmod <- c(0,0,-3,0,0)
cropLongLat_5c <- cbind(cropLongLat_5c,ymod, xmod)
rm(crop_parameters_5c)


crop_parameters_zip <- st_read(here::here("Data","ATL_shps_for_April","expandedzip.shp"))
cropLongLat_zip <- st_transform(crop_parameters_zip,crs = 4269)
xmod <- c(1,0,0,1,-0.4,1)
cropLongLat_zip <- cbind(cropLongLat_zip,xmod)
cropLongLatzip_small <- cropLongLat_zip[,3]
colnames(cropLongLat_zip)[1]<-"ZIP Code"
rm(crop_parameters_zip)

cropLongLat_zip$Unit <- "Select\nZIP Codes"

zipcrop <- st_bbox(cropLongLat_zip)
zipcrop[2] <- 33.615
zipcrop[3] <- -84.34
zipcrop

#####Highways + Misc GIS######

# available_features()
# 
# available_tags(feature = "highway")
# 
# class(cropLongLat5c_small)
# 
# five_c_bb <- st_bbox(cropLongLat5c_small)
# five_c_major <- osmdata_sf(add_osm_feature(opq(bbox = five_c_bb,timeout = 100), 
#                                            key = "highway", value = c("motorway","primary","secondary")))
# 
# five_c_highways <- five_c_major$osm_lines
# st_crs(five_c_highways)
# five_c_highways <- st_transform(five_c_highways, crs=4269)
# #st_write(five_c_highways,here::here("Data","ATL_shps_for_April","5c_highways.gpkg"))
# 
# five_c_highways <- st_intersection(five_c_highways, cropLongLat5c_small)
zip_highways <- st_crop(five_c_highways, zipcrop)

five_c_highways <- st_make_valid(five_c_highways)
zip_highways <- st_make_valid(zip_highways)

five_c_highways <- five_c_highways[,c(1,2,73,230,231)]
st_crs(five_c_highways)

st_write(five_c_highways,here::here("Data","ATL_shps_for_April","5c_highways.gpkg"))



####
five_c_highways<-st_read(here::here("Data","ATL_shps_for_April","5c_highways.gpkg"))
zip_highways <- st_crop(five_c_highways, zipcrop)

tmap_options(check.and.fix = TRUE)

#five_c_highways$Highways <- "Highway"
five_c_motorways <- subset(five_c_highways,`Highway.Type`=="motorway")
plot(st_geometry(five_c_motorways))

#zip_highways$Highways <- "Highway"
zip_motorways <- subset(zip_highways,`Highway.Type` == "motorway")

plot(st_geometry(zip_motorways))

interstates <- st_read(here::here("Data","ATL_shps_for_April","Expressways_Georgia.shp"))
interstates <- st_intersection(interstates, cropLongLat5c_small)
interstateszip <- st_crop(interstates, cropLongLat_zip)

interstates <- st_make_valid(interstates)
interstateszip <- st_make_valid(interstateszip)

interstateszip$ROAD_NAME <- str_sub(interstateszip$ROAD_NAME,1,-3)
interstateszip$Highways <- "Highways"
#interstateszip <- aggregate(interstateszip,interstateszip$ROAD_NAME,mean)

tm_shape(interstateszip)+
  tm_lines()+
  tm_text("Label")
###Redlining
redlining <- st_read(here::here("Data","ATL_shps_for_April","Atlanta_Redlining.geojson"))
plot(st_geometry(redlining))
st_bbox(redlining)

st_bbox(st_union(redlining,cropLongLat_zip))

tm_shape(cropLongLat_zip)+
  tm_borders()+
tm_shape(redlining)+
  tm_polygons(col = "holc_grade",
              palette = c("green","blue","yellow","red"),
              labels = c("A \"Best\"","B \"Still Desirable\"","C \"Definitely Declining\"","D \"Hazardous\""),
              title = "HOLC Grade")

st_crs(redlining)
st_crs(cropLongLat_zip)

tm_shape(redlining, bbox = st_bbox(st_union(redlining,cropLongLat_zip)))+
  tm_polygons(col = "holc_grade",
              palette = c("green","blue","yellow","red"),
              labels = c("A \"Best\"","B \"Still Desirable\"","C \"Definitely Declining\"","D \"Hazardous\""),
              title = "HOLC Grade")+
tm_shape(cropLongLat_zip)+
  tm_text("ZIP Code")+
  tm_borders(lwd=2,col="black")

###MARTA###
MARTA <- st_read(here::here("Data","ATL_shps_for_April","MARTA_Routes.geojson"))
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
Parks <- st_read(here::here("Data","ATL_shps_for_April","test2.shp"))
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
  tm_lines()

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

zip_parks4 <- st_read(here::here("Data","ATL_shps_for_April","Atlanta_Parks.shp"))
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
zip_beltline2[1,2] <- " "

#available_tags(feature= "building")
#zip_stations <- osmdata_sf(add_osm_feature(opq(zipcrop), #Beltline
 #                                          key = "building", value = c("train_station")))
#zip_stations2 <- zip_stations$osm_polygons

zip_stations <- st_read(here::here("Data","ATL_shps_for_April","MARTA_Stops.shp"))
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
zip_pois <- zip_pois[-8,c(1,2,144)]
rownames(zip_pois)<-NULL
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
rownames(zip_pois)<-NULL
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
zip_pois[7,4] <- -1
zip_pois[7,5] <- 0
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
#setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")
setwd(here::here("Data","MOD13A2v6 500m NDVI 2002_01_01-2023_03_23","NDVI"))

NDVI <- stack(list.files())
NDVI
res(NDVI)
names(NDVI)
NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
                     origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
#NDVIseason <- hydroTSM::time2season(NDVIdates,out.fmt="seasons")

NDVInames <- year(NDVIdates)
names(NDVI) <- NDVInames

values(NDVI) <- values(NDVI)*0.0001

NDVI <- dropLayer(NDVI,c(grep(as.character(2023),names(NDVI),fixed=T)))
NDVIindices <- NDVInames[1:482]

# NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
#                      origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
# NDVI <- NDVI[[order(NDVIdates)]]
# NDVIdates <- as.Date(as.numeric(str_sub(names(NDVI),-3,-1))-1,
#                      origin=paste0(str_sub(names(NDVI),-8,-5),"-01-01"))
#NDVIyears <- format(NDVIdates,"%Y")
NDVI <- stackApply(NDVI,NDVIindices,mean)
NDVI <- crop(NDVI, cropLongLat5c_small)
NDVI <- mask(NDVI, cropLongLat5c_small)

plot(NDVI[[1]])

#NDVI_df <- data.frame(rasterToPoints(NDVI))
#colnames(NDVI_df)[3:ncol(NDVI_df)] <- unique(NDVIyears)
#NDVI_df <- NDVI_df[,c(-1,-2)]
#mean(NDVI_df$`2002`)
#NDVI_df <- apply(NDVI_df,2,mean,na.rm=T)
#NDVI_df <- as.data.frame(NDVI_df)
#NDVI_df$date <- rownames(NDVI_df)
#NDVI_df$date <- as.Date(paste0(NDVI_df$date,"-01-01")   # Convert Julian day to date
#)

#NDVI_df$season <- time2season(NDVI_df$date,out.fmt="seasons")
#colnames(NDVI_df)[1]<-"NDVI"
#NDVI_df$group = "5c"
plot(st_geometry(cropLongLat_zip))

NDVI_zip <- crop(NDVI, cropLongLat_zip)
NDVI_zip <- mask(NDVI_zip, cropLongLat_zip)

plot(NDVI_zip[[1]])
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
plot(NDVI[[1]])
target <- raster(resolution=res(NDVI)/2,crs=proj4string(NDVI),ext=extent(NDVI))
res(target)
NDVI <- resample(NDVI,target)
plot(NDVI[[1]])

NDVI <- mask(NDVI, cropLongLat5c_small)


plot(NDVI_zip[[1]])
target <- raster(resolution=res(NDVI_zip)/5,crs=proj4string(NDVI_zip),ext=extent(NDVI_zip))
res(target)
NDVI_zip <- resample(NDVI_zip,target)
plot(NDVI_zip[[1]])

NDVI_zip <- mask(NDVI_zip, cropLongLat_zip)

# 
# targetzip <- raster(resolution=res(NDVIzip)/5,crs=proj4string(NDVIzip),ext=extent(NDVIzip))
# res(targetzip)
# NDVIzip <- resample(NDVIzip,targetzip)
# NDVIzip <- mask(NDVIzip, cropLongLat_zip)


NDVI_list <- list()


for(i in 1:nlayers(NDVI)){
  
  
  NDVI_list[[i]]<-
    tm_shape(NDVI[[i]])+
    tm_raster(col=names(NDVI[[i]]),palette = NDVIcolorvector,
              breaks =c(seq(min(values(NDVI),na.rm=T),max(values(NDVI),na.rm=T),0.2),
                        max(values(NDVI),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Greenspace",legend.reverse=T)+
    tm_shape(cropLongLat_5c)+
      tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod", size=2)+
      tm_borders(col="black",lwd=3)+
   
    tm_shape(five_c_motorways)+
      tm_lines(col = "Highways", 
             palette = "grey",
             labels = "",
             lwd=2,
    )+
    tm_shape(cropLongLat_zip)+
      tm_borders(col="black",lwd=2,lty="solid")+
      tm_fill(col="Unit", palette = "lightblue", alpha= 0.5, title = "Selected\nZIP Codes",labels = "")+
    
   
    tm_scale_bar(position = c(0.05,0.01))+
    tm_compass(position = c(0.03,0.07))+
   # tm_credits("Data Source: NASA MOD13A2v6 500m NDVI",
             #  position = c(0.5,0))+
    tm_layout(frame=F,
             # main.title=paste("Study Area Greenspace\n", str_sub(names(NDVI[[i]]),7,10), "Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(NDVI[[4]])
NDVI_list[[4]]
#setwd("C:/Users/EJLI4/OneDrive - Emory University/3_Ethan_Atlanta_Project/Results")
tmap_animation(NDVI_list,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_NDVI.gif"),delay=150,width=800,height=800)

for(i in 1:length(NDVI_list)){
  tmap_save(NDVI_list[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(NDVI[[i]]),7,10)," NDVI.png"))
}


########

NDVIzip_list <- list()
for(i in 1:nlayers(NDVI_zip)){
  
  
  NDVIzip_list[[i]]<-
    tm_shape(NDVI_zip[[i]])+
    tm_raster(col=names(NDVI_zip[[i]]),palette = NDVIcolorvector,
              breaks =c(seq(min(values(NDVI),na.rm=T),max(values(NDVI),na.rm=T),0.1),
                        max(values(NDVI),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Greenspace",legend.reverse=T)+
    tm_shape(interstateszip)+
    tm_lines(col = "Highways", 
             palette = "blue",
             labels = "",
             lwd=3
    )+
    
    #tm_shape(MARTAzip)+
    # tm_lines(col = "MARTA",
    #        palette="blue",
    #         labels="",
    #         lwd=1.5)+
    tm_shape(zip_beltline2)+
    tm_lines(col="Beltline",lwd=2,palette="purple",title.col = "Beltline Trail")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("ZIP Code",col="black",shadow=T,xmod ="xmod",size=1.2)+
    tm_shape(Parkszip)+
    tm_lines(alpha = 1, col = "Parks",palette="green",lwd=2)+
    
    tm_shape(zip_pois_HJ)+
    tm_borders()+
    tm_dots(size = 0.1,
            col = "red",
            shape = 6,
            legend.show=F
    )+
    tm_text("name",size=0.5,col="black",ymod=0.4)+
    
    tm_shape(zip_pois[c(-19,-12,-1),])+
    tm_dots(size = 0.1,
            col = "POIs",
            title = "Points of Interest",
            palette = "red",
            shape = 6,
    )+
    tm_text("Points of\nInterest",size=0.5,col="black",shadow=T,xmod="xmod",ymod="ymod")+
    
    #
    tm_scale_bar(position = c(0.03,0.17))+
    tm_compass(position = c(0,0.24))+
    tm_credits("Data Source: NASA\nMOD13A2v6 500m NDVI",
               position = c(0.03,0.1))+
    tm_layout(frame=F,
              main.title=paste0("Select ZIP Codes Greenspace\n", str_sub(names(NDVI_zip[[i]]),7,10)," Average"),
              main.title.size=1.4,
              main.title.position="left",
              legend.outside=T,
              legend.text.size=0.8,
              legend.outside.size=0.2,
              outer.margins = c(0,0,0,0)
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
}
plot(NDVI_zip[[4]])
NDVIzip_list[[4]]

tmap_animation(NDVIzip_list,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_NDVI_ZIP.gif"),delay=150,width=800,height=800)

for(i in 1:length(NDVIzip_list)){
  tmap_save(NDVIzip_list[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(NDVI_zip[[i]]),7,10)," NDVI ZIP.png"))
}


####LST####
setwd(here::here("Data","MOD11A1v6 1km LST 2002_01_01 - 2023_03_23","LST_Day_1km"))
LST <- stack(list.files())
LSTdates <- as.Date(as.numeric(str_sub(names(LST),-3,-1))-1,
                    origin=paste0(str_sub(names(LST),-8,-5),"-01-01"))
LSTseasons <- hydroTSM::time2season(LSTdates,out.fmt="seasons")
LSTyearseason <- year_and_season(LSTdates,LSTseasons)
#7579/16#
#LSTindices <- rep(1:474,each=16)
#LSTindices <- LSTindices[1:7579]
names(LST) <- LSTdates
LST_seasonal <- stackApply(LST,LSTyearseason,mean)
names(LST_seasonal)
values(LST_seasonal) <- values(LST_seasonal)*0.02-273
values(LST_seasonal) <- values(LST_seasonal)*(9/5)+32

LST_seasonal <- crop(LST_seasonal,cropLongLat5c_small)
LST_seasonal <- mask(LST_seasonal, cropLongLat5c_small)


LST_seasonal_zip <- crop(LST_seasonal,cropLongLat_zip)
LST_seasonal_zip <- mask(LST_seasonal_zip,cropLongLat_zip)
names(LST_seasonal_zip)
plot(LST_seasonal_zip[[1]])

LST_seasonal_summer <- subset(LST_seasonal,c(grep('summer',names(LST_seasonal),fixed=T)))
LST_seasonal_winter <- subset(LST_seasonal,c(grep('winter',names(LST_seasonal),fixed=T)))
plot(LST_seasonal_summer[[1]])
plot(LST_seasonal_winter[[1]])

LST_seasonal_zip_summer <- subset(LST_seasonal_zip,c(grep('summer',names(LST_seasonal_zip),fixed=T)))
LST_seasonal_zip_winter <- subset(LST_seasonal_zip,c(grep('winter',names(LST_seasonal_zip),fixed=T)))
plot(LST_seasonal_zip_summer[[1]])
plot(LST_seasonal_zip_winter[[1]])


target <- raster(resolution=res(LST_seasonal)/4,crs=proj4string(LST_seasonal),ext=extent(LST_seasonal))
res(target)
LST_seasonal_summer <- resample(LST_seasonal_summer,target)
LST_seasonal_winter <- resample(LST_seasonal_winter,target)
LST_seasonal_summer <- mask(LST_seasonal_summer, cropLongLat5c_small)
LST_seasonal_winter <- mask(LST_seasonal_winter, cropLongLat5c_small)

target <- raster(resolution=res(LST_seasonal_zip)/10,crs=proj4string(LST_seasonal_zip),ext=extent(LST_seasonal_zip))
res(target)
LST_seasonal_zip_summer <- resample(LST_seasonal_zip_summer,target)
LST_seasonal_zip_winter <- resample(LST_seasonal_zip_winter,target)
LST_seasonal_zip_summer <- mask(LST_seasonal_zip_summer, cropLongLat_zip)
LST_seasonal_zip_winter <- mask(LST_seasonal_zip_winter, cropLongLat_zip)


hottestmonthlist <- list()

for(i in 1:nlayers(LST_seasonal_summer)){


hottestmonthlist[[i]]<-
  tm_shape(LST_seasonal_summer[[i]])+
  tm_raster(col=names(LST_seasonal_summer[[i]]),palette = rev(tempcolorvector),
            breaks =c(seq(min(values(LST_seasonal_summer),na.rm=T),max(values(LST_seasonal_summer),na.rm=T),6),
                      max(values(LST_seasonal_summer),na.rm=T)),
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
            main.title=paste("Study Area Land Temperature\nJune-August", str_sub(names(LST_seasonal_summer[[i]]),7,10),"Average"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))

}
plot(LST_seasonal_summer[[4]])
hottestmonthlist[[4]]

tmap_animation(hottestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_SummerMonthsLST.gif"),delay=150,width=800,height=800)

for(i in 1:length(hottestmonthlist)){
  tmap_save(hottestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(LST_seasonal_summer[[i]]),7,10)," Summer LST.png"))
}


coldestmonthlist <- list()

for(i in 1:nlayers(LST_seasonal_winter)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(LST_seasonal_winter[[i]])+
    tm_raster(col=names(LST_seasonal_winter[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(LST_seasonal_winter),na.rm=T),max(values(LST_seasonal_winter),na.rm=T),6),
                        max(values(LST_seasonal_winter),na.rm=T)),
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
              main.title=paste("Study Area Land Temperature\nDecember", str_sub(names(LST_seasonal_winter[[i]]),7,10),"- Februrary", as.numeric(str_sub(names(LST_seasonal_winter[[i]]),7,10))+1,"Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  
}
plot(LST_seasonal_winter[[4]])
coldestmonthlist[[4]]
names(LST_seasonal_winter)

tmap_animation(coldestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_WinterMonthsLST.gif"),delay=150,width=800,height=800)

for(i in 1:length(coldestmonthlist)){
  tmap_save(coldestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(LST_seasonal_winter[[i]]),7,10)," Winter LST.png"))
}

##########
hottestmonthlist <- list()

for(i in 1:nlayers(LST_seasonal_zip_summer)){
  
  
  hottestmonthlist[[i]]<-
    tm_shape(LST_seasonal_zip_summer[[i]])+
    tm_raster(col=names(LST_seasonal_zip_summer[[i]]),palette = rev(tempcolorvector),
              breaks =round(
                c(seq(min(values(LST_seasonal_summer),na.rm=T),max(values(LST_seasonal_summer),na.rm=T),5),
                  max(values(LST_seasonal_summer),na.rm=T)),
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
      tm_lines(col="Beltline",lwd=2,palette="purple",title.col = "Beltline Trail")+
    tm_shape(zip_pois_HJ)+
      tm_borders()+
      tm_dots(size = 0.1,
              col = "red",
              shape = 6,
              legend.show=F
      )+
      tm_text("name",size=0.5,col="black",ymod=0.4)+
    tm_shape(zip_pois[c(-19,-12,-1),])+
      tm_dots(size = 0.1,
              col = "POIs",
              title = "Points of Interest",
              palette = "red",
              shape = 6,
      )+
      tm_text("Points of\nInterest",size=0.5,col="black",shadow=T,xmod="xmod",ymod="ymod")+
   
     #
    
    tm_scale_bar(position = c(0.03,0.1))+
    tm_compass(position = c(0,0.17))+
    tm_credits("Data Source: NASA\nMOD11A1v6 1km LST",
               position = c(0.03,0.04))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes Land Temperature\nJune-August", str_sub(names(LST_seasonal_zip_summer[[i]]),7,10),"Average"),
              main.title.size=1.4,
              main.title.position="left",
              legend.outside=T,
              legend.text.size=0.8,
              legend.outside.size=0.2,
              outer.margins = c(0,0,0,0)
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(LST_seasonal_zip_summer[[21]])
names(LST_seasonal_zip_summer)
hottestmonthlist[[21]]

tmap_animation(hottestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_SummerMonthsLST_zip.gif"),delay=150,width=800,height=800)

for(i in 1:length(hottestmonthlist)){
  tmap_save(hottestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(LST_seasonal_zip_summer[[i]]),7,10)," Summer LST ZIP.png"),dpi=300)
}



coldestmonthlist <- list()

for(i in 1:nlayers(LST_seasonal_zip_winter)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(LST_seasonal_zip_winter[[i]])+
      tm_raster(col=names(LST_seasonal_zip_winter[[i]]),palette = rev(tempcolorvector),
              breaks =round(
                        c(seq(min(values(LST_seasonal_winter),na.rm=T),max(values(LST_seasonal_winter),na.rm=T),3),
                        max(values(LST_seasonal_winter),na.rm=T)),
                        digits=0),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Temp °F",legend.reverse=T)+
    
    tm_shape(interstateszip)+
      tm_lines(col = "Highways", 
             palette = "blue",
             labels = "",
             lwd=3
    )+
    
    #tm_shape(MARTAzip)+
    # tm_lines(col = "MARTA",
    #        palette="blue",
    #         labels="",
    #         lwd=1.5)+
    tm_shape(zip_beltline2)+
      tm_lines(col="Beltline",lwd=2,palette="purple",title.col = "Beltline Trail")+
    tm_shape(cropLongLat_zip)+
      tm_borders(col="black",lwd=2,lty="solid")+
      tm_text("ZIP Code",col="black",shadow=T,xmod ="xmod",size=1.2)+
    tm_shape(Parkszip)+
      tm_lines(alpha = 1, col = "Parks",palette="green",lwd=2)+
    
    tm_shape(zip_pois_HJ)+
      tm_borders()+
      tm_dots(size = 0.1,
              col = "red",
              shape = 6,
              legend.show=F
      )+
      tm_text("name",size=0.5,col="black",ymod=0.4)+

    tm_shape(zip_pois[c(-19,-12,-1),])+
      tm_dots(size = 0.1,
              col = "POIs",
              title = "Points of Interest",
              palette = "red",
              shape = 6,
      )+
      tm_text("Points of\nInterest",size=0.5,col="black",shadow=T,xmod="xmod",ymod="ymod")+
    
    #
    
    tm_scale_bar(position = c(0.03,0.1))+
    tm_compass(position = c(0,0.17))+
    tm_credits("Data Source: NASA\nMOD11A1v6 1km LST",
               position = c(0.03,0.04))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes Land Temperature\nDecember", str_sub(names(LST_seasonal_zip_winter[[i]]),7,10),"- Februrary", as.numeric(str_sub(names(LST_seasonal_zip_winter[[i]]),7,10))+1,"Average"),
              main.title.size=1.4,
              main.title.position="left",
              legend.outside=T,
              legend.text.size=0.8,
              legend.outside.size=0.2,
              outer.margins = c(0,0,0,0)
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(LST_seasonal_zip_winter[[1]])
coldestmonthlist[[1]]

tmap_animation(coldestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_WinterMonthsLST_zip.gif"),delay=150,width=800,height=800)

for(i in 1:length(coldestmonthlist)){
  tmap_save(coldestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(LST_seasonal_zip_winter[[i]]),7,10)," Winter LST ZIP.png"),dpi=300)
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

############
setwd(here::here("Data","1km Cropped Atlanta AOD"))
list.files()
for(i in 1:length(list.files())){
  input <- rast(list.files()[i])
  input <- project(input,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  writeRaster(input,filename = paste0(here::here("Data","1km Cropped Projected Atlanta AOD"),"/Projected ",list.files()[i]))
}


###########
setwd(here::here("Data","1km Cropped Projected Atlanta AOD"))
list.files()
AOD <- stack(list.files())

AODdates <- as.Date(as.numeric(substr(list.files(),35,37))-1,
                    origin = as.Date(paste0(substr(list.files(),31,34),"-01-01")))
AODseasons <- hydroTSM::time2season(AODdates,out.fmt="seasons")
AODyearandseason <- year_and_season(AODdates,AODseasons)

AOD_seasonal <- stackApply(AOD,AODyearandseason,mean)
res(AOD_seasonal)
names(AOD_seasonal)
max(values(AOD_seasonal),na.rm=T)


max(values(AOD_seasonal),na.rm=T)
AOD_seasonal <- crop(AOD_seasonal,cropLongLat5c_small)
AOD_seasonal <- mask(AOD_seasonal, cropLongLat5c_small)


AOD_seasonal_zip <- crop(AOD_seasonal,cropLongLat_zip)
AOD_seasonal_zip <- mask(AOD_seasonal_zip,cropLongLat_zip)
names(AOD_seasonal_zip)
plot(AOD_seasonal_zip[[1]])

AOD_seasonal_summer <- subset(AOD_seasonal,c(grep('summer',names(AOD_seasonal),fixed=T)))
AOD_seasonal_winter <- subset(AOD_seasonal,c(grep('winter',names(AOD_seasonal),fixed=T)))
plot(AOD_seasonal_summer[[1]])
plot(AOD_seasonal_winter[[1]])

AOD_seasonal_zip_summer <- subset(AOD_seasonal_zip,c(grep('summer',names(AOD_seasonal_zip),fixed=T)))
AOD_seasonal_zip_winter <- subset(AOD_seasonal_zip,c(grep('winter',names(AOD_seasonal_zip),fixed=T)))
plot(AOD_seasonal_zip_summer[[1]])
plot(AOD_seasonal_zip_winter[[1]])


target <- raster(resolution=res(AOD_seasonal)/7,crs=proj4string(AOD_seasonal),ext=extent(AOD_seasonal))
res(target)
AOD_seasonal_summer <- resample(AOD_seasonal_summer,target)
AOD_seasonal_winter <- resample(AOD_seasonal_winter,target)
AOD_seasonal_summer <- mask(AOD_seasonal_summer, cropLongLat5c_small)
AOD_seasonal_winter <- mask(AOD_seasonal_winter, cropLongLat5c_small)

target <- raster(resolution=res(AOD_seasonal_zip)/18,crs=proj4string(AOD_seasonal_zip),ext=extent(AOD_seasonal_zip))
res(target)
AOD_seasonal_zip_summer <- resample(AOD_seasonal_zip_summer,target)
AOD_seasonal_zip_winter <- resample(AOD_seasonal_zip_winter,target)
AOD_seasonal_zip_summer <- mask(AOD_seasonal_zip_summer, cropLongLat_zip)
AOD_seasonal_zip_winter <- mask(AOD_seasonal_zip_winter, cropLongLat_zip)


hottestmonthlist <- list()

for(i in 1:nlayers(AOD_seasonal_summer)){
  
  
  hottestmonthlist[[i]]<-
    tm_shape(AOD_seasonal_summer[[i]])+
    tm_raster(col=names(AOD_seasonal_summer[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(AOD_seasonal_summer),na.rm=T),max(values(AOD_seasonal_summer),na.rm=T),0.08),
                        max(values(AOD_seasonal_summer),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air Pollution",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MCD19A2v061 1km AOD",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area Air Pollution\nJune-August", str_sub(names(AOD_seasonal_summer[[i]]),7,10),"Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
}
plot(AOD_seasonal_summer[[4]])
hottestmonthlist[[4]]

tmap_animation(hottestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_SummerMonthsAOD.gif"),delay=150,width=800,height=800)

for(i in 1:length(hottestmonthlist)){
  tmap_save(hottestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(AOD_seasonal_summer[[i]]),7,10)," Summer AOD.png"))
}


coldestmonthlist <- list()

for(i in 1:nlayers(AOD_seasonal_winter)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(AOD_seasonal_winter[[i]])+
    tm_raster(col=names(AOD_seasonal_winter[[i]]),palette = rev(tempcolorvector),
              breaks =c(seq(min(values(AOD_seasonal_winter),na.rm=T),max(values(AOD_seasonal_winter),na.rm=T),0.02),
                        max(values(AOD_seasonal_winter),na.rm=T)),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air Pollution",legend.reverse=T)+
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
    tm_credits("Data Source: NASA MCD19A2v061 1km AOD",
               position = c(0.5,0))+
    tm_layout(frame=F,
              main.title=paste("Study Area Air Pollution\nDecember", str_sub(names(LST_seasonal_winter[[i]]),7,10),"- Februrary", as.numeric(str_sub(names(LST_seasonal_winter[[i]]),7,10))+1,"Average"),
              main.title.size=1.4,
              main.title.position="center",
              legend.just = c("right","bottom"),
              legend.height=-0.45,
              legend.width=0.33,
              legend.frame=F,
              legend.position=c("RIGHT","BOTTOM"))
  
  
}
plot(AOD_seasonal_winter[[4]])
coldestmonthlist[[4]]
names(AOD_seasonal_winter)

tmap_animation(coldestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_WinterMonthsAOD.gif"),delay=150,width=800,height=800)

for(i in 1:length(coldestmonthlist)){
  tmap_save(coldestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(AOD_seasonal_winter[[i]]),7,10)," Winter AOD.png"))
}

##########
hottestmonthlist <- list()

for(i in 1:nlayers(AOD_seasonal_zip_summer)){
  
  
  hottestmonthlist[[i]]<-
    tm_shape(AOD_seasonal_zip_summer[[i]])+
    tm_raster(col=names(AOD_seasonal_zip_summer[[i]]),palette = rev(tempcolorvector),
              breaks =round(
                c(seq(min(values(AOD_seasonal_summer),na.rm=T),max(values(AOD_seasonal_summer),na.rm=T),0.08),
                  max(values(AOD_seasonal_summer),na.rm=T)),
                digits=3),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air Pollution",legend.reverse=T)+
   
    tm_shape(interstateszip)+
    tm_lines(col = "Highways", 
             palette = "blue",
             labels = "",
             lwd=3
    )+
    tm_shape(zip_beltline2)+
    tm_lines(col="Beltline",lwd=2,palette="purple",title.col = "Beltline Trail")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("ZIP Code",col="black",shadow=T,xmod ="xmod",size=1.2)+
    tm_shape(Parkszip)+
    tm_lines(alpha = 1, col = "Parks",palette="green",lwd=2)+
    
    #tm_shape(MARTAzip)+
    # tm_lines(col = "MARTA",
    #        palette="blue",
    #         labels="",
    #         lwd=1.5)+
    
    tm_shape(zip_pois_HJ)+
    tm_borders()+
    tm_dots(size = 0.1,
            col = "red",
            shape = 6,
            legend.show=F
    )+
    tm_text("name",size=0.5,col="black",ymod=0.4)+
    tm_shape(zip_pois[c(-19,-12,-1),])+
    tm_dots(size = 0.1,
            col = "POIs",
            title = "Points of Interest",
            palette = "red",
            shape = 6,
    )+
    tm_text("Points of\nInterest",size=0.5,col="black",shadow=T,xmod="xmod",ymod="ymod")+
    
    #

    tm_scale_bar(position = c(0.03,0.17))+
    tm_compass(position = c(0,0.24))+
    tm_credits("Data Source: NASA\nMCD19A2v061 1km AOD",
               position = c(0.03,0.1))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes Air Pollution\nJune-August", str_sub(names(AOD_seasonal_zip_summer[[i]]),7,10),"Average"),
              main.title.size=1.4,
              main.title.position="left",
              legend.outside=T,
              legend.text.size=0.8,
              legend.outside.size=0.2,
              outer.margins = c(0,0,0,0)
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(AOD_seasonal_zip_summer[[21]])
names(AOD_seasonal_zip_summer)
hottestmonthlist[[21]]

tmap_animation(hottestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_SummerMonthsAOD_zip.gif"),delay=150,width=800,height=800)

for(i in 1:length(hottestmonthlist)){
  tmap_save(hottestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(AOD_seasonal_zip_summer[[i]]),7,10)," Summer AOD ZIP.png"),dpi=300)
}



coldestmonthlist <- list()

for(i in 1:nlayers(AOD_seasonal_zip_winter)){
  
  
  coldestmonthlist[[i]]<-
    tm_shape(AOD_seasonal_zip_winter[[i]])+
    tm_raster(col=names(AOD_seasonal_zip_winter[[i]]),palette = rev(tempcolorvector),
              breaks =round(
                c(seq(min(values(AOD_seasonal_winter),na.rm=T),max(values(AOD_seasonal_winter),na.rm=T),0.02),
                  max(values(AOD_seasonal_winter),na.rm=T)),
                digits=3),
              # labels = c("Low\nVegetation","","","","","","","","High\nVegetation"),
              style = "cont",title = "Air Pollution",legend.reverse=T)+
    
    tm_shape(interstateszip)+
    tm_lines(col = "Highways", 
             palette = "blue",
             labels = "",
             lwd=3
    )+
    
    #tm_shape(MARTAzip)+
    # tm_lines(col = "MARTA",
    #        palette="blue",
    #         labels="",
    #         lwd=1.5)+
    tm_shape(zip_beltline2)+
    tm_lines(col="Beltline",lwd=2,palette="purple",title.col = "Beltline Trail")+
    tm_shape(cropLongLat_zip)+
    tm_borders(col="black",lwd=2,lty="solid")+
    tm_text("ZIP Code",col="black",shadow=T,xmod ="xmod",size=1.2)+
    tm_shape(Parkszip)+
    tm_lines(alpha = 1, col = "Parks",palette="green",lwd=2)+
    
    tm_shape(zip_pois_HJ)+
    tm_borders()+
    tm_dots(size = 0.1,
            col = "red",
            shape = 6,
            legend.show=F
    )+
    tm_text("name",size=0.5,col="black",ymod=0.4)+
    
    tm_shape(zip_pois[c(-19,-12,-1),])+
    tm_dots(size = 0.1,
            col = "POIs",
            title = "Points of Interest",
            palette = "red",
            shape = 6,
    )+
    tm_text("Points of\nInterest",size=0.5,col="black",shadow=T,xmod="xmod",ymod="ymod")+
    
    #
    
    tm_scale_bar(position = c(0.03,0.17))+
    tm_compass(position = c(0,0.24))+
    tm_credits("Data Source: NASA\nMCD19A2v061 1km AOD",
               position = c(0.03,0.1))+
    tm_layout(frame=F,
              main.title=paste("Select ZIP Codes Air Pollution\nDecember", str_sub(names(LST_seasonal_zip_winter[[i]]),7,10),"- Februrary", as.numeric(str_sub(names(LST_seasonal_zip_winter[[i]]),7,10))+1,"Average"),
              main.title.size=1.4,
              main.title.position="left",
              legend.outside=T,
              legend.text.size=0.8,
              legend.outside.size=0.2,
              outer.margins = c(0,0,0,0)
              #legend.just = c("right","bottom"),
              #legend.height=-0.45,
              #legend.width=0.33
              #legend.frame=F,
              #legend.position=c("RIGHT","BOTTOM")
    )
  
}
plot(AOD_seasonal_zip_winter[[1]])
coldestmonthlist[[1]]

tmap_animation(coldestmonthlist,here::here("Atlanta Project","Results_New","Workshop 2 Figures","2002-2022_WinterMonthsAOD_zip.gif"),delay=150,width=800,height=800)

for(i in 1:length(coldestmonthlist)){
  tmap_save(coldestmonthlist[[i]],paste0(here::here("Atlanta Project","Results_New","Workshop 2 Figures"),"/",str_sub(names(AOD_seasonal_zip_winter[[i]]),7,10)," Winter AOD ZIP.png"),dpi=300)
}


#####Map PM Monitoring Stations#####
#####Map EPA Data#####
#######Create combined EPA Speciation Data file#####
#PM_speciation <- read.csv(here::here("Data","PM2.5 Speciation",list.filenames[[file]]))

list.files(here::here("Data","PM2.5 Speciation"))[grepl("^daily_SPEC.*..csv$",list.files(here::here("Data","PM2.5 Speciation")))]
list.filenames <- list.files(here::here("Data","PM2.5 Speciation"))[grepl("^daily_SPEC.*..csv$",list.files(here::here("Data","PM2.5 Speciation")))]
PM_speciation_list <- list()

for(file in 1:length(list.filenames)){
  PM_speciation <- read.csv(here::here("Data","PM2.5 Speciation",list.filenames[[file]]))
  PM_speciation <- PM_speciation %>% 
    filter(State.Name == "Georgia") %>%
    group_by(Latitude,Longitude)%>%
    slice(1)%>%
    ungroup()%>%
    dplyr::select(Latitude,Longitude,Parameter.Name,Date.Local,Units.of.Measure,Arithmetic.Mean,Local.Site.Name,Address,State.Name)
  PM_speciation_list[[file]] <- PM_speciation
  print(file)
}

PM_speciation_reduced <- bind_rows(PM_speciation_list)
PM_speciation_reduced.sf <- PM_speciation_reduced %>%
  distinct(Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs=4269)
PM_speciation_reduced.sf <- PM_speciation_reduced.sf %>%
  st_intersection(cropLongLat5c_small)
PM_speciation_reduced.sf$Station <- "EPA Pollution Monitoring Stations"

tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod", size=2)+
  tm_borders(col="black",lwd=3)+
  
tm_shape(five_c_motorways)+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
tm_shape(cropLongLat_zip)+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_fill(col="Unit", palette = "lightblue", alpha= 0.5, title = "Selected\nZIP Codes",labels = "")+
tm_shape(PM_speciation_reduced.sf)+
  tm_dots(col = "Station",palette= "red", shape = 21, size = 0.7, title = "EPA Pollution\nMonitoring Stations",labels="")+
  
  
tm_scale_bar(position = c(0.05,0.01))+
tm_compass(position = c(0.03,0.07))+
  # tm_credits("Data Source: NASA MOD13A2v6 500m NDVI",
  #  position = c(0.5,0))+
tm_layout(frame=F,
            # main.title=paste("Study Area Greenspace\n", str_sub(names(NDVI[[i]]),7,10), "Average"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))



five_c_weather_stations <- read.csv(here::here("Data","NOAA Daily Weather Data","Five Counties NOAA CDO Daily Weather.csv")) %>%
  distinct(LATITUDE,LONGITUDE) %>%
  st_as_sf(coords=c("LONGITUDE","LATITUDE"),crs=4269)

plot(st_geometry(five_c_weather_stations))

five_c_weather_stations$Station <- "Weather Stations"


tm_shape(five_c_motorways,bbox = st_bbox(cropLongLat_5c))+
  tm_lines(col = "Highways", 
           palette = "grey",
           labels = "",
           lwd=2,
  )+
tm_shape(cropLongLat_zip,bbox = st_bbox(cropLongLat_5c))+
  tm_borders(col="black",lwd=2,lty="solid")+
  tm_fill(col="Unit", palette = "lightblue", alpha= 0.5, title = "Selected\nZIP Codes",labels = "")+

tm_shape(five_c_weather_stations, bbox = st_bbox(cropLongLat_5c))+
  tm_dots(col = "Station",palette= "red", shape = 21, size = 0.7, title = "Weather\nStations",labels="")+
  
tm_shape(cropLongLat_5c)+
  tm_text("NAME",shadow=T,ymod="ymod",xmod="xmod", size=2)+
  tm_borders(col="black",lwd=3)+
  



  
  
tm_scale_bar(position = c(0.05,0.01))+
tm_compass(position = c(0.03,0.07))+
  # tm_credits("Data Source: NASA MOD13A2v6 500m NDVI",
  #  position = c(0.5,0))+
tm_layout(frame=F,
            # main.title=paste("Study Area Greenspace\n", str_sub(names(NDVI[[i]]),7,10), "Average"),
            main.title.size=1.4,
            main.title.position="center",
            legend.just = c("right","bottom"),
            legend.height=-0.45,
            legend.width=0.33,
            legend.frame=F,
            legend.position=c("RIGHT","BOTTOM"))
