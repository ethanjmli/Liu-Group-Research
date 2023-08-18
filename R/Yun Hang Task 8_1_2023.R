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
library(readxl)

tmap_options(check.and.fix = TRUE)

IMPROVE <- read.csv("/Users/ethan_j_li/OneDrive - Emory University/Attachments/IMPROVE (Interagency Monitoring of Protected Visual Environments) - Active.csv")
IMPROVE.add <- read.csv("/Users/ethan_j_li/OneDrive - Emory University/Attachments/IMPROVE (Interagency Monitoring of Protected Visual Environments) - Active_add.csv")
IMPROVE <- bind_rows(IMPROVE,IMPROVE.add)
IMPROVE <- subset(IMPROVE, !(State %in% c("Alaska","Hawaii","Country Of Mexico","Puerto Rico")))

Speciation <- read.csv("/Users/ethan_j_li/OneDrive - Emory University/Attachments/PM2.5 Chemical Speciation Network - Active.csv")
Speciation.add <- read.csv("/Users/ethan_j_li/OneDrive - Emory University/Attachments/PM2.5 Chemical Speciation Network - Active_add.csv")
Speciation <- bind_rows(Speciation,Speciation.add)
Speciation <- subset(Speciation, !(State %in% c("Alaska","Hawaii","Country Of Mexico","Puerto Rico")))
#States <- st_read("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/US States Shapefile/US States Shapefile.shp")
#plot(States)
States <- get_acs(geography = "state",
                  variable = "B01002_001",
                  geometry=T)
States <- subset(States,!(NAME%in% c("Alaska","Hawaii","Puerto Rico")))
States$`Climate Region` <- NA
States$`Climate Region` <- ifelse(States$NAME %in% c("Washington","Oregon","Idaho"),"Northwest",
                                ifelse(States$NAME %in% c("California","Nevada"),"West",
                                    ifelse(States$NAME %in% c("Utah","Colorado","Arizona","New Mexico"),"Soutwest",
                                        ifelse(States$NAME %in% c("Wyoming","Montana","North Dakota","South Dakota","Nebraska"),"West North Central",
                                            ifelse(States$NAME %in% c("Iowa","Minnesota","Michigan","Wisconsin"),"East North Central",
                                                ifelse(States$NAME %in% c("Texas","Kansas","Oklahoma","Arkansas","Louisiana","Mississippi"),"South",
                                                    ifelse(States$NAME %in% c("Missouri","Illinois", "Indiana","Ohio","Tennessee","Kentucky","West Virginia"),"Central",
                                                        ifelse(States$NAME %in% c("Alabama","Georgia","Florida","South Carolina","North Carolina","Virginia","District of Columbia"),"Southeast","Northeast"))))))))
States <- subset(States,select = c(NAME,geometry,`Climate Region`,GEOID))

IMPROVE.sf <- st_as_sf(IMPROVE, coords = c("x","y"))
IMPROVE.sf$IMPROVE <- "IMPROVE Stations"
Speciation.sf <- st_as_sf(Speciation, coords = c("x","y"))
Speciation.sf$Speciation <- "Speciation Stations"
#States.valid <- st_make_valid(States)

tm_shape(IMPROVE.sf)+
  tm_dots()
tm_shape(Speciation.sf)+
  tm_dots()
tm_shape(States)+
  tm_borders()
#tm_shape(States.valid)+
#  tm_borders()

tm_shape(States)+
  tm_polygons(col="Climate Region")+
tm_shape(IMPROVE.sf)+
  tm_dots(shape = 9, size= 0.1, col = "IMPROVE",title="IMPROVE",palette="red")+
tm_shape(Speciation.sf)+
  tm_dots(shape = 13, size = 0.1, col = "Speciation",title="Speciation",palette="black")+
tm_compass(position = c(0,0.1))+
tm_scale_bar(position = c("left","bottom"))+
tm_layout(legend.position = c(0.88,0),
          frame = F,
          main.title = "Distribution of PM2.5 Chemical Speciation Stations and IMPROVE Sites",
          main.title.size = 1.5,
          main.title.position = "center"
          )

  
  
  
  
  