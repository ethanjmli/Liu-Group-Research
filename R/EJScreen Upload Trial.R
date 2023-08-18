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

setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI")
NDVItest <- raster("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/MOD13A2v6 500m NDVI 2002_01_01-2023_03_23/NDVI/MOD13A1_NDVI_2020_225.tif")
plot(NDVItest)
NDVItest <- crop(NDVItest,cropLongLat_5c)
NDVItest <- mask(NDVItest,cropLongLat_5c)
plot(NDVItest)
NDVItest
values(NDVItest)<-values(NDVItest)*0.0001
plot(NDVItest)
NDVItest.sf <- data.frame(rasterToPoints(NDVItest))
header <- c("x","y","NDVI")
NDVItest.sf <- rbind(header,NDVItest.sf)
write.csv(NDVItest.sf,"EJScreenTest.csv")
NDVItest.sf <- st_as_sf(NDVItest.sf,coords=c("x","y"),crs="+proj=longlat +datum=WGS84 +no_defs")
plot(NDVItest.sf)

st_crs(NDVItest.sf) <- "+proj=longlat +datum=WGS84 +no_defs"
library(rgdal)
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April")
st_write(NDVItest.sf,"EJScreenTest.csv")


