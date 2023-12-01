pacman::p_load(MODIStsp,
               raster,
               sf,
               terra,
               stars,
               tidyverse,
               tidycensus,
               tmap,
               RColorBrewer,
               osmdata,
               here)


####Get Shapefiles####
crop_parameters_5c <- st_read(here::here("Data","ATL_shps_for_April","five_counties.shp"))
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = 4269)
cropLongLat_5c <- st_make_valid(cropLongLat_5c)
cropLongLat5c_small <- cropLongLat_5c[,9]


####AOD####
setwd("H:/Raw Atlanta AOD")
AODfilenames <- list.files()
AODfilenames
AODfilenames2 <- paste0(substr(AODfilenames,10,13),"_",substr(AODfilenames,14,16), "_",substr(AODfilenames,18,20))
AODfilenames2
AODfilenames2 <- substr(AODfilenames2,1,8)
table(table(AODfilenames2))
table(AODfilenames2)
which(table(AODfilenames2)==1)

aodcrs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"Unknown datum based upon the custom spheroid\",\n        DATUM[\"Not specified (based on custom spheroid)\",\n            ELLIPSOID[\"Custom spheroid\",6371007.181,0,\n                LENGTHUNIT[\"metre\",1,\n                    ID[\"EPSG\",9001]]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Sinusoidal\"],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"Meter\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"Meter\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"Meter\",1]]]"
cropLongLat_hdf <- st_transform(crop_parameters_5c,crs = aodcrs)
cropLongLat_hdf <- cropLongLat_hdf[,9]
crs(cropLongLat_hdf)


for(i in seq(from=1,to=length(AODfilenames),by=2)){
  a <- sds(AODfilenames[i])
  b<- sds(AODfilenames[i+1])
  a <- a[2]
  b <- b[2]
  a <- mean(a,na.rm=TRUE)
  b<-mean(b,na.rm=TRUE)
  a <- crop(a, cropLongLat_hdf)
  b <- crop(b,cropLongLat_hdf)
  ab<-merge(a,b)
  writeRaster(ab, here::here("Data","Cleaned AOD",paste0(str_sub(AODfilenames[i],10,16)," Atlanta AOD.tif")))
  print(i)
}


list.filenames <- list.files(here::here("Data","Cleaned AOD"))
for(i in 1:length(list.filenames)){
  unprojected <- rast(here::here("Data","Cleaned AOD",list.filenames[i]))
  projected <- project(unprojected, "epsg:4269")
  writeRaster(projected,here::here("Data","Final AOD",paste0(str_sub(list.filenames[i],1,7)," Projected Atlanta AOD.tif")))
  print(i)
}


list.filenames <- list.files(here::here("Data","1km Cropped Projected Atlanta AOD"))
for(i in 1:length(list.filenames)){
  a <- raster(here::here("Data","1km Cropped Projected Atlanta AOD",list.filenames[i]))
  a <- crop(a, cropLongLat5c_small)
  writeRaster(a,here::here("Data","Final AOD",paste0("Final ",str_sub(list.filenames[i],-23,-17)," Atlanta AOD.tif")))
  print(i)
}


a <- raster(here::here("Data","Final AOD","Final 2022306 Atlanta AOD.tif"))
b <- raster(here::here("Data","Final AOD","Final 2023074 Atlanta AOD.tif"))
