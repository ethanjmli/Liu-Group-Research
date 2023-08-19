setwd("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
setwd("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/DayMet Processed Files 2")
list.files()

crop_parameters_5c <- st_read("C:/Users/EJLI4/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
crop_parameters_5c <- st_read("/Users/ethan_j_li/OneDrive - Emory University/Liu Group Research/ATL_shps_for_April/five_counties.shp")
cropLongLat_5c <- st_transform(crop_parameters_5c,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

daymet <- readRDS(list.files()[1])
daymet_coords <- st_as_sf(daymet[,c(2,3)], coords = c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84 +no_defs")

daymet_5c_coords <- st_intersection(daymet_coords, st_difference(cropLongLat_5c))
daymet_5c_coords <-st_coordinates(daymet_5c_coords)
daymet_5c_coords <- as.data.frame(daymet_5c_coords)
names(daymet_5c_coords) <- c("Longitude","Latitude")
daymet_5c <- merge(daymet, daymet_5c_coords, by = c("Longitude","Latitude"))

daymet_zip_coords <- st_intersection(daymet_coords, cropLongLat_zip)
daymet_zip_coords <-st_coordinates(daymet_zip_coords)
daymet_zip_coords <- as.data.frame(daymet_zip_coords)
names(daymet_zip_coords) <- c("Longitude","Latitude")
daymet_zip <- merge(daymet, daymet_zip_coords, by = c("Longitude","Latitude"))

daymet_5c_relevant <- daymet_5c[,c(1,2,4,5,11,12)]
daymet_zip_relevant <- daymet_zip[,c(1,2,4,5,11,12)]

#install.packages("spatstat")
library(Metrics)
suppressMessages(library(spatstat))

obs_window_5c <- owin(xrange=range(daymet_5c_relevant$Longitude),yrange=range(daymet_5c_relevant$Latitude))
ppp_daymet_5c <- ppp(daymet_5c_relevant$Longitude,daymet_5c_relevant$Latitude,
                  marks = daymet_5c_relevant[,c(3,4,5,6)],window=obs_window_5c)

obs_window_zip <- owin(xrange=range(daymet_zip_relevant$Longitude),yrange=range(daymet_zip_relevant$Latitude))
ppp_daymet_zip <- ppp(daymet_zip_relevant$Longitude,daymet_zip_relevant$Latitude,
                  marks = daymet_zip_relevant[,c(3,4,5,6)],window=obs_window_zip)

powers <- seq(7.48,7.50,0.001)
mse_result <- NULL
mse_result_2 <- NULL
for(power in powers){
  CV_idw <- idw(ppp_daymet, power=power, at="points")
  mse_result_2 <- c(mse_result_2,Metrics::mse(ppp_daymet$marks,CV_idw))
  print(power)
}
optimal_power <- powers[which.min(mse_result_2)]
optimal_power

idw_daymet_5c <- idw(ppp_daymet_5c, power=7.488, at="pixels", dimyx = c(63, 63))
idw_daymet_zip <- idw(ppp_daymet_zip, power=7.488, at="pixels", dimyx = c(500,500))

rasterlist_5c <- list()
rasterlist_5c <- lapply(idw_daymet_5c, function(x){
  raster(x)
})

rasterlist_zip <- list()
rasterlist_zip <- lapply(idw_daymet_zip, function(x){
  raster(x)
})

idw_raster_5c <- stack(rasterlist_5c)
idw_raster_5c <- crop(idw_raster_5c, cropLongLat_5c)
idw_raster_5c <- mask(idw_raster_5c, cropLongLat_5c)
crs(idw_raster_5c) <- "+proj=longlat +datum=WGS84 +no_defs"
res(idw_raster_5c)

idw_raster_zip <- stack(rasterlist_zip)
idw_raster_zip <- crop(idw_raster_zip, cropLongLat_zip)
idw_raster_zip <- mask(idw_raster_zip, cropLongLat_zip)
crs(idw_raster_zip) <- "+proj=longlat +datum=WGS84 +no_defs"
res(idw_raster_zip)


idwcoords <- xyFromCell(idw_raster_5c, seq_len(ncell(idw_raster_5c)))
idwdf5c <- as.data.frame(getValues(idw_raster_5c))
names(idwdf5c) <- c('tmin','tmax','tmean','RH')
idwdf5c <- cbind(idwcoords, idwdf5c)
idwdf5c <- idwdf5c[!is.na(idwdf5c$tmean),]
idw_sf_5c <- st_as_sf(idwdf5c, coords = c("x", "y"), 
                   crs= "+proj=longlat +datum=WGS84 +no_defs")

idwcoords <- xyFromCell(idw_raster_zip , seq_len(ncell(idw_raster_zip)))
idwdfzip <- as.data.frame(getValues(idw_raster_zip ))
names(idwdfzip) <- c('tmin','tmax','tmean','RH')
idwdfzip <- cbind(idwcoords, idwdfzip)
idwdfzip <- idwdfzip[!is.na(idwdfzip$tmean),]
idw_sf_zip <- st_as_sf(idwdfzip, coords = c("x", "y"), 
                   crs= "+proj=longlat +datum=WGS84 +no_defs")

st_write(idw_sf_5c,"5c_daymet.shp")

tm_shape(idw_sf_zip)+
  tm_dots(col = "tmean", size = 0.35, palette = tempcolorvector, 
          style = "cont",title = "Temperature °C",legend.col.reverse=T)+
tm_shape(cropLongLat_zip)+
  tm_borders(col="Black", lwd = 2)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_grid(lines=FALSE)+
  tm_layout(title="July Average Temperature 2000-2020",legend.height=-0.32,legend.frame=T)

tm_shape(idw_sf_5c)+
  tm_dots(col = "tmean", size = 0.35, palette = tempcolorvector, 
          style = "cont",title = "Temperature °C",legend.col.reverse=T)+
tm_shape(cropLongLat_5c)+
  tm_borders(col="Black", lwd = 2)+
  tm_text("NAME",col = "white",shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_grid(lines=FALSE)+
  tm_layout(title="July Average Temperature 2000-2020",legend.height=-0.32,legend.frame=T)



ggplot() + 
  geom_tile(data = idwdfzip,
            aes(x, y, fill = tmean),
            na.rm = TRUE
            #filter(Weekday2020maskdf, season == "winter")
  )+
  #facet_wrap(vars(period), ncol=4) +
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

tm_shape(idw_sf_zip)+
  tm_dots(col = "tmean", size = 0.0000000000001, palette = tempcolorvector, 
          style = "cont",title = "Temperature °C",legend.col.reverse=T)+
tm_shape(cropLongLat_zip)+
  tm_borders(col="Black", lwd = 2)+
tm_text("NAME",col = "white",shadow=T)+
  tm_scale_bar(position = c(0.05,0.01))+
  tm_compass(position = c(0.03,0.07))+
  tm_grid(lines=FALSE)+
  tm_layout(title="July Average Temperature 2000-2020",legend.height=-0.32,legend.frame=T)

