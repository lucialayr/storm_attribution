library(sf)
library(terra)
library(tictoc)
library(raster)
library(dplyr)

wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)

Storms = read.csv("data/cyclones/cyclones_named_hydro.txt")

reclass_list = c(0, 20.7, NA, 20.7, 100, 1)
reclass_m = matrix(reclass_list, ncol = 3, byrow = TRUE)

for (i in c(1:length(Storms$Name))) {
  print(paste(Storms$Name[i], 'starting...'))
  
  storms = paste(Storms$Name[i], 'finished. Total time: ')
  tic(storms)
    
    tic("reclassify")
      path_1 = paste0("data/cyclones/biasMean_proj_", Storms$Name[i], ".nc")
      raster_1 = terra::rast(path_1)
      raster_2 = terra::classify(raster_1, reclass_m)
      poly = terra::as.polygons(raster_2)
    toc(log = TRUE)
    
    tic("convert to polygon")
      target_path = paste0("data/cyclones/poly_", Storms$Name[i], ".shp")
      terra::writeVector(poly, target_path)
      poly = st_read(target_path, quiet = TRUE)
      poly = st_cast(poly, to = "POLYGON")
    toc(log = TRUE)
    
    
    tic("add attributes")
      poly = poly %>% 
        dplyr::mutate(year = Storms$Year[i],
                      month = Storms$Month[i],
                      storm_id = Storms$Name[i]) %>%
        select(-wind_speed)
        
    toc(log = TRUE)
    
    tic("Save final file")
      st_write(poly, target_path, delete_dsn=TRUE)
    toc(log = TRUE)
  toc(log = TRUE)
}



kyrill_reclass = terra::classify(kyrill_test, reclass_m)
poly = terra::as.polygons(kyrill_reclass)

terra::writeVector(poly, "data/winter_storms/poly.shp")
kyrill_rotated = nc_open("Kyrill_rawFoot.nc")
kyrill_data = ncvar_get(kyrill_rotated, "wind_speed_of_gust")
kyrill_raster = raster::raster(kyrill_rotated, crs=CRS("+proj=ob_tran +o_proj=longlat +o_lon_p=-162.0 +o_lat_p=39.25 +lon_0=180 +lat_0=0"))
kyrill_rotated = raster::raster("Kyrill_rawFoot.nc", crs = "+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=WGS84")
kyrill_proj = raster::projection(kyrill_rotated, "+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=180 +ellps=WGS84")
correct_grid = terra::rast("historic_regular_grid.nc")

historic_regular_grid_band_1 = correct_grid$tg_1

terra::writeCDF(historic_regular_grid_band_1, "historic_regular_grid_band_1.nc")

Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
            "bulgaria", "croatia", "czechia", "denmark", "estonia",
            "finland", "france", "germany", "greece", "hungary", 
            "ireland", "italy", "latvia", "lithuania", "macedonia", 
            "moldova", "montenegro", "netherlands", "norway", "poland", 
            "portugal", "romania", "serbia", "slovakia", "slovenia", 
            "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")

europe = terra::rast("data/attributed_disturbance_maps/agent_classes_albania.tif")

for (i in c(2:35)) {
  print(paste0(Country[i], ' starting...'))
  C = paste(Country[i], 'added. Total time: ')
  
  tic(C)
    raster_path = paste0("data/attributed_disturbance_maps/agent_classes_", Country[i], ".tif")
    raster_1 = terra::rast(raster_path)
    europe = terra::union(europe, raster_1)
  toc(log = TRUE)
}
  


cdo(csl("remapbil", "eur_grid_lon_lat"), Kyrill_rawFoot.nc, Kyrill_proj.nc)  
