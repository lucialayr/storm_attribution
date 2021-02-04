library(sf)
library(terra)
library(tictoc)
library(raster)
library(dplyr)

wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)

Storms = read.csv("data/cyclones/cyclones_all.txt")
 
Storms = Storms %>% dplyr::mutate(
  hydro_Year = ifelse(Month < 9, Year, Year +1 )
)

threshhold = 20.7
mode = "biasMean"
reclass_list = c(0, threshhold, NA, threshhold, 100, 1)
reclass_m = matrix(reclass_list, ncol = 3, byrow = TRUE)

for (i in c(1:length(Storms$Name))) {
  print(paste(Storms$Name[i], 'starting...'))

  storms = paste(Storms$Name[i], 'finished. Total time: ')
  tic(storms)

    tic("reclassify")
      path_1 = paste0("data/cyclones/", mode, "_proj_", Storms$Name[i], ".nc")
      raster_1 = terra::rast(path_1)
      raster_2 = terra::classify(raster_1, reclass_m)
      poly = terra::as.polygons(raster_2)
    toc(log = TRUE)

    tic("convert to polygon")
      target_path = paste0("data/storm_outlines/poly_", mode, "_", Storms$Name[i],"_", threshhold, ".shp")
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
      poly = st_cast(poly, to = "POLYGON")
    toc(log = TRUE)

    tic("Save final file")
      st_write(poly, target_path, delete_dsn=TRUE)
    toc(log = TRUE)
  toc(log = TRUE)
}



