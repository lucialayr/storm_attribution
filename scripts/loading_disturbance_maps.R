# packages
library(terra)
library(sf)
library(sp)
library(tictoc)
library(dplyr)
library(raster)
library(snow)

# wd
wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution"
setwd(wd)

# all countries with data
cnts = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
            "bulgaria", "croatia", "czechia", "denmark", "estonia",
            "finland", "france", "germany", "greece", "hungary", 
            "ireland", "italy", "latvia", "lithuania", "macedonia", 
            "moldova", "montenegro", "netherlands", "norway", "poland", 
            "portugal", "romania", "serbia", "slovakia", "slovenia", 
            "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")

codes = list("al", "at", "by", "be", "ba",
            "bg", "hr", "cz", "dk", "ee",
            "fi", "fr", "de", "gr", "hu", 
            "ie", "it", "lv", "lt", "mk", 
            "md", "me", "nl", "no", "pl", 
            "pt", "ro", "rs", "sk", "si", 
            "es", "se", "ch", "ua","gb") # iso 3166


# matrix for raster reclassification
reclass_list = c(0, 1.5, NA, 2.5, 3.5, NA)
reclass_m = matrix(reclass_list, ncol = 3, byrow = TRUE)



for (i in c(34:35)) {
  print(paste(cnts[i], 'starting...'))
  
  country = paste(cnts[i], 'finished. Total time: ')
  tic(country)
  
  #create polygons for windthrows
    tic("load and reclass disturbances")
      raster_1_path = paste0("data/attributed_disturbance_maps/agent_classes_", cnts[i], ".tif")
      raster_1 = terra::rast(raster_1_path)
      raster_1 = terra::classify(raster_1, reclass_m)
    toc(log = TRUE)
  
    tic("load years")
      raster_2_path = paste0(wd, "/data/disturbance_maps/disturbance_year_grouped_", cnts[i], ".tif")
      raster_2 = terra::rast(raster_2_path)
    toc(log = TRUE)

    tic("subset years for windthrow")
      stack = c(raster_1, raster_2)
      raster_3 = terra::lapp(stack, fun = function(raster_1, raster_2) {
        raster_1 + raster_2 - raster_1 # add/substract windthrow from years turn all years != windthrow as NA
      })
    toc(log = TRUE)

    tic("convert to polygon")
      poly = terra::as.polygons(raster_3)
    toc(log = TRUE)
    
    tic("safe, reload  as simple feature and cast")
      target_path = paste0("data/intermed/windthrow_", cnts[i], ".shp" )
      terra::writeVector(poly, target_path, delete_dsn = TRUE)
      poly = st_read(target_path, quiet = TRUE)
      poly = st_cast(poly, to = "POLYGON")
    toc(log = TRUE)
    
    tic("clean up attributes")
      poly = poly %>% dplyr::rename(year = lyr1) %>%
        dplyr::mutate(id = paste0(codes[i], "_", dplyr::row_number()))
    toc(log = TRUE)
    
    tic("save final shapefile")
      st_write(poly, target_path, delete_dsn=TRUE)
    toc(log = TRUE)
    
  toc(log = TRUE)
  
  if (file.exists(target_path)) {
    print(paste(cnts[i], "sucessfully written to disk"))
  } else {
    print(paste(cnts[i], "was not written to disk"))
  }
}



