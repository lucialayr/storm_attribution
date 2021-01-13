# packages
library(terra)
library(sf)
library(sp)
library(tictoc)
library(dplyr)
library(raster)


# issues:
# saving shapefile does not create a prj and shx file for some reasons
# this results in loaded shapefile not to have crs
# saving and reloading seems to be necessary for st_cast to do its job (for some reason)
# changing of attributed works, main issue is the saving but it appeared once I added the dplyr part in

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

# mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# matrix for raster reclassification
reclass_list = c(0, 1.5, NA, 2.5, 3.5, NA)
reclass_m = matrix(reclass_list, ncol = 3, byrow = TRUE)

for (i in 2) {
  print(paste(cnts[i], 'starting...'))
  
  country = paste(cnts[i], 'finished')
  tic(country)
  
  #create polygon
    tic("as polygons")
      raster_1_path = paste0(wd, "/data/attributed_disturbance_maps/agent_classes_", cnts[i], ".tif")
      raster_1 = terra::rast(raster_1_path)
      raster_1_reclass = terra::classify(raster_1, reclass_m)
      poly = terra::as.polygons(raster_1_reclass)
    toc(log = TRUE)
  
    tic("convert to simple feature")
      poly = as(poly, "Spatial")
      poly = as(poly, "sf" ) 
      target_path = paste0(wd, "/data/intermed/windthrow_", cnts[i], ".shp" )
    toc(log = TRUE)
  
    tic("write shapefile")
      st_write(poly, target_path, delete_dsn = TRUE)
    toc(log = TRUE)
  
    tic("create multiple polygons")
      poly = st_read(target_path)
      poly = st_cast(poly, "POLYGON")
    toc(log = TRUE)
  
    print(paste0(cnts[i], ": Polygon created"))
  
  
  # add year and clean up attributes
    tic("add attributes")
      raster_2_path = paste0(wd, "/data/disturbance_maps/disturbance_year_grouped_", cnts[i], ".tif")
      raster_2 = raster::raster(raster_2_path)
      poly = dplyr::mutate(poly,
                    year = extract(raster_2, poly, fun = mean, na.rm = TRUE), # function ensures one value per polygon. If polygon contains more than one year, this will give an incorrect year. Watch our for non-integers in final file
                    id = paste0(codes[i], "_", dplyr::row_number())) %>%
        dplyr::select(-agnt_c_)
      print(paste0(cnts[i], ": attributes:", names(poly)))
    toc(log = TRUE)

  # save file
    tic("save final file")
      target_path = paste0(wd, "/data/intermed/windthrow_", cnts[i], ".shp" )
      st_write(poly, target_path, delete_dsn = TRUE)
    toc(log = TRUE)
  
    remove(raster_1,  raster_2, poly, country)
  toc(log = TRUE)
  
  if (file.exists(target_path)) {
    print(paste(cnts[i], "sucessfully written to disk"))
  } else {
    print(paste(cnts[i], "was not written to disk"))
  }
  
}




netherlands_terra = rast("data/attributed_disturbance_maps/agent_classes_netherlands.tif")
netherlands_reclass = classify(netherlands_terra, reclass_m)
netherlands_poly = as.polygons(netherlands_reclass) 
netherlands_poly = as(netherlands_poly, "Spatial")
netherlands_poly = as(netherlands_poly, "sf")
netherlands_poly = st_cast(netherlands_poly, to = 'POLYGON')

netherlands_year = rast("data/disturbance_maps/disturbance_year_grouped_netherlands.tif")
netherlands_poly = extract(netherlands_year, netherlands_poly)
writeVector(netherlands_poly, "data/intermed/netherlands_terra_2_shp")

austria_terra = terra::rast("data/attributed_disturbance_maps/agent_classes_austria.tif")
austria_reclass = classify(austria_terra, reclass_m)
austria_poly = as.polygons(austria_reclass, dissolve = FALSE)



albania_year = raster::raster("data/disturbance_maps/disturbance_year_grouped_albania.tif")
albania_sf = st_read("data/intermed/windthrow_albania.shp")   %>% st_cast(albania_sf, to = "POLYGON")
albania_sf = albania_sf  %>%
  dplyr::mutate(albania_sf,
                year = extract(albania_year, albania_sf, fun = mean, na.rm = TRUE),
                id = paste0("ai_", dplyr::row_number()) ) %>% dplyr::select(-agnt_c_)

austria_windthrow_year = extract(austria_year, austria_sf)
austria_sf_years = cbind(austria_sf, austria_windthrow_year)


tic("Austria as spatvector")
plot(austria_poly)
toc()
tic("Austria as simple feature")
plot(austria_sf)
toc()






