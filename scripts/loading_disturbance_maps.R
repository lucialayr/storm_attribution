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

for (i in c(1)) {
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
      poly = st_read(target_path)
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



#textcountry small
albania_year = terra::rast("data/disturbance_maps/disturbance_year_grouped_albania.tif")
albania_disturbance = terra::rast("data/attributed_disturbance_maps/agent_classes_albania.tif")
albania_windthrow = terra::classify(albania_disturbance, reclass_m)

stack = c(albania_year, albania_windthrow)

albania_windthrow_years = terra::lapp(stack, 
                                   fun = function (albania_year, albania_windthrow) {
                                     albania_year + albania_windthrow - albania_windthrow
                                     # return(albania_year)
                                     })

albania_poly = terra::as.polygons(albania_windthrow_years)
terra::writeVector(albania_poly, "albania_poly.shp")

albania_shp = terra::as.polygons(stack_2)
writeVector(albania_shp, "albania_shp.shp")

albania_sf = st_read("data/intermed/windthrow_albania.shp")   %>% st_cast(albania_sf, to = "POLYGON")
albania_sf = albania_sf  %>%
  dplyr::mutate(albania_sf,
                year = extract(albania_year, albania_sf, fun = mean, na.rm = TRUE),
                id = paste0("ai_", dplyr::row_number()) ) %>% dplyr::select(-agnt_c_)

# testcountry large
austria_terra = terra::rast("data/attributed_disturbance_maps/agent_classes_austria.tif")
austria_reclass = classify(austria_terra, reclass_m)
austria_poly = as.polygons(austria_reclass, dissolve = FALSE)
austria_windthrow_year = extract(austria_year, austria_sf)
austria_sf_years = cbind(austria_sf, austria_windthrow_year)

# analysis: polygons with multiple years result in polygons inbetween years

windthrow_counts = windthrow_austria %>% count(year) 
plot(windthrow_counts$year, windthrow_counts$n, main = "numer of polygon with a certain year")

years = seq(from = min(windthrow_counts$year), to = max(windthrow_counts$year), by = 1 )
windthrow_badyears = windthrow_counts %>% filter(year %in% years == FALSE)
Main = paste0("Polygons with invalid years \n Total: ", 
              sum(windthrow_badyears$n), 
              "\n Percentage: ", 
              round(100*sum(windthrow_badyears$n)/length(windthrow_austria$id), digits = 1), "%")
plot(windthrow_badyears$year, windthrow_badyears$n, main = Main, cex = .5, bg = "darkgrey", pch = 21)

# postgis
# https://postgis.net/install/
# https://postgis.net/windows_downloads/
# http://www.bostongis.com/PrinterFriendly.aspx?content_name=postgis_tut01
# https://www.enterprisedb.com/postgresql-tutorial-resources-training?cid=437
# https://postgis.net/docs/postgis_installation.html#install_short_version
# https://medium.com/@tjukanov/why-should-you-care-about-postgis-a-gentle-introduction-to-spatial-databases-9eccd26bc42b
 
  









