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

# mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# matrix for raster reclassification
reclass_list = c(0, 1.5, NA, 2.5, 3.5, NA)
reclass_m = matrix(reclass_list, ncol = 3, byrow = TRUE)

for (i in c(2)) {
  print(paste(cnts[i], 'starting...'))
  
  country = paste(cnts[i], 'finished')
  tic(country)
  
  #create polygon
    tic("as polygons")
      raster_1_path = paste0("data/attributed_disturbance_maps/agent_classes_", cnts[i], ".tif")
      raster_1 = terra::rast(raster_1_path)
      raster_1_reclass = terra::classify(raster_1, reclass_m)
      poly = terra::as.polygons(raster_1_reclass)
    toc(log = TRUE)
  
    #tic("convert to simple feature")
    #  poly = as(poly, "Spatial")
    #  poly = as(poly, "sf" ) 
    #  target_path = paste0(wd, "/data/intermed/windthrow_", cnts[i], ".shp" )
    #toc(log = TRUE)
  
    tic("write shapefile")
      target_path = paste0(wd, "/data/intermed/windthrow_", cnts[i], ".shp" )
      writeVector(poly, target_path, overwrite=TRUE) # st_casts only works if shapefile is loaded from disk fsr
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
      # beginCluster()
      poly = dplyr::mutate(poly,
                    year = raster::extract(raster_2, poly, fun = mean, na.rm = TRUE), # function ensures one value per polygon. If polygon contains more than one year, this will give an incorrect year. Watch our for non-integers in final file
                    id = paste0(codes[i], "_", dplyr::row_number())) %>%
        dplyr::select(-agent_clas)
      endCluster()
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



#textcountry small
albania_year = raster::raster("data/disturbance_maps/disturbance_year_grouped_albania.tif")
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
 
  









