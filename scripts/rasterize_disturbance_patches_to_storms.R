library(sf)
library(exactextractr)
library(raster)
library(tictoc)
library(dplyr)
library(ggplot2)


wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)


rasterize_windthrow_on_storms = function(year){
  Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
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
               "es", "se", "ch", "ua","gb")
  
  # raster template
  europe_land = raster::shapefile("data/europe_countries.shp") 
  vivian_storm = raster::raster("data/cyclones/00_biasMean_proj_Vivian.tif") %>%
    raster::mask(europe_land)
  
  df = data.frame(x = NA, y = NA, layer = NA)
  
  for (i in c(1:length(Country))){
    print(paste0(Country[i], ' starting...'))
    C = paste(Country[i], 'added. Total time: ')
    
    tic(C)
    
    windthrow_path = paste0("data/intermed/windthrow_", Country[i], ".shp")
    windthrow_sf = st_read(windthrow_path)
    windthrow_year = windthrow_sf[windthrow_sf$year == year,] %>% st_union()
    if (class(windthrow_year)[1] == "sfc_MULTIPOLYGON") { # escape empty geometries if no windthrow happened in this year, will be sfc instead of sf
      windthrow_rast = exactextractr::coverage_fraction(vivian_storm, windthrow_year)
      
      tic("save")
      target_path = paste0("data/rasterize_windthrow_on_storms/rasterize_windthrow_", Country[i], "_", year, ".tif" )
      raster::writeRaster(windthrow_rast[[1]], target_path, overwrite=TRUE)
      toc(log=TRUE)
    } else { print(paste0(Country[i], "not used")) } 
    toc(log = TRUE)
  }
  
  path_final_table = paste0("data/rasterize_windthrow_on_storms/complete_", year, ".csv")
  write.csv(df, path_final_table, append = FALSE)
  return(df)
  
}


rasterize_windthrow_on_storms(2005)

# rasterize forwind

wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)

europe_land = raster::shapefile("data/europe_countries.shp") 
vivian_storm = raster::raster("data/cyclones/00_biasMean_proj_Vivian.tif") %>%
  raster::mask(europe_land)

wd = "C:/Users/Lucia/Documents/Diss"
setwd(wd)

forwind = st_read("Data/FORWIND/FORWIND_v2.shp")
kyrill = forwind[forwind$StormName == "Kyrill",] %>% st_union()
kyrill_rasterized = exactextractr::coverage_fraction(vivian_storm, kyrill, crop = TRUE)
writeRaster(kyrill_rasterized[[1]], "Data/FORWIND/FORWIND_v2_kyrill_rasterized.shp" )
