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
        target_path = paste0("data/rasterize_windthrow_on_storms/rasterize_windthrow_", Country[i], ".tif" )
        raster::writeRaster(windthrow_rast[[1]], target_path, overwrite=TRUE)
        windthrow_df = raster::as.data.frame(windthrow_rast[[1]], xy = TRUE)
        df = full_join(df, windthrow_df)
        
        table_path = paste0("data/rasterize_windthrow_on_storms/", year, ".csv")
        write.csv(df, table_path, append = FALSE)
        toc(log=TRUE)
        } else { print(paste0(Country[i], "not used")) } 
      toc(log = TRUE)
  }
  
  path_final_table = paste0("data/rasterize_windthrow_on_storms/complete_", year, ".csv")
  write.csv(df, path_final_table, append = FALSE)
  return(df)
  
}


# data analysis

Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
               "bulgaria", "croatia", "czechia", "denmark", "estonia",
               "finland", "france", "germany", "greece", "hungary", 
               "ireland", "italy", "latvia", "lithuania", "macedonia", 
               "moldova", "montenegro", "netherlands", "norway", "poland", 
               "portugal", "romania", "serbia", "slovakia", "slovenia", 
               "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")

df = data.frame("x" = NA, "y" = NA, "windthrow_frac" = NA )
for (i in c(1:length(Country))) {
  print(Country[i])
  path = paste0("data/rasterize_windthrow_on_storms/rasterize_windthrow_", Country[i], ".tif" )
  if (file.exists(path) == TRUE) {
  wind_frac_tif = raster::raster(path)
  wind_frac_df = raster::as.data.frame(wind_frac_tif[[1]], xy = TRUE)
  names(wind_frac_df) = c("x", "y", "windthrow_frac")
  #wind_frac_df = wind_frac_df[wind_frac_df$windthrow_frac != 0, ]
  df = df %>% full_join(wind_frac_df)}
}

write.csv(df, "data/rasterize_windthrow_on_storms/data_wt_1990.csv")



europe_land = raster::shapefile("data/europe_countries.shp") 
vivian_storm = raster::raster("data/cyclones/00_biasMean_proj_Vivian.tif") %>%
  raster::mask(europe_land)
wiebke_storm = raster::raster("data/cyclones/00_biasMean_proj_Wiebke.tif") %>%
  raster::mask(europe_land)


df_vivian = raster::as.data.frame(vivian_storm$X00_biasMean_proj_Vivian, xy = TRUE)
names(df_vivian) = c("x", "y", "wind_speed_V")

df_wiebke = raster::as.data.frame(wiebke_storm$X00_biasMean_proj_Wiebke, xy = TRUE)
names(df_wiebke) = c("x", "y", "wind_speed_W")

df_storms = full_join(df_vivian, df_wiebke, vy = c("x", "y"))
df_full_data = full_join(df, df_storms, by = c("x", "y")) 
df_full_data = df_full_data %>%
  arrange(windthrow_frac)

(ggplot() + geom_point(data = df_full_data, aes(x = wind_speed_V, y = wind_speed_W, color = windthrow_frac), alpha = .75, pch = 16) +
    scale_fill_gradientn(colors = c("white",  "#FFE4E1",  high="#DC143C"),
                         values = scales::rescale(c(0, 0.0015, 0.09))) +
  scale_color_gradientn(colors = c("white",  "#FFE4E1",  high="#DC143C"),
                       values = scales::rescale(c(0, 0.0015, 0.09)))+
    geom_hline(yintercept = 20) + geom_vline(xintercept = 20))
