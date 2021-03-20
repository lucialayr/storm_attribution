library(sf)
library(exactextractr)
library(raster)
library(tictoc)
library(dplyr)
library(ggplot2)


wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)

# data analysis

create_table = function(year) {
  
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
    path = paste0("data/rasterize_windthrow_on_storms/rasterize_windthrow_", Country[i], "_", year, ".tif" )
    if (file.exists(path) == TRUE) {
      wind_frac_tif = raster::raster(path)
      wind_frac_df = raster::as.data.frame(wind_frac_tif[[1]], xy = TRUE)
      names(wind_frac_df) = c("x", "y", "windthrow_frac")
      #wind_frac_df = wind_frac_df[wind_frac_df$windthrow_frac != 0, ]
      df = df %>% full_join(wind_frac_df)}
  }
  
  write.csv(df, "data/rasterize_windthrow_on_storms/data_wt_", year, ".csv" )
  return(df)
  
}


gudrun_data = create_table(2005)

scatterplot_storm_windthrow = function(year, storm) {
  
  path_wt = paste0("data/rasterize_windthrow_on_storms/data_wt_", year, ".csv")
  windthrow = read.csv(path_wt)
  print(names(windthrow))
  
  
  europe_land = raster::shapefile("data/europe_countries.shp") 
  path_storm = paste0("data/cyclones/00_biasMean_proj_", storm, ".tif")
  storm = raster::raster(path_storm) %>%
    raster::mask(europe_land)
  
  data_storm = raster::as.data.frame(storm[[1]], xy = TRUE)
  names(df_vivian) = c("x", "y", "wind_speed")
  print(head(data_storm))
  print(head(windthrow))
  
  full_data = full_join(windthrow, storm, by = c("x", "y")) 
  full_data = full_data %>%
    arrange(windthrow_frac)
  
  plot = ggplot() + geom_point(data = full_data, aes(x = wind_speed, y =  windthrow_frac), pch = 16) +
    geom_vline(xintercept = 20)
  
  return(plot)
  
}

gudrun_plot = scatterplot_storm_windthrow(2005, "Gudrun")


for (i in c(1:length(Country))) {
  
  path = paste0("data/rasterize_windthrow_on_storms/rasterize_windthrow_", Country[i], "_2007.tif" )
  if (file.exists(path) == TRUE) {
  print(Country[i])
  wind_frac_tif = raster::raster(path)
  wind_frac_df = raster::as.data.frame(wind_frac_tif[[1]], xy = TRUE)
  names(wind_frac_df) = c("x", "y", "windthrow_frac")
  #wind_frac_df = wind_frac_df[wind_frac_df$windthrow_frac != 0, ]
  df = df %>% full_join(wind_frac_df)}
}

write.csv(gudrun_data, "data/rasterize_windthrow_on_storms/data_wt_2005.csv")



europe_land = raster::shapefile("data/europe_countries.shp") 
vivian_storm = raster::raster("data/cyclones/00_biasMean_proj_Vivian.tif") %>%
  raster::mask(europe_land)
df_vivian = raster::as.data.frame(vivian_storm$X00_biasMean_proj_Vivian, xy = TRUE)
names(df_vivian) = c("x", "y", "wind_speed_V")


wiebke_storm = raster::raster("data/cyclones/00_biasMean_proj_Wiebke.tif") %>%
  raster::mask(europe_land)
df_wiebke = raster::as.data.frame(wiebke_storm$X00_biasMean_proj_Wiebke, xy = TRUE)
names(df_wiebke) = c("x", "y", "wind_speed_W")


gudrun_storm = raster::raster("data/cyclones/00_biasMean_proj_Gudrun.tif") %>%
  raster::mask(europe_land)
df_gudrun = raster::as.data.frame(gudrun_storm$X00_biasMean_proj_Gudrun, xy = TRUE)
names(df_gudrun) = c("x", "y", "wind_speed_W")
full_data_gudrun = full_join(df, df_gudrun, by = c("x", "y")) 

kyrill_storm = raster::raster("data/cyclones/00_biasMean_proj_Kyrill.tif") %>%
  raster::mask(europe_land)
df_kyrill = raster::as.data.frame(kyrill_storm[[1]], xy = TRUE)
names(df_kyrill) = c("x", "y", "wind_speed_W")
full_data_kyrill = full_join(df, df_kyrill, by = c("x", "y")) 





df_storms = full_join(df_vivian, df_wiebke, by = c("x", "y"))
df_full_data = full_join(df, df_gudrun, by = c("x", "y")) 
df_full_data = df_full_data %>%
  arrange(windthrow_frac)

(ggplot() + geom_point(data = df_full_data, aes(x = wind_speed_V, y = wind_speed_W, color = windthrow_frac), alpha = .75, pch = 16) +
    scale_fill_gradientn(colors = c("white",  "#FFE4E1",  high="#DC143C"),
                         values = scales::rescale(c(0, 0.0015, 0.09))) +
  scale_color_gradientn(colors = c("white",  "#FFE4E1",  high="#DC143C"),
                       values = scales::rescale(c(0, 0.0015, 0.09)))+
    geom_hline(yintercept = 20) + geom_vline(xintercept = 20))

(ggplot() + geom_point(data = full_data_gudrun, aes(x = wind_speed_W, y = windthrow_frac), alpha = .75, pch = 16) +
     geom_vline(xintercept = 20))
