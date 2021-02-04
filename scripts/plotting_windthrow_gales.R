# packages
library(sf)
library(ggplot2)
library(tictoc)

# wd
wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution"
setwd(wd)

# base map
europe_sf = st_read("data/europe_countries.shp")
base_map = ggplot() 

threshhold = 20.7
mode = "biasMean"
# variables, layers
Year = seq(from = 1986, to = 2016, by = 1)

Storms = read.csv("data/cyclones/cyclones_all.txt")

Storms = Storms %>% dplyr::mutate(
  hydro_Year = ifelse(Month < 9, Year, Year +1 )
)

Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
               "bulgaria", "croatia", "czechia", "denmark", "estonia",
               "finland", "france", "germany", "greece", "hungary", 
               "ireland", "italy", "latvia", "lithuania", "macedonia", 
               "moldova", "montenegro", "netherlands", "norway", "poland", 
               "portugal", "romania", "serbia", "slovakia", "slovenia", 
               "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")

#create a map for each year
for (j in c(1:length(Year))) {
  windthrow_map = base_map
  print(paste(Year[j], 'starting...'))
  Y = paste(Year[j], 'added. Total time: ')
  title = paste0("Storms with threshhold ", threshhold, "m/s. \nYear: ", Year[j], "\nStorms: ")
  tic(Y)
  
  #add all storms happening in this year
    tic(paste0("Storms for ", Year[j]))
      for (k in c(1:length(Storms$Name)) ) {
        if (Storms$hydro_Year[k] == Year[j]){
          print("loop started sucessful")
          storm_path = paste0("data/storm_outlines/poly_", mode, "_", Storms$Name[k], "_", threshhold, ".shp")
          storm = st_read(storm_path, quiet = TRUE)
          windthrow_map = windthrow_map + geom_sf(data = sf::st_geometry(storm),  
                                                  fill = "#5F9EA0", 
                                                  color = NA , 
                                                  alpha = 0.18) 
            
          
          title = paste0(title, Storms$Name[k], " (Month ",Storms$Month[k], "), ")
          print(title)
        } 
      }
    
    windthrow_map = windthrow_map + geom_sf(data = europe_sf, alpha = 0)
    toc(log = TRUE)
    
  #plot disturbance patches per country
    for (i in c(1:35)){
      print(paste0(Country[i], ' (', Year[j], ') starting...'))
      C = paste(Country[i], 'added. Total time: ')
      
      tic(C)
      windthrow_path = paste0("data/intermed/windthrow_", Country[i], ".shp" )
      windthrow_sf = st_read(windthrow_path)
      windthrow_map = windthrow_map + geom_sf(data = subset(windthrow_sf, year == Year[j]), 
                                              color = "#D55E00")
      toc(log = TRUE)
    }
 
    windthrow_map = windthrow_map + ggtitle(title)
    
    tic("plot")
      target_path = paste0("figures/all_storms_", mode, "/windthrow_storms_", threshhold, "_", Year[j], ".png" )
    
      png(target_path)
      print(windthrow_map)
      dev.off()
    toc(log = TRUE)
    
    toc(log = TRUE)
}








