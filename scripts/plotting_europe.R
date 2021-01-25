# packages
library(sf)
library(ggplot2)
library(tictoc)

# wd
wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution"
setwd(wd)

europe_sf = st_read("data/europe_countries.shp")
base_map = ggplot() + geom_sf(data = europe_sf)

Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
            "bulgaria", "croatia", "czechia", "denmark", "estonia",
            "finland", "france", "germany", "greece", "hungary", 
            "ireland", "italy", "latvia", "lithuania", "macedonia", 
            "moldova", "montenegro", "netherlands", "norway", "poland", 
            "portugal", "romania", "serbia", "slovakia", "slovenia", 
            "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")

Year = seq(from = 1986, to = 2016, by = 1)

for (j in c(1:31)) {
  windthrow_map = base_map
  print(paste(Year[j], 'starting...'))
  Y = paste(Year[j], 'added. Total time: ')
  
  tic(Y)
  
    for (i in c(1:35)){
      print(paste0(Country[i], ' (', Year[j], ') starting...'))
      C = paste(Country[i], 'added. Total time: ')
      
      tic(C)
      windthrow_path = paste0("data/intermed/windthrow_", Country[i], ".shp" )
      windthrow_sf = st_read(windthrow_path)
      windthrow_map = windthrow_map + geom_sf(data = subset(windthrow_sf, year == Year[j]), color = "#D55E00")
      toc(log = TRUE)
    }
 
    windthrow_map = windthrow_map + ggtitle(Year[j])
    
    tic("plot")
      target_path = paste0("figures/years/windthrow_", Year[j], ".png" )
    
      png(target_path)
      print(windthrow_map)
      dev.off()
    toc(log = TRUE)
    
    toc(log = TRUE)
}








