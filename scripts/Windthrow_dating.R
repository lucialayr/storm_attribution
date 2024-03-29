library(raster)
library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(benford)
library(rgeos)
library(ggplot2)
library(ggnewscale)
library(cowplot)
library(exactextractr)

wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)

# DATA PREPARATION

dates = read.csv("data/WISC/WISC_proj/date.txt", header = FALSE, col.names = "date")
# create table with years
dates = dates %>% 
  dplyr::mutate(year = as.numeric(str_sub(as.character(date), 1, 4)),
                month = as.numeric(str_sub(as.character(date), 5, 6)),
                hyear = ifelse(month < 5, year,year + 1))


# FUNCTIONS

load_storm_in_laea = function (Date, debug = FALSE) {
  storm_path = paste0("data/WISC/WISC_proj/W", Date, "_proj.nc")
  if (file.exists(storm_path) == TRUE) {
    storm_file = raster::raster(storm_path) %>%
      raster::projectRaster(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    names(storm_file) = Date
    storm_file = setZ(storm_file, Date)
    print(getZ(storm_file))
    return(storm_file)
  }
  
}


classify_with_threshold = function(storm, x) {
  reclass_mx = as.matrix(c(0, x, NA))
  raster_rc = raster::reclassify(storm, reclass_mx)
}

assign_storms_to_pixel = function(Year, Treshold) {
  dates = read.csv("data/WISC/WISC_proj/date.txt", header = FALSE, col.names = "date") %>% 
    dplyr::mutate(year = as.numeric(str_sub(as.character(date), 1, 4)),
                  month = as.numeric(str_sub(as.character(date), 5, 6)),
                  hyear = ifelse(month < 5, year,year + 1))
  
  for ( i in c(1:length(dates$date))) {
    if (dates$hyear[i] == Year) {
      print(dates$date[i])
      storm_layer = load_storm_in_laea(dates$date[i])
      print(storm_layer)
      print("layer loaded")
      storm_layer = classify_with_threshold(storm_layer, 20)
      print("layer classified")
      names(storm_layer) = dates$date[i]
      if (exists("storm_stack")==FALSE) {
        storm_stack = raster::stack(storm_layer)
      } else {
        storm_stack = addLayer(storm_stack, storm_layer)
      }
      print(dim(storm_stack))
     
    }
  }
  return(storm_stack)
  
}


create_polygons_of_maxvalues = function(x) {
  date_vector = as.numeric(str_sub(as.character(names(x)), 2, 10))
  if (dim(x)[3] > 1) { # which.max fails is there is only one layer
    print("loop entered, x > 1")
    y = which.max(x)
    y = rasterToPolygons(y, dissolve = TRUE)
    print("converted to polygon")
    y = st_as_sf(y)  %>% mutate(storm_date = date_vector[y$layer])
  } else {
    print("loop entered, x < 1")
    reclass_m = c(0, maxValue(x)+1, as.numeric(str_sub(as.character(names(x)), 2, 10)))
    y = raster::reclassify(x, reclass_m)
    y = rasterToPolygons(y, dissolve = TRUE)
  }
  return(y)
}


write_polygon_storms = function(Year, Threshold) {
  stack = assign_storms_to_pixel(Year, Threshold)
  polygon = create_polygons_of_maxvalues(stack)
  print("saving starts...")
  path = paste0("data/WISC/wisc_storm_per_pixel/", Year, ".shp")
  if (class(polygon)[1] == "SpatialPolygonsDataFrame") {
    raster::shapefile(polygon, path)
  } else {
    sf::st_write(polygon, path, delete_dsn = TRUE)
  }
}

assign_storm_to_disturbance = function(Country) {
  #use table with storm dates to create date vector
  dates = read.csv("data/WISC/WISC_proj/date.txt", header = FALSE, col.names = "date")
  dates = dates %>% 
    dplyr::mutate(year = as.numeric(str_sub(as.character(date), 1, 4)),
                  month = as.numeric(str_sub(as.character(date), 5, 6)),
                  hyear = ifelse(month < 5, year,year + 1))
  year_list = unique(dates$hyear)
  
  # load patches
  path_disturbance = paste0("data/intermed/windthrow_", Country, ".shp")
  disturbance_poly = st_read(path_disturbance)
  print("disturbance loaded")
  
  for (y in c(min(year_list):max(year_list))) {
    disturbance_poly_year = disturbance_poly[disturbance_poly$year == y,]
  
    
    if (dim(disturbance_poly_year)[1] > 0) {
      path_storm = paste0("data/WISC/wisc_storm_per_pixel/", y, ".shp") #make sure this path is correct
      if (file.exists(path_storm) == TRUE) {
        storm_poly = st_read(path_storm)
        st_crs(storm_poly) = st_crs(disturbance_poly)
        print(names(storm_poly))
        if ( length(names(storm_poly)) == 3 ) {
          storm_poly$layer = NULL
        } else {
          names(storm_poly) = c("storm_date", "geometry")
        }
        print(names(storm_poly))
        disturbance_poly_year_dated = st_join(disturbance_poly_year, storm_poly)
        
        print(names(disturbance_poly_year_dated))
        if (exists("disturbance_poly_dated") == TRUE) {
          print("start union")
          disturbance_poly_dated = rbind(disturbance_poly_dated, disturbance_poly_year_dated)
        } else {
          print("initiate new file")
          disturbance_poly_dated = disturbance_poly_year_dated
        }
        print(paste0(y, "dated"))
      }
      
    }
    
  }
  return(disturbance_poly_dated)
}

mosaic_map_of_max_windspeed = function(Year, Threshold) {
  x = assign_storms_to_pixel(Year, Treshold)
  date_vector = as.numeric(str_sub(as.character(names(x)), 2, 10))
  print(date_vector)
  if (dim(x)[3] > 1) { 
    print("more than one storm")
    path_poly = paste0("data/WISC/wisc_storm_per_pixel/", Year, ".shp")
    y_poly = st_read(path_poly)
    for ( i in c(1:length(date_vector))) {
      mask = y_poly[y_poly$layer == i,]
      print("mask created")
      wind_speed = raster::mask(x[[i]], mask)
      print("masked")
      if (exists("wind_mosaic") == TRUE){
        wind_mosaic = stack(wind_mosaic, wind_speed)
      } else {
      wind_mosaic = wind_speed
      }
    }
  } else {
    print("loop entered, x < 1")
    wind_mosaic = x
  }
  names(wind_mosaic) = date_vector
  wind_mosaic = setZ(wind_mosaic, date_vector)
  return(wind_mosaic)
}





plot_gg_wind_speed = function(Year) { 
  
  print(paste0(Year, " starting, storm"))
  base_map = st_read("data/europe_countries.shp", quiet = TRUE)
  europe_outline = st_union(base_map)
  
  path_raster = paste0("data/WISC/mosaic/mosaic_",Year, ".grd")
  mosaic = raster::stack(path_raster) %>% mask(base_map)
  
  path_poly = paste0("data/WISC/wisc_storm_per_pixel/", Year , ".shp")
  outline = st_read(path_poly, quiet = TRUE) 
  
  st_crs(outline) = st_crs(base_map)
  outline =  st_intersection(outline, st_union(base_map))
  
  date_vector = as.numeric(str_sub(as.character(names(mosaic)), 2, 10))
  plot = ggplot() + 
    geom_sf(data = europe_outline, fill = "dimgrey") + theme_bw() +
    geom_sf(data = outline, fill = NA)
  colors_light = c("#14F57D", "#FFD500", "#65FC24", "#2347FD", "#FF5500", "#14F57D", "#FFD500", "#65FC24", "#2347FD", "#FF5500")
  colors_low = c("#4DB57D", "#BD9756", "#84C668", "#5667BD", "#DF8558", "#4DB57D", "#BD9756", "#84C668", "#5667BD", "#DF8558")
  
  print("set up done")
  
  for (i in c(1:length(date_vector))) {
    # data preparation
    pts = rasterToPoints(mosaic[[i]], spatial = TRUE)
    df  = data.frame(pts)
    #print(head(df))
    #print(names(mosaic)[i])
    plot = plot + 
      geom_tile(data = df , aes_string(x = "x", y = "y", alpha = names(mosaic)[i]), color = NA, fill = colors_low[i]) +
      scale_alpha(limits = c(20, 50), range= c(0, .9), na.value = .9)+
      ggnewscale::new_scale("alpha")+
      geom_tile(data = df , aes_string(x = "x", y = "y", alpha = names(mosaic)[i]), color = NA, fill = colors_light[i]) +
      scale_alpha(limits = c(40,  80), range= c(0, 1), na.value = 0) + ggnewscale::new_scale("alpha")
  }
  plot = plot + theme(legend.position = "none")
  print(date_vector)
  return(plot)
}

rasterize_disturbance = function(Year) {
  print("...")
  base_map = st_read("data/europe_countries.shp", quiet = TRUE)
  
  path_raster = paste0("data/WISC/mosaic/mosaic_",Year, ".grd")
  mosaic = raster::stack(path_raster) %>% mask(base_map)
  date_vector = as.numeric(str_sub(as.character(names(mosaic)), 2, 10))
  
  Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
                 "bulgaria", "croatia", "czechia", "denmark", "estonia",
                 "finland", "france", "germany", "greece", "hungary", 
                 "ireland", "italy", "latvia", "lithuania", "macedonia", 
                 "moldova", "montenegro", "netherlands", "norway", "poland", 
                 "portugal", "romania", "serbia", "slovakia", "slovenia", 
                 "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")
  
  print("set-up done")
  
  for (i in c(1:length(date_vector))) {
    print(paste0("Starting ", date_vector[i], "..."))
    
    for (c in c(1:length(Country))) {
      print(paste0("Starting ", Country[c]))
      disturbance_path = paste0("data/dated_patches/windthrow_", Country[c], "_dated.shp" )
      disturbance_poly = st_read(disturbance_path, quiet = TRUE)
      
      if (date_vector[i] %in% unique(disturbance_poly$storm_date) == TRUE) {
        disturbance_storm = disturbance_poly[disturbance_poly$storm_date == date_vector[i],] %>%
          filter(!st_is_empty(.)) %>% st_union()
        
        template = mosaic[[i]]
        
        country_rast = exactextractr::coverage_fraction(template, disturbance_storm)
        print(paste0(Country[c], " rasterized"))
      
      if (exists("raster_disturbance") == TRUE) {
          raster_disturbance = raster_disturbance + country_rast[[1]]
        } else {
          raster_disturbance = country_rast[[1]]
        }
      } 
    }
    
    if (exists("raster_disturbance") == TRUE ) {
      names(raster_disturbance) = date_vector[i]
      
      if(exists("raster_stack") == TRUE) {
        raster_stack = stack(raster_stack, raster_disturbance)
      } else {
        raster_stack = raster_disturbance
      }
      rm(raster_disturbance)
    }
    }
  return(raster_stack)
}

plot_rasterized_disturbance = function(Year) { #not tested
  
  print(paste0(Year, " starting, disturbance"))
  path_poly = paste0("data/WISC/wisc_storm_per_pixel/", Year , ".shp")
  
  if(file.exists(path_poly)) {
    base_map = st_read("data/europe_countries.shp", quiet = TRUE)
    
    outline = st_read(path_poly, quiet = TRUE) 
    st_crs(outline) = st_crs(base_map)
    outline =  st_intersection(outline, st_union(base_map))
    
    path_disturbance = paste0("data/rasterize_windthrow_on_storms/fine_resolv/disturbance_", Year)
    disturbance = raster::stack(path_disturbance) %>% 
      raster::crop(base_map) %>%
      raster::mask(base_map)
    
    date_vector = as.numeric(str_sub(as.character(names(disturbance)), 2, 10))
    
    colors_light = c("#14F57D", "#FFD500", "#65FC24", "#2347FD", "#FF5500", "#14F57D", "#FFD500", "#65FC24", "#2347FD", "#FF5500")
    colors_low = c("#4DB57D", "#BD9756", "#84C668", "#5667BD", "#DF8558", "#4DB57D", "#BD9756", "#84C668", "#5667BD", "#DF8558")
    
    plot = ggplot() + geom_sf(data = st_union(base_map), fill = "dimgrey")  +
      geom_sf(data = outline, fill = NA) +
      theme_bw() + theme(legend.position = "none")
      
    print("set up done")
    for (i in c(1:length(date_vector))) {
      pts = rasterToPoints(disturbance[[i]], spatial = TRUE)
      df  = data.frame(pts)
      #print(head(df))
      plot = plot + 
        geom_tile(data = df , aes_string(x = "x", y = "y", alpha = paste0("X", date_vector[i])),  color = NA, fill = colors_low[i]) +
        scale_alpha(limits = c(0, 0.05), range= c(0, .9), na.value = .9) +
        ggnewscale::new_scale("alpha")+
        geom_tile(data = df , aes_string(x = "x", y = "y", alpha = paste0("X", date_vector[i])), color = NA, fill = colors_light[i]) +
        scale_alpha(limits = c(0.05,  1), range= c(0, 1), na.value = 0) + ggnewscale::new_scale("alpha")
        }
    print(date_vector)
    return(plot)
  }
}
  
 
plot_storm_disturbance = function(Year) { 
  A = plot_gg_wind_speed(Year)
  B = plot_rasterized_disturbance(Year)
  
  plot_row = plot_grid(A, B)
  
  title = ggdraw() + 
    draw_label(Year, x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  plot_grid(title, plot_row, 
            ncol = 1, 
            rel_heights = c(0.05, 1)) #https://wilkelab.org/cowplot/articles/plot_grid.html
}



# EXECUTION

for (a in c(2014:2017)) {
  write_polygon_storms(a, 20)
  
}

for (a in c(2012:2016)) {
  test_path = paste0("data/WISC/wisc_storm_per_pixel/", a , ".shp")
  if(file.exists(test_path)) {
    mosaic = mosaic_map_of_max_windspeed(a, 20)
    path = paste0("data/WISC/mosaic/mosaic_",a)
    writeRaster(mosaic, path, overwrite = TRUE)
  }
}

for (a in c(2012:2016)) {
  test_path = paste0("data/WISC/wisc_storm_per_pixel/", a , ".shp")
  if(file.exists(test_path)) {
    raster = rasterize_disturbance(a)
    path = paste0("data/rasterize_windthrow_on_storms/fine_resolv/disturbance_",a)
    writeRaster(raster, path, overwrite = TRUE)
  }
}

for (a in c(1999:2000)) {
  test_path = paste0("data/WISC/wisc_storm_per_pixel/", a , ".shp")
  if(file.exists(test_path)) {
    plot = plot_storm_disturbance(a)
    path = paste0("figures/storm_vs_dist/storm_dist_",a, ".pdf")
    
    pdf(path)
    print(plot)
    dev.off()
  }
}




Country = list("albania", "austria", "belarus", "belgium", "bosniaherzegovina",
               "bulgaria", "croatia", "czechia", "denmark", "estonia",
               "finland", "france", "germany", "greece", "hungary", 
               "ireland", "italy", "latvia", "lithuania", "macedonia", 
               "moldova", "montenegro", "netherlands", "norway", "poland", 
               "portugal", "romania", "serbia", "slovakia", "slovenia", 
               "spain", "sweden", "switzerland", "ukraine", "unitedkingdom")

for (c in c(1:length(Country))) {
  dated_patch = assign_storm_to_disturbance(Country[c])
  target_path = paste0("data/dated_patches/windthrow_",Country[c], "_dated.shp" )
  st_write(dated_patch, target_path, delete_dsn = TRUE)
}



