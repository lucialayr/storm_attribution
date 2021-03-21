library(raster)
library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(benford)
library(rgeos)

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
  storm_file = raster::raster(storm_path) %>%
    raster::projectRaster(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  names(storm_file) = Date
  storm_file = setZ(storm_file, Date)
  print(getZ(storm_file))
  return(storm_file)
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
      return(storm_stack)
    }
  }
  
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
    sf::st_write(polygon, path)
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
      path_storm = paste0("data/WISC/wisc_storm_per_pixel/max_storm_per_pixel_", y, ".shp")
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

# EXECUTION

for (a in c(1985:2017)) {
  write_polygon_storms(a, 20)
  
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
  st_write(dated_patch, target_path)
}
