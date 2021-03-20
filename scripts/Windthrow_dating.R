
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
      print("layer classified")
      storm_layer = classify_with_threshold(storm_layer, 20)
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
    #    group_by(layer) %>% summarize(geometry = st_union(geometry))
    # z = y %>% 
    return(z)
  } else {
    print("loop entered, x < 1")
    y = x
    y[[1]] = as.numeric(str_sub(as.character(names(x)), 2, 10))
    y = rasterToPolygons(y, dissolve = TRUE)
    return(y)
  }
}


write_polygon_storms = function(Year, Threshold) {
  stack = assign_storms_to_pixel(Year, Threshold)
  polygon = create_polygons_of_maxvalues(stack)
  print("saving starts...")
  path = paste0("data/WISC/wisc_storm_per_pixel/max_storm_per_pixel_", Year, ".shp")
  if (class(polygon)[1] == "SpatialPolygonsDataFrame") {
    raster::shapefile(polygon, path)
  } else {
    sf::st_write(polygon, path)
  }
}

# EXECUTION

for (a in c(1987:2017)) {
  write_polygon_storms(a, 20)
}


