wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution"
setwd(wd)
getwd()


library(raster)
library(sf)
library(tictoc)

# load wind data
u10 = raster::brick("data/era5/1990_france.nc", varname = "u10")
v10 = raster::brick("data/era5/1990_france.nc", varname = "v10")

# calculate actual wind speed from u and v elements
# formulas from https://www.eol.ucar.edu/content/wind-direction-quick-reference

#windspeed
w10 = sqrt(u10^2 + v10^2)
raster::writeRaster(w10, "data/era5/era5_fi_wind_w.nc")
  
  #wnd direction
  DperR = (180/pi)
  d10 = 270 - (raster::atan2(v10, u10)*DperR)
  raster::writeRaster(d10, "data/era5/era5_fi_wind_d.nc")



# filter to only include extreme values 
threshhold_storm = .99
quantile_speed = raster::quantile(w10$layer.1, probs = threshhold_storm)
max_speed = max(raster::maxValue(w10))

reclass_matrix = as.matrix(c(0, quantile_speed, 0,
                             quantile_speed, max_speed , 1 ))


s10 = raster::reclassify(w10, reclass_matrix)
raster::writeRaster(s10, "data/era5/era5_fi_wind_s.nc")

s10_reduced = s10


delete_layers_with_zeros_only = function(x) {
  print(getwd())
  zero_layers = c()
  reduced_brick = x
  print(class(reduced_brick))
  for (i in c(1:raster::nlayers(x))) { 
    if (raster::cellStats(reduced_brick[[i]], stat = 'sum') < 5) { #less than five pixels contain 1s
      print(dim(x))
      print(paste0("Layer", i, "contains only zeros!"))
      zero_layers = append(zero_layers, i)
    }
  }
  
  print("Succesfully looped through all layers, start deleting layers")
  reduced_brick = raster::dropLayer(reduced_brick, zero_layers)
  
  raster::writeRaster(reduced_brick, "data/era5/s10_reduced_larger_five.nc", format="CDF")
  print(class(reduced_brick))
}

delete_layers_with_zeros_only(s10_reduced)

test = s10
for (i in c(15087:16000)) { 
  if (raster::cellStats(test[[i]], stat = 'sum')== 0) {
    print(dim(test))
    print(paste0("Layer", i, "contains only zeros!"))
    test = raster::dropLayer(test, i)
  }
}


# layer all rasters on top of each other to see where these are (or a more elegant version of the same)
# alternative: identify time steps where extreme values overlap with windthrow fields
windthrow_fi_11 = st_read("data/intermed/windthrow_finland.shp") %>%
  subset(year == 2011)



#todo: make this work, might be faster
windspeed_from_components = function(u,v) {
  sqrt(u^2 +  v^2)
}

wind_direction_from_components = function(u,v) {
  270 - (atan2(v, u)*DperR)
}

tic('raster::overlay')
  w100 = raster::overlay(u10, v10, fun=windspeed_from_components(u10, v10))
toc(log = TRUE)

tic('raster::overlay')
  d100 = raster::overlay(x = u10, y = v10, fun=wind_direction_from_components(u,v))
toc(log = TRUE)

