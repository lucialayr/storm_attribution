library(raster)

# load wind data
u10 = raster::brick("data/era5/vivian.nc", varname = "u10")
v10 = raster::brick("data/era5/vivian.nc", varname = "v10")

# calculate actual wind speed from u and v elements
# formulas from https://www.eol.ucar.edu/content/wind-direction-quick-reference
writeRaster(w10[[1]], "test.nc", overwrite = TRUE)
#windspeed
w10 = sqrt(u10^2 + v10^2)

hist(maxValue(w10))

#wnd direction
DperR = (180/pi)
d10 = 270 - (raster::atan2(v10, u10)*DperR)

