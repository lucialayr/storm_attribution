library(raster)
library(ggplot2)

wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution/"
setwd(wd)
# load wind data
u10 = raster::brick("data/era5/vivian.nc", varname = "u10")
v10 = raster::brick("data/era5/vivian.nc", varname = "v10")

# calculate actual wind speed from u and v elements
# formulas from https://www.eol.ucar.edu/content/wind-direction-quick-reference
writeRaster(w10[[1]], "test.nc", overwrite = TRUE)
#windspeed
w10 = sqrt(u10^2 + v10^2)

hist(maxValue(w10))
df = data.frame(maxValue(w10))


europe_land = raster::shapefile("data/europe_countries.shp") 
vivian_storm = raster::raster("data/cyclones/00_biasMean_proj_Vivian.tif") %>%
  raster::mask(europe_land)
df_vivian = raster::as.data.frame(vivian_storm$X00_biasMean_proj_Vivian, xy = TRUE)
names(df_vivian) = c("x", "y", "wind_speed_V")

(ggplot() + geom_histogram(data = df_vivian, aes(x = wind_speed_V), alpha = .5, fill = "#3CB371"))

(ggplot() + geom_histogram(data = df, aes(x =  maxValue.w10.), alpha = .5, fill = "#4682B4") +
    xlim(c(0,45)))




