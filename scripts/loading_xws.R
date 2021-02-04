#downloading xrs catalogues netcdf fiels of storm foot prints

# wd
wd = "C:/Users/Lucia/Documents/Diss/Projects/Storm_Attribution"
setwd(wd)

#packages
library(sf)

#load long-lat grid
long_lat_grid = read.csv("data/winter_storms/eur_grid_lon_lat.txt")






#load data
track = read.csv("data/Kyrill_track.csv", header = FALSE)
kyrill_track = read_sf("data/Kyrill_track.csv")

kyrill_nc = read_ncdf("data/Kyrill_rawFoot.nc")

# FOOTPRINT netCDF AS RASTER
library(terra)
library(raster)
library(stars)
library(sf)

kyrill_terra = terra::rast("data/Kyrill_rawFoot.nc")
kyrill_raster = raster::raster("data/Kyrill_rawFoot.nc")
kyrill_stars = stars::read_ncdf("data/Kyrill_rawFoot.nc")

europe = st_read("data/Europe_coastline.shp")

# RASTER DATA: DOES NOT WORK SO FAR
# File is unreasonable huge and does not plot
grid_locations = read.csv("data/grid_locations.csv")
kyrill_foot = read.csv("data/Kyrill_rawFoot.csv", col.names = c('Grid.number', 'windspeed')) %>%
  right_join(., grid_locations)
kyrill_foot$Grid.number = NULL
names(kyrill_foot) = c('Windspeed', 'Longitude', 'Latitude') # fix spelling mistake
kyrill_foot = kyrill_foot[, c('Longitude', 'Latitude', 'Windspeed')] # for stars ro regognize dimensions
kyrill_raster = st_as_stars(kyrill_foot, dimensions = c('Londitude', 'Latitude'))

kyrill_raster = rasterFromXYZ(kyrill_foot)

# VECTOR DATA: TRACK




