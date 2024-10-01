# ---
# title: "Create a netcdf"
# date: "2024-09-09"
# Source: https://pjbartlein.github.io/REarthSysSci/netCDF.html#introduction
# ---
library(raster)

# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

r <- raster("data/output/mynetcdf.nc",  varname = "tmp")
proj4string(r)=CRS("+proj=longlat +datum=WGS84 +no_defs")
r
# calculate and save the min and max values of the raster to the raster object
r <- setMinMax(r)

#select first 12th time
r12 <- raster("data/output/mynetcdf.nc",  varname = "tmp", band="1")
plot(r12)

# subset 
e <- c(100, 120, -10, 10)

r_crop <- crop(r12, e)
plot(r_crop)

