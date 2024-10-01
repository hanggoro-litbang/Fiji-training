# ---
# title: "read netcdf data"
# date: "2024-09-09"
# Source: https://randroll.wordpress.com/2018/08/10/plotting-netcdf-files-with-r/
# ---

# install library (only runs once)
#install.packages(c("ncdf4","CFtime","lattice","RColorBrewer"))

library(ncdf4)        # package to work with netCDF files
library(fields)         # this package has image.plot function
library(viridis)        # this package has nice colormap palettes

# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

# Opening the netCDF
inp <- nc_open("data/input/wrfout_d01_2005-08-29_00_00_00")
head(inp)
#Extracting the variables we want
xlon <- ncvar_get(inp, varid = 'XLONG')
xlat <-  ncvar_get(inp, varid = 'XLAT')
sst <- ncvar_get(inp, varid = "SST")
# Close the netCDF files
nc_close(inp)

# Retrieving only the sequence of lat and lon from xlat and xlong matrix
lon <- xlon[, 1]
lat <-  xlat[1, ]

# Here is the plot:
image.plot(lon, lat, sst,
           main = "Skin Temperature at 2005-08-29_00_00_00",
           xlab = "Longitude",
           ylab = "Latitude",
           legend.lab = "Kelvin",
           legend.line = 2.5,
           col = rev(magma(200)))  # To get the higher emission values darker we invert it
mtext("America")  # To put a sub-title
