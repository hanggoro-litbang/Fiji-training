# ---
# title: "read netcdf data"
# date: "2024-09-09"
# Source: https://pjbartlein.github.io/REarthSysSci/netCDF.html#introduction
# ---

# install library (only runs once)
# install.packages(c("ncdf4","CFtime","raster"))

# load library
library(ncdf4)
library(CFtime)
library(raster)
library(RColorBrewer)
library(lattice)


# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

# read data
ncin <- nc_open("data/input/cru10min30_tmp.nc")
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
tunits

nt <- dim(time)
nt

# get temperature
dname<-"tmp"
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

ls()
# nc_close(ncin)

#3D to 2D and plot
# decode time
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf

timestamps <- CFtimestamp(cf) # get character-string times
timestamps

class(timestamps)

time_cf <- CFparse(cf, timestamps) # parse the string into date components
time_cf

class(time_cf)

# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1])))

# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array[,,m]

# quick map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


#' create a bounding box to work with a subset
LonIdx <- which( ncin$dim$lon$vals > 90 & ncin$dim$lon$vals < 140)
LatIdx <- which( ncin$dim$lat$vals > -15 & ncin$dim$lat$vals < 15)

ncin_subset <- ncvar_get(ncin, varid = dname, start = c(LonIdx[1], LatIdx[1],1), 
                         count = c(length(LonIdx), length(LatIdx),-1))
dim(ncin_subset)
# replace netCDF fill values with NA's
ncin_subset[ncin_subset==fillvalue$value] <- NA

length(na.omit(as.vector(ncin_subset[,,1])))

# get a single slice or layer (January)
m <- 2
ncin_sub_slice <- ncin_subset[,,m]

lonsub <- lon[LonIdx]
latsub <- lat[LatIdx]

# quick map
image(lonsub,latsub,ncin_sub_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lonsub, lat=latsub)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(ncin_sub_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# select coordinate
jkt_lat <- -6.2
jkt_lon <- 106.81

# extract date with alternate way
ncin$dim$time$units
ncin_dates <- as.Date(ncin$dim$time$vals, origin = '1900-01-01')

# get values at location lonlat
ncin_out <- ncvar_get(ncin, varid = dname,
                      start= c(which.min(abs(ncin$dim$lon$vals - jkt_lon)), # look for closest long
                               which.min(abs(ncin$dim$lat$vals - jkt_lat)),  # look for closest lat
                               1),
                      count = c(1,1,-1)) #count '-1' means 'all values along that dimension'
ncin_out

datafinal <- data.frame(dates= ncin_dates, temp = ncin_out)

# set path and filename
csvpath <- "/Users/wido/Documents/WORK/2024/Training-Fiji/data/output/"
csvname <- "jkt_cru.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(datafinal),csvfile, row.names=FALSE, sep=",")

plot(datafinal, type="l")

plot(datafinal$dates,datafinal$temp,type='l',xlab='Month',ylab='Temperature (C)')

# create netcdf file
# path and file name, set dname
ncpath <- "/Users/wido/Documents/WORK/2024/Training-Fiji/data/output/"
ncname <- "mynetcdf"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

xvals <- seq(90, 140, 5)
yvals <- seq(-15, 15, 5) 
nx <- length(xvals)
ny <- length(yvals)
lon <- ncdim_def("lon", "degrees_east", xvals)
lat <- ncdim_def("lat", "degrees_north", yvals)
time <- ncdim_def("time","months", 1:12, unlim=TRUE)
nt <- time$len

mv <- -999 #missing value to use
var_temp <- ncvar_def(dname, "celsius", list(lon, lat, time), 
                      longname="CRU_Global_1961-1990_Mean_Monthly_Surface_Temperature_Climatology", mv) 

ncnew <- nc_create(ncfname, list(var_temp))

print(paste("The file has", ncnew$nvars,"variables"))
#[1] "The file has 1 variables"
print(paste("The file has", ncnew$ndim,"dimensions"))
#[1] "The file has 3 dimensions"

# Some fake dataset based on latitude, to check whether the data are
# written in the correct order

v <- sample(-20:20, nx*ny*nt, replace = TRUE)

# Reshape the vector 'v' into a 3-dimensional array with dimensions nx ny nt
dim(v) <- c(nx, ny, nt)


# Add 5 random -999 value to check whether missing values are correctly
# written
v[sample(1:(nx*ny*nt), 20, replace = FALSE)] <- -999
ncvar_put(ncnew, var_temp, v, start=c(1,1,1), count=c(nx,ny,nt))

# Don't forget to close the file
nc_close(ncnew)

# open the created data
mync <- nc_open('data/output/mynetcdf.nc')
lon <- ncvar_get(mync,"lon")
nlon <- dim(lon)

lat <- ncvar_get(mync,"lat")
nlat <- dim(lat)

# get time
time <- ncvar_get(mync,"time")
time

mync_tmp <- ncvar_get(mync, dname)

image(lon,lat,mync_tmp[,,2])
