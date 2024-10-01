#Example of read_forecast function
#Load required libraries
library('harpIO')
library('harpVis')
library('dplyr')
library('tibble')

file_path="/Users/wido/Documents/WORK/2024/harp/data"

# setting netcdf options based on netcdf file attributes (ncdump -h filename.nc)
my_opts <- netcdf_opts(
  param_find = list("T2M","Q2","HGT","RAINNC","RAINC"),
  proj4 = "wrf",
  x_dim = "west_east",
  y_dim = "south_north",
  lon_var = "XLONG",
  lat_var = "XLAT",
  #z_var = ,
  dx = NULL,
  dy = NULL,
  time_var = "XTIME",
  force_param_name = TRUE
)

#read and display model data
temperature <- read_grid(
  file_name = paste0(file_path,"/netcdf/wrf/wrfout_d01_2024-01-08_00_00_00.nc"),
  parameter = "T2M", 
  file_format_opts = my_opts,
  file_format = "netcdf",
  data_frame  = FALSE
)

class(temperature)
length(temperature)
plot_field(temperature[[25]],
           breaks     = seq(280, 310, 2))

# read forecast data and convert to sqlite
station_id <- read.csv("stationlist_ID.csv")
template <- "{fcst_model}_d01_{YYYY}-{MM}-{DD}_{HH}_00_00.nc" #show_file_templates()
sql_folder <- "/Users/wido/Documents/WORK/2024/harp/data/SQlite_tables/FCtables"

read_forecast(
  dttm           = seq_dttm(20240108, 20240111, "12h"),
  fcst_model     = "wrfout",
  parameter      = c("T2M","Q2","HGT","RAINNC","RAINC"), #show_param_defs("grib"), use same names as in OBS table
  file_format = "netcdf",
  file_format_opts = my_opts, # or using 'netcdf_opts("wrf")' for wrf netcdf
  lead_time      = seq(0, 72, 3),
  transformation = "interpolate",
  transformation_opts = interpolate_opts(
    stations = station_id,
    method = "bilinear",
    clim_param = "HGT",
    correct_t2m = FALSE  
  ),
  file_path      = paste0(file_path,"/netcdf/wrf"),
  file_template  = template,
  output_file_opts = sqlite_opts(
    path = sql_folder,
    template = "fctable_det", #show_file_templates(5)
    index_cols = c("fcst_dttm", "lead_time", "SID"),
    remove_model_elev = TRUE), 
  return_data    = TRUE #usually FALSE
)

#read from sqlite
wrf_point <- read_point_forecast(
  dttm             = seq_dttm(20240108, 20240111, "12h"),
  fcst_model       = c("wrfout"),
  fcst_type        = "det", 
  parameter        = "T2M",
  lead_time        = seq(0,25,3),
  by               = "3h",
  file_path        = paste0(file_path,"/SQlite_tables/FCtables"),
  file_template    = "fctable_det",
  force_param_name = TRUE,
  drop_any_na      = FALSE
)

# plot
station_96001 <- filter(wrf_point, SID == "96001" & lead_time == 0 & fcst_cycle == "00")
station_97980 <- filter(wrf_point, SID == "97980" & lead_time == 0 & fcst_cycle == "00")
plot(station_96001$fcst_dttm, station_96001$fcst, type = "l",
     ylim = c(295,308),
     main = "Temperature at Station 96001 and 97980",
     xlab = "date",
     ylab = "Temp (°K)")
lines(station_97980$fcst_dttm, station_97980$fcst, col="red", lty=2)
legend("bottomright", legend=c("96001", "97980"),
       col=c("black", "red"), lty=1:2, cex=0.8)
head(station_96001)

