#Example of read_obs function
#Load required libraries
library('harpIO')
library('dplyr')
library('tibble')

##Read_csv_obs function is needed for read_obs function
read_csv_obs <- function(file_name, dttm, parameter = NULL, ...) {
  
  # read the csv data using read.csv
  obs_data <- read.csv(file_name)
  
  # add a column for the date-time
  obs_data <- dplyr::mutate(
    obs_data, valid_dttm = dttm, .before = dplyr::everything()
  )
  
  # Change column names to names harp knows about using psub()
  # We wrap in suppressWarnings as we don't want it to warn about 
  # substitutions that don't exist in case not all files contain all the 
  # expected columnn names. 
  #use a parameter naming convention that harp knows 
  #show_param_defs()
  #harp_parameters <- get("harp_params")
  colnames(obs_data) <- suppressWarnings(psub(
    colnames(obs_data),
    c("wmo", "elevation", "TA_PT1M_AVG","TD_PT1M_AVG", "WS_PT10M_AVG", "PRA_PT1H_ACC", "PRA_PT3H_ACC", "PRA_PT6H_ACC", "PRA_PT12H_ACC", "RH_PT1M_AVG", "PA_PT1M_AVG"),
    c("SID", "elev", "T2m","Td2m", "S10m", "AccPcp1h", "AccPcp3h", "AccPcp6h", "AccPcp12h", "RH2m", "Pressure")
  ))
  
  # Only return parameters that have been asked for
  if (length(parameter) > 0 && !all(is.na(parameter))) {
    parameter <- stats::na.omit(parameter)
    obs_data <- dplyr::select(
      obs_data, 
      dplyr::any_of(c("valid_dttm", "SID", "lat", "lon", "elev", parameter))
    )
  }
  
  # Make a data frame for parameter units
  obs_units <- tibble::tribble(
    ~parameter, ~accum_hours, ~units,
    "T2m"       , 0 , "K",
    "Td2m"      , 0 , "K",
    "S10m"      , 0 , "m/s",
    "AccPcp1h"  , 1 , "kg/m^2",
    "AccPcp3h"  , 3 , "kg/m^2",
    "AccPcp6h"  , 6 , "kg/m^2",
    "AccPcp12h" , 12, "kg/m^2",
    "RH2m"      , 0 , "fraction",
    "Pmsl"      , 0 , "hPa",
  )
  
  # Filter the obs_units data frame to only those parameters that are in the 
  # data
  obs_units <- dplyr::filter(obs_units, .data$parameter %in% colnames(obs_data))
  
  # return the data as a named list
  list(synop = obs_data, synop_params = obs_units)
}

#Modify following parameters if needed
starttime = 2024010800
endtime = 2024011123
timestep <- "1h"
file_path <- "~/Documents/WORK/2024/harp/data/new_csv"
sql_folder <- "~/Documents/WORK/2024/harp/data/SQlite_tables/OBS"

# read csv obs data and convert to sqlite format
# convert the observations to SQLite format, which is much quicker to access and 
# filter to what we need using read_point_obs(). 
# This process is done using read_obs() and setting output_format_opts.
read_obs(
  dttm               = seq_dttm(starttime, endtime, timestep), 
  parameter          = NULL, 
  file_format        = "csv_obs", 
  file_path          = file_path,
  file_template      = "obs_{YYYY}{MM}{DD}{HH}.csv",
  output_format      = "obstable", 
  output_format_opts = obstable_opts(path = sql_folder, template = "obstable")
)

# Read SQLITE database
station_all <- read_point_obs(
  seq_dttm(2024010800, 2024011123), "T2m", 
  obs_path = sql_folder, stations = c(96001, 97980, 97700)
)

# plot
station_96001 <- filter(station_all, SID == "96001")
station_97980 <- filter(station_all, SID == "97980")
plot(station_96001$valid_dttm, station_96001$T2m, type = "l",
     ylim = c(295,308),
     main = "Temperature at Station 96001 and 97980",
     xlab = "date",
     ylab = "Temp (°K)")
lines(station_97980$valid_dttm, station_97980$T2m, col="red", lty=2)
legend("bottomright", legend=c("96001", "97980"),
       col=c("black", "red"), lty=1:2, cex=0.8)
