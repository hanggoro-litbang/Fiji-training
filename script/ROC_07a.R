cat("\014") # clear
rm(list = ls()) # clear stored variables

# install.packages(c("netcdf, "xlsx", "dplyr"))
# install.packages("netcdf")
# load libraries
library(ncdf4)
library(xlsx)
library(dplyr)

#set working directory
setwd('/Users/wido/Documents/WORK/2024/Training-Fiji/')

# observation data
obs_batinikama <- read.xlsx('data/input/Fiji_daily_rainfall/Batinikama_Labasa_Mill_dly_rain.xlsx',
                      sheetIndex = 1)
# check unusual data
head(obs_batinikama)
tail(obs_batinikama)
summary(obs_batinikama)
summary(obs_batinikama$X1.8)
plot(obs_batinikama$X1.8, type="l")

# the obs data start at 1960-01-02
# and end at 2020-12-31
# there are few -99.9 value (suspected as NA value)

# data cleaning (takes about 80% of all analysis processes)
# create date column
# seq(as.Date("2014/09/04"), by = "day", length.out = 5)
obs_batinikama$date <-seq(as.Date("1960/01/02"), as.Date("2020/12/31"),
                          by = "day")
# change -99.9 to NA
obs_batinikama <- obs_batinikama %>% 
  mutate(obs = ifelse(X1.8 < 0, NA, X1.8))

# select date and obs column
obs_batinikama <- obs_batinikama[,c(5,6)]

# forecast (dummy forecast)

# load cmorph
nc_cmorph <- nc_open('data/input/Fiji_daily_rainfall/cmorph_labasa.nc')
nc_cmorph
cmorph_rain <- ncvar_get(nc_cmorph,'prcp')
#create data variable
nc_cmorph$dim$time$units
cmorph_date <- as.Date(nc_cmorph$dim$time$vals, origin="1998-01-01")
cmorph_rain <- data.frame(cmorph_date,cmorph_rain)
colnames(cmorph_rain) <- c("date","cmorph")
# select date based on obs date
cmorph_rain <- filter(cmorph_rain, 
                      date >= "1960-01-02" & date <= "2020-12-31")
nc_close(nc_cmorph)
# load cpc
nc_cpc <- nc_open('data/input/Fiji_daily_rainfall/cpc_labasa.nc')
nc_cpc
cpc_rain <- ncvar_get(nc_cpc,'prcp')
#create data variable
nc_cpc$dim$time$units
cpc_date <- as.Date(nc_cpc$dim$time$vals, origin="1979-01-01")

cpc_rain <- data.frame(cpc_date,cpc_rain)
colnames(cpc_rain) <- c("date","cpc")
# select date based on obs date
cpc_rain <- filter(cpc_rain, 
                      date >= "1960-01-02" & date <= "2020-12-31")
nc_close(nc_cpc)

# load era5
nc_era5 <- nc_open('data/input/Fiji_daily_rainfall/era5_labasa.nc')
nc_era5
era5_rain <- ncvar_get(nc_era5,'tp')
#create data variable
nc_era5$dim$time$units
era5_date <- as.Date(nc_era5$dim$time$vals, origin="1950-01-01")

era5_rain <- data.frame(era5_date,era5_rain)
colnames(era5_rain) <- c("date","era5")
# select date based on obs date
era5_rain <- filter(era5_rain, 
                   date >= "1960-01-02" & date <= "2020-12-31")
nc_close(nc_era5)

# load gpm02
nc_gpm02 <- nc_open('data/input/Fiji_daily_rainfall/gpm02_labasa.nc')

nc_gpm02
gpm02_rain <- ncvar_get(nc_gpm02,'precipitationCal')
#create data variable
nc_gpm02$dim$time$units
gpm02_date <- as.Date(nc_gpm02$dim$time$vals, origin="2000-01-01")

gpm02_rain <- data.frame(gpm02_date,gpm02_rain)
colnames(gpm02_rain) <- c("date","gpm02")
# select date based on obs date
gpm02_rain <- filter(gpm02_rain, 
                   date >= "1960-01-02" & date <= "2020-12-31")
nc_close(nc_gpm02)

# load gpm05
nc_gpm05 <- nc_open('data/input/Fiji_daily_rainfall/gpm05_labasa.nc')
nc_gpm05
gpm05_rain <- ncvar_get(nc_gpm05,'precipitationCal')
#create data variable
nc_gpm05$dim$time$units
gpm05_date <- as.Date(nc_gpm05$dim$time$vals, origin="2000-01-01")

gpm05_rain <- data.frame(gpm05_date,gpm05_rain)
colnames(gpm05_rain) <- c("date","gpm05")
# select date based on obs date
gpm05_rain <- filter(gpm05_rain, 
                   date >= "1960-01-02" & date <= "2020-12-31")
nc_close(nc_gpm05)

# merge data
# merge_rain <- left_join(obs_batinikama, cmorph_rain, cpc_rain, era5_rain,
                        # gpm02_rain,gpm05_rain)

alldata <- obs_batinikama %>%
  left_join(cmorph_rain, by='date') %>%
  left_join(cpc_rain, by='date') %>%
  left_join(era5_rain, by='date') %>%
  left_join(gpm02_rain, by='date') %>%
  left_join(gpm05_rain, by='date')

# analysis the data
# Plot multiple lines using matplot
matplot(alldata$date, alldata[,2:7], type = "h", lty = 1)

# histogram
# First distribution
hist(alldata$obs, breaks=30, xlim=c(0,200), col=rgb(1,0,0,0.5) )

# Second with add=T to plot on top
hist(alldata$cmorph, breaks=30, xlim=c(0,200), col=rgb(0,0,1,0.5), add=T)
hist(alldata$era5, breaks=30, xlim=c(0,200), col=rgb(0,0,1,0.5), add=T)
# select data from the shortest time period
# replace_na<-function(df){
#   while(anyNA(df[1,])==TRUE){
#     df=df[-1,]
#   }
#   return(df)
# }
# short_data <- replace_na(alldata)

short_data <- filter(alldata, 
       date >= "2001-01-01" & date <= "2020-12-31")
# select data with extreme value
short_data$obs_con <- ifelse(short_data$obs >= 20, 
                                    yes=1, no=0)

short_data$cmorph_con <- ifelse(short_data$cmorph >= 20, 
                                    yes=1, no=0)

short_data$cpc_con <- ifelse(short_data$cpc >= 20, 
                                    yes=1, no=0)

short_data$era5_con <- ifelse(short_data$era5 >= 20, 
                                    yes=1, no=0)

short_data$gpm02_con <- ifelse(short_data$gpm02 >= 20, 
                                    yes=1, no=0)


short_data$gpm05_con <- ifelse(short_data$gpm05 >= 20, 
                                      yes=1, no=0)

short_data$prob <- (short_data$cmorph_con +
                      short_data$cpc_con + short_data$era5_con +
                      short_data$gpm02_con + short_data$gpm05_con)/5

roc(short_data$obs_con,short_data$prob, plot=TRUE)

roc(short_data$obs_con, short_data$prob, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", ylab="True Postive Percentage",
    col="#377eb8", lwd=4, print.auc.x=45, print.auc = TRUE)


rd <- ReliabilityDiagram(short_data$prob, short_data$obs_con, plot=TRUE, bins = 3,
                         handle.na="use.pairwise.complete")


ws_cont <- confusionMatrix(data=as.factor(short_data$gpm02_con), 
                           reference = as.factor(short_data$obs_con))

ws_cont <- confusionMatrix(data=as.factor(short_data$gpm05_con), 
                           reference = as.factor(short_data$obs_con))

ws_cont
