# ---
# title: "basic_r"
# date: "2024-08-11"
# Source: https://www.digitalocean.com/community/tutorials/confusion-matrix-in-r
# ---

library(xlsx)
library(caret)
library(verification)
library(dplyr)
library(lubridate)


# get current dir
getwd()

# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

data <- read.xlsx("data/input/TOD_Verification.xlsx", sheetIndex = 1, header = TRUE, 
                  colClasses = "character")

# change column name
colnames(data)[colnames(data) == 'AERODROME'] <- 'AERO'
colnames(data)[colnames(data) == 'FLIGHT..'] <- 'FLIGHT'
colnames(data)[colnames(data) == 'ETD..UTC.'] <- 'ETD_UTC'
colnames(data)[colnames(data) == 'Time.TOD.Sent..UTC.'] <- 'Time_TOD_Sent_UTC'
colnames(data)[colnames(data) == 'X.FCST_WS'] <- 'FCST_WS'
colnames(data)[colnames(data) == 'X.OBS_WS'] <- 'OBS_WS'

# fix the each column file type
sapply(data, class) # check data format for each column
data$ETD_UTC <- format(data$ETD_UTC, "%H:%M")
data$Time_TOD_Sent_UTC <- format(data$Time_TOD_Sent_UTC, "%H:%M")

data$FCST_TEMP <- as.numeric(data$FCST_TEMP)

# concatenate date and hours
data$DATE <- as.character(data$DATE)
data$DATE <- with(data, paste(DATE,ETD_UTC, sep = " "))
data$DATE <- strptime(data$DATE, format = "%Y-%m-%d %H:%M", tz = "UTC")

# check outliers and NA
sapply(data, summary) # check data statistic

data$FCST_DEW[data$FCST_DEW > 50 ] <- NA # remove outliers
data$OBS_DEW[is.na(data$OBS_DEW)] <- mean(data$OBS_DEW, na.rm = TRUE)
data$FCST_WDIR[data$FCST_WDIR > 360] <- NA
data$OBS_WDIR[data$OBS_WDIR > 360] <- NA

# Now we have clean data
# let's try to create Plot
plot(data$FCST_TEMP, type = "l", lty=2, col="red", ylab = "Temp (C)", xlab= "Index-N")
lines(data$OBS_TEMP, , type = "l", lty=1, col="blue")
title("Nadi Forecast VS Obs Temp")
legend("bottomright", legend=c("Fcst", "Obs"),
      col=c("red", "blue"), lty=2:1, cex=0.8)

# scatter and qqplot
qqplot(data$FCST_TEMP,data$OBS_TEMP)
abline(0,1)
plot(data$FCST_TEMP,data$OBS_TEMP)
abline(0,1)

png(filename="nadi.png", width = 768, height = 768)
plot(data$FCST_TEMP, type = "l", lty=2, col="red", ylab = "Temp (C)", xlab= "Index-N")
lines(data$OBS_TEMP, , type = "l", lty=1, col="blue")
title("Nadi Forecast VS Obs Temp")
legend("bottomright", legend=c("Fcst", "Obs"),
       col=c("red", "blue"), lty=2:1, cex=0.8)
dev.off()

# let's calculate error
err <- data$FCST_TEMP - data$OBS_TEMP

# histogram
hist(err, col="green")

me <- mean(err, na.rm=TRUE)
stde <- sd(err, na.rm=TRUE)
mae <- mean(abs(err),na.rm=TRUE)
rmse <- sqrt(me*me+stde*stde)

print(me)
print(stde)
print(mae)
print(rmse)

# correlation
cor(data$FCST_TEMP, data$OBS_TEMP, method = "pearson")

# leps
temp_leps<-leps(data$OBS_TEMP, data$FCST_TEMP) 
temp_leps

# select day data
daytime <- data %>% 
  filter(hour(data$DATE) > 0 & hour(data$DATE) <= 12)

err <- daytime$FCST_TEMP - daytime$OBS_TEMP

me <- mean(err, na.rm=TRUE)
stde <- sd(err, na.rm=TRUE)
mae <- mean(abs(err),na.rm=TRUE)
rmse <- sqrt(me*me+stde*stde)

print(me)
print(stde)
print(mae)
print(rmse)

# correlation
cor(daytime$FCST_TEMP, daytime$OBS_TEMP, method = "pearson")

# leps
temp_leps<-leps(daytime$OBS_TEMP, daytime$FCST_TEMP) 
temp_leps

# select night data
nighttime <- data %>% 
  filter(hour(data$DATE) > 12 & hour(data$DATE) <= 23)

err <- nighttime$FCST_TEMP - nighttime$OBS_TEMP

me <- mean(err, na.rm=TRUE)
stde <- sd(err, na.rm=TRUE)
mae <- mean(abs(err),na.rm=TRUE)
rmse <- sqrt(me*me+stde*stde)

print(me)
print(stde)
print(mae)
print(rmse)

# correlation
cor(nighttime$FCST_TEMP, nighttime$OBS_TEMP, method = "pearson")

# leps
temp_leps<-leps(nighttime$OBS_TEMP, nighttime$FCST_TEMP) 
temp_leps

# contingency table / confusion matrix
# create category if wind speed lt 5 = calm
calm_obs <- factor(ifelse(data$OBS_WS <= 5, 
                        yes=1, no=0))

calm_fcst <- factor(ifelse(data$FCST_WS <= 5, 
                   yes=1, no=0))

#Creating confusion matrix
ws_cont <- confusionMatrix(data=calm_fcst, reference = calm_obs)

#Display results 
ws_cont
mosaicplot(ws_cont$table)

a <- ws_cont$table[1,1]
b <- ws_cont$table[1,2]
c <- ws_cont$table[2,1]
d <- ws_cont$table[2,2]

# Frequency Bias Index
FBI <- (a+b)/(a+c)
# Proportion Correct
PC <- (a+d)/(a+b+c+d)
# Probability of Detection / Hit Rate
POD <- a/(a+c)
# False Alarm Ratio
FAR <- b/(a+b)
# Post Agreement
PAG <- a/(a+b)
# Probability of False Detection
POFD <- b/(b+d)
# True Skill Statistics / Hansen and Kuiper Skill Score
TSS <- POD-POFD
# Threat Score
TS <- a/(a+b+c)
# Equitable Threat Score
ar <- (a+b)*(a+c)/(a+b+c+d)
ETS <- (a-ar)/(a+b+c-ar)
# Heidke Skill Score
HSS <- 2*((a*d)-(b*c))/((a+c)*(c+d)+(a+b)*(b+d))
# odds ratio
OR <- (a*d)/(b*c)
ORSS <- (OR-1)/(OR+1)

print(paste("FBI", FBI,sep = "="))
print(paste("PC", PC,sep = "="))
print(paste("POD", POD,sep = "="))
print(paste("FAR", FAR,sep = "="))
print(paste("PAG", PAG,sep = "="))
print(paste("POFD", POFD,sep = "="))
print(paste("TSS", TSS,sep = "="))
print(paste("TS", TS,sep = "="))
print(paste("ETS", ETS,sep = "="))
print(paste("HSS", HSS,sep = "="))
print(paste("OR", OR,sep = "="))
print(paste("ORSS", ORSS,sep = "="))

