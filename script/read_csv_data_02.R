# ---
# title: "read csv data"
# date: "2024-08-28"
# Source: https://www.tutorialspoint.com/r & https://intro2r.com/
# ---

# get current dir
getwd()

# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

# open .csv data
data <- read.csv("data/input/input.csv")
class(data$start_date)
data$month <- substr(data$start_date,6,7)
data$month
data$start_date <- as.Date(data$start_date,format = "%Y-%m-%d")
data$month <- data$start_date
class(data$start_date)
print(data)
class(data$id)
class(data$start_date)
class(data$salary)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))
dim(data)
# Get the max salary from data frame.
sal <- max(data$salary)
print(sal)

# Get the max salary from data frame.
sal <- max(data$salary)

# Get the person detail having max salary.
retval <- subset(data, salary == max(salary))
print(retval)

# Create a data frame.
retval <- subset( data, dept == "IT")
print(retval)

info <- subset(data, salary > 600 & dept == "IT")
print(info)

retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01") & dept == "HR")
print(retval)

# write .csv data
write.csv(retval,"data/output/output.csv",row.names = FALSE)
newdata <- read.csv("data/output/output.csv")
print(newdata)

