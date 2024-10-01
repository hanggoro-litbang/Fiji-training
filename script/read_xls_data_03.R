# ---
# title: "read xls data"
# date: "2024-08-28"
# Source: https://www.tutorialspoint.com/r & https://intro2r.com/
# ---

# install library (only runs once)
# install.packages("xlsx")
# install.packages("readxl")

# load library
library("readxl")

# get current dir
getwd()

# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

# Read the first worksheet in the file input.xlsx.
# data <- read.xlsx("data/input/input.xlsx", sheetIndex = 1)
data <- read_xlsx("data/input/input.xlsx", sheet = "Sheet1")
print(data)
class(data$start_date)
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))

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

retval <- subset(data, as.Date(start_date) > as.Date("2014-01-01"))
print(retval)

# write .csv data
write.csv(retval,"output.csv")
newdata <- read.csv("output.csv")
print(newdata)

