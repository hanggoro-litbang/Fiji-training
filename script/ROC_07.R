# ---
# title: "basic_r"
# date: "2024-08-11"
# Source: https://github.com/StatQuest/roc_and_auc_demo/blob/master/roc_and_auc_demo.R
# ---
# install.packages(c("pROC","SpecsVerification"))
library(pROC) # install with install.packages("pROC")
library(SpecsVerification)

# get current dir
getwd()

# set working directory
wdir <- '/Users/wido/Documents/WORK/2024/Training-Fiji/'
setwd(wdir)

rain<-read.csv("data/input/rain_prob_edit.csv")
rain <- subset(rain,p24_cat0 > -999)
rain$bin <- ifelse(rain$obs.mm. > 0.2, yes = 1, no=0)
roc(rain$bin,rain$p24_cat0, plot=TRUE)
par(pty = "s")
roc(rain$bin,rain$p24_cat0, plot=TRUE)
roc(rain$bin, rain$p24_cat0, plot=TRUE, legacy.axes=TRUE)


roc(rain$bin, rain$p24_cat0, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", ylab="True Postive Percentage",
    col="#377eb8", lwd=4, print.auc.x=45, print.auc = TRUE)


roc(rain$bin, rain$p24_cat0, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", ylab="True Postive Percentage",
    col="#377eb8", lwd=4, print.auc.x=45, print.auc = TRUE, partial.auc=c(100, 90), 
    auc.polygon = TRUE, auc.polygon.col = "#377eb822")

roc.info <- roc(rain$bin, rain$p24_cat0, legacy.axes=TRUE)
str(roc.info)

## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 


tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

# reability diagram
rd <- ReliabilityDiagram(rain$p24_cat0, rain$bin, plot=TRUE, bins = 10)
print(rd)

# brier score
brierScore <- mean((rain$p24_cat0-rain$bin)^2)


