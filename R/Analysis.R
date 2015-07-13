rm(list=ls(all=TRUE))

source("R/packages.R")
source("R//functions.R")

##################
#Process dataste #
##################
# extr <- read.csv("Data//WTC_extractable.csv", colClasses = c("time" = "factor", "location" = "factor", "date" = "character"))
# extr <- extr[complete.cases(extr), ]
# extr <- droplevels(extr)
# extr$chamber <- factor(ifelse(extr$chamber < 10, paste("0", extr$chamber, sep = ""), extr$chamber))
# extr$date <- as.Date(dmy(extr$date)) 
#   #dmy makes variables POSIXct but horizontal line of errobars won't be plotted proparly probably
#   # because of difference scales
# extr$id <- extr$chamber:extr$side
# save(extr, file = "Output//Data/WTC_ExtractableNutrient.RData")
load("Output//Data/WTC_ExtractableNutrient.RData")

# rmeove negative nh values
extr$nh[extr$nh < 0] <- NA

#################
# Summary table #
#################
source("R/SummaryTableExcel.R")

########
# Figs #
########
source("R/Figs.R")

#########
# Stats #
#########
source("R//Stats.R")

save.image("Output/Data/AllObj.RData")
