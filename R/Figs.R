palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))

theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
ChMean <- ddply(extrMlt, .(time, date, temp, chamber, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(ChMean, .(time, date, temp, variable), function(x) Crt_SmryDF(x, val = "Mean"))

#################################
# plot each nutrient separately #
#################################
vars <- c("Nitrate", "Ammonium", "Phosphate")

ChFg <- dlply(ChMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Extractable_Chamber_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = ChFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Extractable_Temp_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

########################
# Plot for publication #
########################
# y label for facet_grid
ylabs <- list(
  'no' = expression(KCl*-extractable~NO[3]^"-"),
  'nh' = expression(KCl*-extractable~NH[4]^"+"),
  'po' = expression(Bray*-extractable~PO[4]^"3-"))

ylab_label <- function(variable, value){
  return(ylabs[value])
}


# load stat table, note that if you want the most updated one, you need to run
# Stat.R first
load("Output//Data/TempTime_Stat.RData")

# ymax value for each variable
ymaxDF <- ddply(TrtMean, .(variable), function(x) max(x$Mean + x$SE, na.rm = TRUE))

# load contrastDF to annotate stat result and combine with max values from
# TrtMean as y position
load("Output//Data/WTCE_Extractable_ContrastDF.RData")
Antt_CntrstDF <- merge(ContrastDF, 
                       ddply(TrtMean, .(date, variable), summarise, yval = max(Mean + SE)),
                       # this return maximum values
                       by = c("date", "variable"), all.x = TRUE)

Antt_CntrstDF$temp <- "amb" # co2 column is required as it's used for mapping
Antt_CntrstDF <- subset(Antt_CntrstDF, stars != "") 
# remove empty rows as they causes trouble when using geom_text

# create a plot
p <- WBFig(data = TrtMean, 
           ylab = expression(Extractable~soil~nutrients~(mg~kg^"-1")),
           facetLab = ylab_label,
           StatRes = Stat_TempTime, 
           StatY = c(ymaxDF[1:2, 2]*1.1, ymaxDF[3, 2]-2.5)) +
  geom_text(data = Antt_CntrstDF, aes(x = date, y = yval, label = stars), 
            vjust = 0, parse = TRUE) +
  theme(legend.position = c(.8, .94),
        legend.background = element_rect(fill = "transparent",colour = NA))
p
ggsavePP(filename = "Output//Figs/Manuscriopt/WTC_Extractable", plot = p, 
         width = 6.65, height = 6.65)

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'no' = expression(KCl*-extractable~NO[3]^"-"),
  'nh' = expression(KCl*-extractable~NH[4]^"+"),
  'po' = expression(Bray*-extractable~PO[4]^"3-"))


ylab_label <- function(variable, value){
  return(ylabs[value])
}


pl <- PltMean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsavePP(filename = "Output//Figs/WTC_ExtractableNutrient_Temp", plot = pl, width = 6, height = 6)
