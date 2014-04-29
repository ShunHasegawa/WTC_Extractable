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
ChFg <- dlply(ChMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Mineralisation_Chamber_", c("Nitrification", "N_mineralisation", "P_mineralisation"), ".pdf",sep = "")
l_ply(1:3, function(x) ggsave(filename = fls[x], plot = ChFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Mineralisation_Temp_", c("Nitrification", "N_mineralisation", "P_mineralisation"), ".pdf",sep = "")
l_ply(1:3, function(x) ggsave(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))
