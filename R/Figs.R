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

############################
# Fig for BES presentaiton #
############################
poster_theme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = "non")

TrtFg <- dlply(TrtMean, .(variable), function(x) PltMean(x) + poster_theme)
fls <- paste("Output/Figs/BES_Presentation/WTC_Extractable_Temp_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 5, height = 3))


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
