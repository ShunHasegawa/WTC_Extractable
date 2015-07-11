#############################
# Merge with soil variables #
#############################
diff(unique(extr$date))
# use 1month(28 days) mean

# soil variables
load("Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")
# restructure
names(soilChmSmry)[c(1, 2, 4)] <- c("date", "chamber", "probe")
SoilChMlt <- melt(soilChmSmry, id = c("date", "chamber", "temp", "probe"))
SoilCh <- cast(SoilChMlt, date + chamber + temp ~ probe + variable)

# mean of soil vars during incubation period
SoilIncSampMean <- function(insertion, sampling, Chm, data = SoilCh){
  a <- subset(data, date >= insertion & date <= sampling & chamber == Chm)
  vars <- names(a)[which(!names(a) %in% c("date", "chamber", "temp"))]
  b <- ddply(a, .(chamber), function(x) colMeans(x[, vars], na.rm = TRUE))
  return(cbind(insertion, sampling, b))
}

# chamber mean for soil nutrients

# Add NP ratio
extr$NP <- with(extr, (no + nh)/po)
names(extr)
Extr_ChMean <- ddply(extr, .(time, date, chamber, temp), 
                    function(x) {
                      d1 <- colMeans(x[,c("no", "nh", "po", "NP")], na.rm = TRUE)
                      d2 <- with(x, gm_mean(NP, na.rm = TRUE))
                      return(c(d1, gmNP = d2))
                    })

# compute means of soil variabes for each incubation period (28 days), and merge
# with nutrient df
Extr_DF <- ddply(Extr_ChMean, .(time, date, chamber, temp, no, nh, po, NP, gmNP),
                 function(x) SoilIncSampMean(insertion= x$date - 28, sampling= x$date,
                                             Chm = x$chamber))

# visudally check if the function works
p <- ggplot(SoilCh, aes(x = date, y = SoilVW_5_25_Mean))
p2 <- p + 
  geom_line() +
  geom_point(data = Extr_DF, aes(x = date - 14, y = SoilVW_5_25_Mean), 
             col = "red", size = 2)+
  facet_wrap( ~ chamber)+
  geom_vline(xintercept = as.numeric(unique(Extr_DF$date)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(unique(Extr_DF$date)) - 28, linetype = "dashed")
p2  
# good but 1st measurement of chamber01 don't have enought data points

Extr_DF$moist <- Extr_DF$SoilVW_5_25_Mean

###########
# Nitrate #
###########
source("R/Stats_NO.R")

############
# Ammonium #
############
source("R/Stats_NH.R")

#############
# Phosphate #
#############
source("R/Stats_PO.R")

###########
# Summary #
###########

################
## CO2 x Time ##
################

# create stat summary table for LMM with CO2 and time
TempTimeStatList <- list('no' = AnvF_no, 
                         'nh' = AnvF_nh, 
                         'po' = AnvF_po) 

Stat_TempTime <- ldply(names(TempTimeStatList), 
                       function(x) StatTable(TempTimeStatList[[x]], variable = x))
save(Stat_TempTime, file = "Output//Data/TempTime_Stat.RData")

########################
## Result of contrast ##
########################
ContrastDF <- WTC_ExtractableP_cntstDF
save(ContrastDF, file = "Output//Data/WTCE_Extractable_ContrastDF.RData")
