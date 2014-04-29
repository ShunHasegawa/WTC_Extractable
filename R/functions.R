###################################
# Coorect AQ2 result based on CCV #
###################################

# subset dataset beween each ccv
Crrct.ccv <- function(x, data, ccval = 7.5){
  # subset dataset between given ccvs
  ccv.ps <- grep("V$", as.character(data$Sample.ID))
  
  ts <- data[c(ccv.ps[x] : ccv.ps[x + 1]),]
  trng <- range(ts$times)
  xv <- as.numeric(trng)
  yv <- ts$Result[ts$times == trng[1] | ts$times == trng[2]] 
  
  b <- ccval * (1 / yv[1] - 1 / yv[2]) / (xv[1] - xv[2])
  a <- ccval / yv[1] - b * xv[1]
  
  ts$Result <- ts$Result * (a + b * as.numeric(ts$times))
  ts$times <- NULL
  return(ts)
}

# applied the function above for each subsets of ccvs
Crrtct.ccv.df <- function(filename, ccval = 7.5){
  data <- read.csv(paste("Data/AQ2/NeedToBeCorrected/", filename, sep = ""), header = TRUE)
  
  # make time factor as numeric
  a <- sapply(as.character(data$Time), strsplit, " ")
  b <- ldply(a, function(x) paste(x[c(5, 2, 3, 4)], collapse = "/"))
  
  b$V1 <- ymd_hms(b$V1)
  
  names(b) <- c("Time", "times")
  
  # merge
  mrg.res <- merge(data, b, by = "Time")
  
  # reorder accoding to time
  mrg.res <- mrg.res[order(mrg.res$times), ]
  
  # add the latest ccv value at the end of data frame to make sure all measured values are placed between ccvs
  # if the last line is not ccv, vlues after last ccv will be turned into NA
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  lstTime <- mrg.res$times[nrow(mrg.res)]
  mrg.res[nrow(mrg.res) + 1, ] <- mrg.res[max(ccv.ps), ] 
  mrg.res$times[nrow(mrg.res)] <- lstTime + 1 # make the last time latest by adding 1 to the actual latest time
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID)) # update ccv.position
  
  # re-caluculate the results
  a <- ldply(1:(length(ccv.ps)-1), function(x) Crrct.ccv(x, data = mrg.res, ccval))
  
  # if there are negative values, add minimum value to remove any negative values
  if(any(a$Result < 0)) a$Result <- a$Result - min(a$Result, na.rm = TRUE)
  
  return(a)
}

######################################################
# process and combine aq 2 data, then create a table #
######################################################
prcsAQ2 <- function(data){
  # remove ccv, ccb, standard
  res <- data[-grep("^C|^STANDARD", as.character(data$Sample.ID)),]
  
  # remove dup, top, middle
  res <- res[-grep("dup$|top|middle", as.character(res$Sample.Details)),]
  
  # sample labels
  a <- strsplit(as.character(res$Sample.Details), "[.]")
  
  # turn this into data frame
  a.df <- ldply(a)
  names(a.df)[c(1, 4:6)] <- c("Date", "Incubation", "Chamber", "Side")
  a.df$Date <- ymd(a.df$Date)
  res.df <- cbind(a.df, res)
  res.df <- res.df[c("Date", "Incubation","Chamber", "Side", "Result")]
  res.df <- res.df[order(res.df$Date, res.df$Incubation, as.numeric(res.df$Chamber), as.numeric(res.df$Side)),]
  return(res.df)
}

cmbn.fls <- function(file){
  # read files
  rd.fls <- lapply(file, function(x) read.csv(paste("Data/AQ2/ReadyToProcess/", x, sep = ""), header = TRUE))
  
  # process and make data frame for each test type
  pr.df <- ldply(rd.fls, function(x) ddply(x, .(Test.Name), prcsAQ2))
  
  # reshape
  names(pr.df)[grep("Result", names(pr.df))] <- "value"
  pr.cst$Chamber <- as.numeric(pr.cst$Chamber)
  pr.cst$Side  <- as.numeric(pr.cst$Side)
  pr.cst <- cast(pr.df, Date + Incubation + Chamber + Side ~ Test.Name)
  pr.cst <- pr.cst[order(pr.cst$Incubation, pr.cst$Date, as.numeric(pr.cst$Chamber), as.numeric(pr.cst$Side)),]
  return(pr.cst)
}

##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac){
  a <- dataset[c("date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(mer)
}

#function which creates excel worksheets
crSheet <- function(sheetname, dataset){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  addDataFrame(dataset, sheet, showNA = TRUE, row.names = FALSE, startRow = 2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=mg DrySoil(kg)^(-1) day^(-1)")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

####################
# plot mean and se #
####################
PltMean <- function(data){
  ylabs <- c(expression(atop("Nitrification rates", paste((mg~dry_soil_kg^-1~day^-1)))),
             expression(atop("N mineralisation rates", paste((mg~dry_soil_kg^-1~day^-1)))),
             expression(atop("P mineralisation rates", paste((mg~dry_soil_kg^-1~day^-1)))))
  
  ylab <- ifelse(length(unique(data$variable)) > 1, expression((mg~dry_soil_kg^-1~day^-1)),
                 ifelse(unique(data$variable) == "nitrification", ylabs[1], 
                        ifelse(unique(data$variable) == "n.min", ylabs[2],
                               ylabs[3])))
  
  colfactor <- ifelse(any(names(data) == "chamber"), "chamber", "temp")
  
  p <- ggplot(data, aes_string(x = "date", y = "Mean", col = colfactor))
  
  p2 <- p + geom_line(size = 1) + 
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", col = colfactor), width = 5) + 
    labs(x = "Time", y = ylab)
  
  # change colors, linetype and associated legend according plotting groups (chamber or treatment)
  if(colfactor == "temp") p2 +  scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) else
    p2 + scale_color_manual(values = palette(), "Chamber", labels = paste("Chamber", c(1:12), sep = "_")) +
    scale_linetype_manual(values = rep(c("solid", "dashed"), 6), "Chamber", labels = paste("Chamber", c(1:12), sep = "_")) +
    guides(color = guide_legend(keyheight = 0.7))
}

#######################
#model simplification #
#######################
MdlSmpl <- function(model){
  mod2 <- update(model, method = "ML") #change method from REML to ML
  stai <- stepAIC(mod2, trace = FALSE) #model simplification by AIC
  dr <- drop1(stai, test="Chisq") #test if removing a factor even more significantly lowers model
  model <- update(stai, method="REML")
  ifelse(all(dr[[4]] < 0.05, na.rm=TRUE), anr <- anova(model), anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic = stai$anova, drop1 = dr, anova.reml = anr, model.reml = model, model.ml = stai))
}

#############################################
# compare different auto-correlation models #
#############################################
atcr.cmpr <- function(model, rndmFac){
  if(rndmFac == "chamber/side"){
    model2 <- update(model,corr=corCompSymm(form=~1|chamber/side)) 
  } else {
    if(rndmFac == "chamber"){
      model2 <- update(model,corr=corCompSymm(form=~1|Chamber))
    } else {
      model2 <- update(model,corr=corCompSymm(form=~1|id))
    }
  }
  
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  models <- list(model, model2, model3, model4, model5, a)
  return(models)
}

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3)
bxplts <- function(value, ofst = 0, data){
  par(mfrow = c(2,2))
  y <- data[[value]] + ofst #ofst is added to make y >0
  boxplot(y ~ temp*time, data)
  boxplot(log(y) ~ temp*time, main = "log", data)
  boxplot(sqrt(y) ~ temp*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ temp*time, main = "power(1/3)", data)
  par(mfrow = c(1,1))
}
