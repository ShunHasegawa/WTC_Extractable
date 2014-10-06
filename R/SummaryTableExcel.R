extrMlt <- melt(extr, id = c("time", "date", "chamber", "location", "side", "id", "temp"))

# chamber summary table & mean
ChSmmryTbl <- dlply(extrMlt, .(variable), function(x) CreateTable(x, fac = "chamber"))
ChMean <- ddply(extrMlt, .(time, date, temp, chamber, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(ChMean, .(variable), function(x) CreateTable(x, fac = "temp"))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rawdata
sheet <- createSheet(wb,sheetName="raw_data")
addDataFrame(extr, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
shnames <- paste("Chamber_mean.",c("Nitrate", "Ammonium","Phosphate", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = ChSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrate", "Ammonium","Phosphate"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/WTC_Extractable.xlsx")
