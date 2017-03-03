#Creates a table from an object processed by bof()
library(lubridate)

table.bof <- function(object, obs.lag = 2, ID = NULL){
  
  object$Date <- strftime(object$DateTime, format = "%Y-%m-%d")
  object$Time <- strftime(object$DateTime, format = "%H:%M:%S")
  
  object$DayStart <- NA
  object$DayEnd <- NA
  
  z <- unique(object$Date)
  
  for(i in z){
    # i = z[1]
    sub <- object[object$Date == i, ]
    object$DayStart[object$Date == i] <- min(sub$Time)
    object$DayEnd[object$Date == i] <- max(sub$Time)
  }
  
  BeginTime <- aggregate(object$DateTime, list(object$num), min)
  BeginTime <- BeginTime  -  minutes(obs.lag)
  EndTime <- aggregate(object$DateTime, list(object$num), max)
  Duration <- rep(NA, length.out = nrow(BeginTime))
  minT <- aggregate(object$temp, list(object$num), min)
  maxT <- aggregate(object$temp, list(object$num), max)
  mean <- aggregate(object$temp, list(object$num), mean)
  depth <- aggregate(object$dif, list(object$num), sum)
  typ <- aggregate(object$typ, list(object$num), unique)
  DayStart <- aggregate(as.POSIXct(object$DayStart, format = "%H:%M:%S"), list(object$num), min)
  DayEnd <- aggregate(as.POSIXct(object$DayEnd, format = "%H:%M:%S"), list(object$num), min)
  
  if(is.null(ID)){
    table <- data.frame(EndTime$Group.1, BeginTime$x, EndTime$x, Duration, minT$x, maxT$x, mean$x, depth$x ,typ$x, strftime(DayStart$x, format = "%H:%M:%S"), strftime(DayEnd$x, format = "%H:%M:%S"))
    colnames(table) <- c("nbout", "BeginTime", "EndTime", "duration","minT", "maxT","meanT", "depthT","typ", "DayStart", "DayEnd")
  }
  
  if(!is.null(ID)){
    table <- data.frame(ID,EndTime$Group.1, BeginTime$x, EndTime$x, Duration, minT$x, maxT$x, mean$x, depth$x ,typ$x, strftime(DayStart$x, format = "%H:%M:%S"), strftime(DayEnd$x, format = "%H:%M:%S"))
    colnames(table) <- c("ID", "nbout", "BeginTime", "EndTime", "duration","minT", "maxT","meanT", "depthT","typ", "DayStart", "DayEnd")
  }
  
  table$duration <- as.numeric(table$EndTime - table$BeginTime)
  table <- table[order(table$BeginTime),]
  
  return(table)
}


# In developpment; A function for summarizing data by day

# source("Bof.R")
# source("SummarizeFunctions.R")
# 
# DayNightData<- read.table("DayNightData.csv", sep = ",", header = TRUE)
# boffed <- bof(object = DayNightData)
# BoutTable <- table.bof(boffed, obs.lag = 2, ID = "NestBox1-2012")

# object = BoutTable; ID = "WriteID"; sunrise = NULL; sunset = NULL

day.sum <- function(object = NULL, ID = NULL, sunrise = NULL, sunset = NULL) {
  
  if(is.null(sunrise) & is.null(sunset)){
    object$Date <- strftime(object$BeginTime, format = "%Y-%m-%d")
    
    TimeIn <- aggregate(x = object[object$typ == "in", "duration"], by = list(object[object$typ == "in", "Date"]), FUN = sum)
    colnames(TimeIn) <- c("Date", "TimeIn")
    TimeOut <- aggregate(x = object[object$typ == "out", "duration"], by = list(object[object$typ == "out", "Date"]), FUN = sum)
    colnames(TimeOut) <- c("Date", "TimeOut")
    
    MeanOnDur <- aggregate(x = object[object$typ == "in", "duration"], by = list(object[object$typ == "in", "Date"]), FUN = mean)
    colnames(MeanOnDur) <- c("Date", "MeanOnDur")
    MeanOffDur <- aggregate(x = object[object$typ == "out", "duration"], by = list(object[object$typ == "out", "Date"]), FUN = mean)
    colnames(MeanOffDur) <- c("Date", "MeanOffDur")
    
    OnCount <-  aggregate(x = object[object$typ == "in", "typ"], by = list(object[object$typ == "in", "Date"]), FUN = length)
    colnames(OnCount) <- c("Date", "OnCount")
    OffCount <-  aggregate(x = object[object$typ == "out", "typ"], by = list(object[object$typ == "out", "Date"]), FUN = length)
    colnames(OffCount) <- c("Date", "OffCount")
    
    MeanOnT <-  aggregate(x = object[object$typ == "in", "meanT"], by = list(object[object$typ == "in", "Date"]), FUN = mean)
    colnames(MeanOnT) <- c("Date", "MeanOnT")
    MeanOffT <-  aggregate(x = object[object$typ == "out", "meanT"], by = list(object[object$typ == "out", "Date"]), FUN = mean)
    colnames(MeanOffT) <- c("Date", "MeanOffT")
    
    minT <-  aggregate(x = object[, "minT"], by = list(object[, "Date"]), FUN = min)
    colnames(minT) <- c("Date", "MinT")
    maxT <-  aggregate(x = object[, "maxT"], by = list(object[, "Date"]), FUN = max)
    colnames(maxT) <- c("Date", "maxT")
    
    merge()
  }
  
}
