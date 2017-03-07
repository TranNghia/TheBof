# In developpment; A function for summarizing data by day
# Still some work to be done... because the current version determines whether a bout stared during the day or night, 
# this would result in night duration not being all equal... 


# Debugger ####
rm(list = ls())
source("Bof.R")
source("SummarizeFunctions.R")

DayNightData<- read.table("DayNightData.csv", sep = ",", header = TRUE)
boffed <- bof(object = DayNightData)
# BoutTable <- table.bof(boffed, obs.lag = 2, ID = "NestBox1-2012", dur.units = "mins")

#object = BoutTable; sunrise = NULL; sunset = NULL; ID = "Test"
object = boffed; sunrise = "06:00:00"; sunset = "20:00:00"; ID = "Test"; time.format = "%H:%M:%S"; obs.lag = 2

# Day.sum ####

day.sum <- function(object = NULL, sunrise = NULL, sunset = NULL, obs.lag = NULL) {

# Using an table.bof output ####   
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

    DayTable <- join_all(dfs = list(TimeIn, TimeOut, MeanOnDur, MeanOffDur, OnCount, OffCount, MeanOnT, MeanOffT, minT, maxT),
                by = "Date", match = "all")
  }

  if(!is.null(sunrise) & !is.null(sunset) & !is.null(time.format)){

    object$time <- strptime(strftime(object$BeginTime, format = "%H:%M:%S"), format = "%H:%M:%S")
    object$period <- ifelse(object$time > strptime(sunrise, format = "%H:%M:%S") & object$time < strptime(sunset, format = "%H:%M:%S"), "Day", "Night")
    object$date <- strptime(strftime(object$BeginTime, format = "%Y-%m-%d"), format = "%Y-%m-%d")

    object[object$period == "Night" & object$time <= strptime(sunrise, format = "%H:%M:%S"), ]$date <-
      object[object$period == "Night" & object$time <= strptime(sunrise, format = "%H:%M:%S"), ]$date - days(1)

    object$date.period <- paste(strftime(object$date, format = "%Y-%m-%d"), object$period, sep = "")
    object <- object[ , -c(which(colnames(object) == "time"), which(colnames(object) == "date"))]

    ####
    TimeIn <- aggregate(x = object[object$typ == "in", "duration"], by = list(object[object$typ == "in", "date.period"]), FUN = sum)
    colnames(TimeIn) <- c("date.period", "TimeIn")
    TimeOut <- aggregate(x = object[object$typ == "out", "duration"], by = list(object[object$typ == "out", "date.period"]), FUN = sum)
    colnames(TimeOut) <- c("date.period", "TimeOut")

    MeanOnDur <- aggregate(x = object[object$typ == "in", "duration"], by = list(object[object$typ == "in", "date.period"]), FUN = mean)
    colnames(MeanOnDur) <- c("date.period", "MeanOnDur")
    MeanOffDur <- aggregate(x = object[object$typ == "out", "duration"], by = list(object[object$typ == "out", "date.period"]), FUN = mean)
    colnames(MeanOffDur) <- c("date.period", "MeanOffDur")

    OnCount <-  aggregate(x = object[object$typ == "in", "typ"], by = list(object[object$typ == "in", "date.period"]), FUN = length)
    colnames(OnCount) <- c("date.period", "OnCount")
    OffCount <-  aggregate(x = object[object$typ == "out", "typ"], by = list(object[object$typ == "out", "date.period"]), FUN = length)
    colnames(OffCount) <- c("date.period", "OffCount")

    MeanOnT <-  aggregate(x = object[object$typ == "in", "meanT"], by = list(object[object$typ == "in", "date.period"]), FUN = mean)
    colnames(MeanOnT) <- c("date.period", "MeanOnT")
    MeanOffT <-  aggregate(x = object[object$typ == "out", "meanT"], by = list(object[object$typ == "out", "date.period"]), FUN = mean)
    colnames(MeanOffT) <- c("date.period", "MeanOffT")

    minT <-  aggregate(x = object[, "minT"], by = list(object[, "date.period"]), FUN = min)
    colnames(minT) <- c("date.period", "MinT")
    maxT <-  aggregate(x = object[, "maxT"], by = list(object[, "date.period"]), FUN = max)
    colnames(maxT) <- c("date.period", "maxT")

    DayTable <- join_all(dfs = list(TimeIn, TimeOut, MeanOnDur, MeanOffDur, OnCount, OffCount, MeanOnT, MeanOffT, minT, maxT),
                         by = "date.period", match = "all")
  }


# Using an bof output ####
  
  object$DateTime <- strptime(object$DateTime, format = "%Y-%m-%d %H:%M:%S")
  object$date <- strftime(object$DateTime, format = "%Y-%m-%d")
  object$time <- strptime(strftime(object$DateTime, format = "%H:%M:%S"), format = "%H:%M:%S")
  object$period <- ifelse(object$time > strptime(sunrise, format = "%H:%M:%S") & object$time < strptime(sunset, format = "%H:%M:%S"), "Day", "Night")
  object <- object[, -which(colnames(object) == "time")]
  object$date.period <- paste(object$date, object$period, sep = "")
  
  
  TimeIn <- aggregate(x = object[object$typ == "in", "typ"], by = list(object[object$typ == "in", "date.period"]), FUN = length)
  colnames(TimeIn) <- c("date.period", "TimeIn")
  TimeIn$TimeIn <- TimeIn$TimeIn*obs.lag
  
  TimeOut <- aggregate(x = object[object$typ == "out", "typ"], by = list(object[object$typ == "out", "date.period"]), FUN = length)
  colnames(TimeOut) <- c("date.period", "TimeOut")
  TimeOut$TimeOut <- TimeOut$TimeOut*obs.lag
  
  OnBoutCount <- aggregate(x = object[object$typ == "in", "num"], by = list(object[object$typ == "in", "date.period"]), FUN = unique)
  colnames(OnBoutCount) <- c("date.period", "OnBoutCount")
  OnBoutCount$OnBoutCount <- lengths(OnBoutCount$OnBoutCount)
  
  OffBoutCount <- aggregate(x = object[object$typ == "out", "num"], by = list(object[object$typ == "out", "date.period"]), FUN = unique)
  colnames(OffBoutCount) <- c("date.period", "OffBoutCount")
  OffBoutCount$OffBoutCount <- lengths(OffBoutCount$OffBoutCount)
  
  MeanOnT <-  aggregate(x = object[object$typ == "in", "temp"], by = list(object[object$typ == "in", "date.period"]), FUN = mean)
  colnames(MeanOnT) <- c("date.period", "MeanOnT")
  
  MeanOffT <-  aggregate(x = object[object$typ == "out", "temp"], by = list(object[object$typ == "out", "date.period"]), FUN = mean)
  colnames(MeanOffT) <- c("date.period", "MeanOffT")
  
  minT <-  aggregate(x = object[, "temp"], by = list(object[, "date.period"]), FUN = min)
  colnames(minT) <- c("date.period", "MinT")
  
  maxT <-  aggregate(x = object[, "temp"], by = list(object[, "date.period"]), FUN = max)
  colnames(maxT) <- c("date.period", "maxT")
  
  minOnT <-  aggregate(x = object[object$typ == "in", "temp"], by = list(object[object$typ == "in", "date.period"]), FUN = min)
  colnames(minOnT) <- c("date.period", "MinT")
  
  maxOnT <-  aggregate(x = object[object$typ == "in", "temp"], by = list(object[object$typ == "in", "date.period"]), FUN = max)
  colnames(maxOnT) <- c("date.period", "maxT")
  
  minOffT <-  aggregate(x = object[object$typ == "out", "temp"], by = list(object[object$typ == "out", "date.period"]), FUN = min)
  colnames(minOffT) <- c("date.period", "MinT")
  
  maxOffT <-  aggregate(x = object[object$typ == "out", "temp"], by = list(object[object$typ == "out", "date.period"]), FUN = max)
  colnames(maxOffT) <- c("date.period", "maxT")
  
  dur <- aggregate(object$typ, by = list(object$num), FUN = length)
  object$dur <- ifelse(object$num %in% dur$Group.1, dur$x[match(object$num, dur$Group.1)], NA)
  object$dur <- object$dur*obs.lag
  MeanOnDur <- aggregate(x = (object[object$typ == "in", "dur"]), by = list(object[object$typ == "in", "date.period"]), FUN = mean)
  colnames(MeanOnDur) <- c("date.period", "MeanOnDur")
  
  MeanOffDur <- aggregate(x = (object[object$typ == "out", "dur"]), by = list(object[object$typ == "out", "date.period"]), FUN = mean)
  colnames(MeanOffDur) <- c("date.period", "MeanOnDur")
  
  DayTable <- join_all(dfs = list(TimeIn, TimeOut, MeanOnDur, OnBoutCount, MeanOffDur ,OffBoutCount, MeanOnT, MeanOffT, minT, maxT, minOnT, maxOnT, minOffT, maxOffT),
                       by = "date.period", match = "all")
  DayTable$IncCon <- (DayTable$TimeIn)/(DayTable$TimeIn + DayTable$TimeOut)
}  
