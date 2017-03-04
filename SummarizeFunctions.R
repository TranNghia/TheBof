#Creates a table from an object processed by bof()
library(lubridate)

table.bof <- function(object, obs.lag = 2, ID = NULL, dur.units = "mins"){
  
  object$Date <- strftime(object$DateTime, format = "%Y-%m-%d")
  object$Time <- strftime(object$DateTime, format = "%H:%M:%S")

  # z <- unique(object$Date)
  # 
  # for(i in z){
  #   # i = z[1]
  #   sub <- object[object$Date == i, ]
  #   object$DayStart[object$Date == i] <- min(sub$Time)
  #   object$DayEnd[object$Date == i] <- max(sub$Time)
  # }
  
  BeginTime <- aggregate(object$DateTime, list(object$num), min)
  BeginTime <- BeginTime$x - minutes(obs.lag)
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
    colnames(table) <- c("nbout", "BeginTime", "EndTime", "duration","minT", "maxT","meanT", "depthT","typ")
  }
  
  if(!is.null(ID)){
    table <- data.frame(ID, EndTime$Group.1, BeginTime$x, EndTime$x, Duration, minT$x, maxT$x, mean$x, depth$x ,typ$x, strftime(DayStart$x, format = "%H:%M:%S"), strftime(DayEnd$x, format = "%H:%M:%S"))
    colnames(table) <- c("ID", "nbout", "BeginTime", "EndTime", "duration","minT", "maxT","meanT", "depthT","typ")
  }
  
  table$duration <- as.numeric(difftime(table$EndTime, table$BeginTime, units = dur.units))
  table <- table[order(table$BeginTime),]
  
  return(table)
}


