# Plotting function using an bof object

plot.bof<-function(object, tdif = 2, nbout = NA, from = NA, to = NA, time.format = NA, date = NA, date.format = NA, plot.all = FALSE , off.window = TRUE, off.window.num = TRUE,...){
  
  #To select bouts     
  if(!is.na(nbout) & is.na(from) & is.na(to) & is.na(time.format) & is.na(date) & is.na(date.format) & plot.all == FALSE){
    object2 <- subset(object, object$num %in% nbout)
    
    plot(object2$DateTime, object2$temp, type="p", main = paste("Begin Date:", min(object2$DateTime), sep = " "), ...)
    lines(object2$DateTime, object2$temp, type="l")
    
    if(off.window == TRUE){
      offbout <- subset(nbout, nbout< 0)
      
      for(i in offbout){
        offbout2 <- subset(object2, num == i) 
        a = min(offbout2$DateTime) - tdif*60
        rect(a, min(object2$temp) ,max(offbout2$DateTime), max(object2$temp), col = rgb(0, 0.5, 1, alpha=0.3))
        if(off.window.num == TRUE){text(min(offbout2$DateTime), min(object2$temp), labels = i, bg = "white")}
      }
    }
  }
  
  #To select time window  
  if(!is.na(from) & !is.na(to) & !is.na(time.format) & is.na(nbout) & is.na(date) & is.na(date.format) & plot.all == FALSE){  
    object2 <- subset(object, DateTime >= as.POSIXct(from, format = time.format) & DateTime <= as.POSIXct(to, format = time.format))
    
    plot(object2$DateTime, object2$temp, type="p", main = paste("Begin Date:", min(object2$DateTime), sep = " "), ...)
    # plot(object2$DateTime, object2$temp, type="p", main = paste("Begin Date:", min(object2$DateTime), sep = " ")) 
    lines(object2$DateTime, object2$temp, type="l")
    
    if(off.window == TRUE){
      offbout <- unique(object2[object2$num < 0,]$num)
      for(i in offbout){
        offbout2 <- subset(object2, num == i) 
        a = min(offbout2$DateTime) - tdif*60
        rect(a, min(object2$temp) ,max(offbout2$DateTime), max(object2$temp), col = rgb(0, 0.5, 1, alpha=0.3))
        if(off.window.num == TRUE){text(min(offbout2$DateTime), min(object2$temp), labels = i, bg = "white")}
      }
    }
  }
  
  #To select a day
  if(!is.na(date) & !is.na(date.format) & is.na(from) & is.na(to) & is.na(time.format) & is.na(nbout) & plot.all == FALSE){  
    object2 <- object
    object2$Date <- as.POSIXct(strftime(object2$DateTime, format = date.format))
    object2<- subset(object2, Date %in% as.POSIXct(date, format = date.format))
    
    # plot(object2$DateTime, object2$temp, type="p", pch = 19, cex = 0.3)
    # lines(object2$DateTime, object2$temp, type="l")
    plot(object2$DateTime, object2$temp, type="p", ...)
    lines(object2$DateTime, object2$temp, type="l")
    
    if(off.window == TRUE){
      offbout <- unique(object2[object2$num < 0,]$num)
      for(i in offbout){
        offbout2 <- subset(object2, num == i) 
        a = min(offbout2$DateTime) - tdif*60
        rect(a, min(object2$temp) ,max(offbout2$DateTime), max(object2$temp), col = rgb(0, 0.5, 1, alpha=0.3))
        if(off.window.num == TRUE){text(min(offbout2$DateTime), min(object2$temp), labels = i, bg = "white")}
      }
    }
  }  
  
  #To plot all
  if(is.na(date) & is.na(date.format) & is.na(from) & is.na(to) & is.na(time.format) & is.na(nbout) & plot.all == TRUE){  
    object2 <- object
    
    plot(object2$DateTime, object2$temp, type="p" ,...)
    # plot(object2$DateTime, object2$temp, type="p", main = paste("Begin Date:", min(object2$DateTime), sep = " "))
    lines(object2$DateTime, object2$temp, type="l")
    
    if(off.window == TRUE){
      offbout <- unique(object2[object2$num < 0,]$num)
      for(i in offbout){
        offbout2 <- subset(object2, num == i) 
        a = min(offbout2$DateTime) - tdif*60
        rect(a, min(object2$temp) ,max(offbout2$DateTime), max(object2$temp), col = rgb(0, 0.5, 1, alpha=0.3))
        if(off.window.num == TRUE){text(min(offbout2$DateTime), min(object2$temp), labels = i, bg = "white")}
      }
    }
  } 
  
} 


# The same as plot.bof but with more detailed information that are usefull when doing the process of visual inspection
# install.packages("TeachingDemos")
library(TeachingDemos)

plot.bof.correct <- function(object, tdif = 2, nbout = NA, from = NA, to = NA, time.format = NA, date = NA, date.format = NA, plot.all = FALSE , off.window = TRUE, off.window.num = TRUE, on.window.num = FALSE, numneg = FALSE,numpos = FALSE, plot.threshold = TRUE,...){
  
  #To select bouts     
  if(!is.na(nbout) & is.na(from) & is.na(to) & is.na(time.format) & is.na(date) & is.na(date.format) & plot.all == FALSE){
    object2 <- subset(object, object$num %in% nbout)
    
    plot(object2$DateTime, object2$temp, type="p", ylim =c(min(object2$temp) - 1, (max(object2$temp) + 5)), ...)
    lines(object2$DateTime, object2$temp, type="l")
  }
  
  #To select time window  
  if(!is.na(from) & !is.na(to) & !is.na(time.format) & is.na(nbout) & is.na(date) & is.na(date.format) & plot.all == FALSE){  
    object2 <- subset(object, DateTime >= as.POSIXct(from, format = time.format) & DateTime <= as.POSIXct(to, format = time.format))
    
    plot(object2$DateTime, object2$temp, type="p", ylim =c(min(object2$temp) - 1, (max(object2$temp) + 5)), ...)
    # plot(object2$DateTime, object2$temp, type="p", main = paste("Begin Date:", min(object2$DateTime), sep = " ")) 
    lines(object2$DateTime, object2$temp, type="l")
  }
  
  #To select a day
  if(!is.na(date) & !is.na(date.format) & is.na(from) & is.na(to) & is.na(time.format) & is.na(nbout) & plot.all == FALSE){  
    object2 <- object
    object2$Date <- as.POSIXct(strftime(object2$DateTime, format = date.format))
    object2<- subset(object2, Date %in% as.POSIXct(date, format = date.format))
    
    # plot(object2$DateTime, object2$temp, type="p", ylim =c((min(object2$temp) - 3), (max(object2$temp) + 5)), pch = 19, cex = 0.3) # for developping
    plot(object2$DateTime, object2$temp, type="p", ylim =c((min(object2$temp) - 3), (max(object2$temp) + 5)), ...)
    lines(object2$DateTime, object2$temp, type="l")
  }
  
  # To plot everything
  if(is.na(date) & is.na(date.format) & is.na(from) & is.na(to) & is.na(time.format) & is.na(nbout) & plot.all == TRUE){  
    object2 <- object
    
    plot(object2$DateTime, object2$temp, type="p", ylim =c(min(object2$temp) - 1, (max(object2$temp) + 5)), ...)
    lines(object2$DateTime, object2$temp, type="l")
  }
  
  # Adding off window, numbers and threshold
  if(off.window == TRUE){
    offbout <- unique(object2[object2$num < 0,]$num)
    for(i in offbout){
      offbout2 <- subset(object2, num == i) 
      a = min(offbout2$DateTime) - tdif*60
      rect(a, min(object2$temp) ,max(offbout2$DateTime), max(object2$temp), col = rgb(0, 0.5, 1, alpha=0.3))
      if(off.window.num == TRUE){
        text(min(offbout2$DateTime), min(object2$temp), labels = i, bg = "white", cex = 0.8, col = "red")
      }
      if(plot.threshold == TRUE){
        text((min(offbout2$DateTime) + 0.5), (max(offbout2$temp) + 0.5), labels = round(offbout2[1, "sum"], digits = 2), bg = "white", cex = 0.5, col = "black")
      }
    }
  }
  
  if(on.window.num == TRUE){
    onbout <- unique(object2[object2$num > 0,]$num)
    for(i in onbout){
      onbout2 <- subset(object2, num == i) 
      text(min(onbout2$DateTime), min(object2$temp) - 1, labels = i, bg = "white", cex = 0.8, col = "blue")
      if(plot.threshold == TRUE){
        text((min(onbout2$DateTime) + 0.5), (min(onbout2$temp) - 0.5), labels = round(onbout2[1, "sum"], digits = 2), bg = "white", cex = 0.5, col = "black")
      }
    } 
  }
  # Adding numpos and numneg
  if(numpos == TRUE){
    Znumpos <- na.exclude(unique(object2$numpos))
    for(i in Znumpos){
      numpos2 <- subset(object2, numpos == i) 
      a = min(numpos2$DateTime) - tdif*60
      abline(v = a, col = "blue", lwd = 1)
    }
  }
  
  if(numneg == TRUE){
    Znumneg <- na.exclude(unique(object2$numneg))
    for(i in Znumneg){
      numneg2 <- subset(object2, numneg == i) 
      a = min(numneg2$DateTime) - tdif*60
      abline(v = a, col = "red", lwd = 1)
    }
  }
  
  if(numpos == TRUE){
    Znumpos <- na.exclude(unique(object2$numpos))
    for(i in Znumpos){
      numpos2 <- subset(object2, numpos == i) 
      a = min(numpos2$DateTime) - tdif*60
      shadowtext(a, (max(object2$temp) + 1), labels = i, bg = "white", col = "blue", cex = 0.8)
    }
  }
  
  if(numneg == TRUE){
    Znumneg <- na.exclude(unique(object2$numneg))
    for(i in Znumneg){
      numneg2 <- subset(object2, numneg == i) 
      a = min(numneg2$DateTime) - tdif*60
      shadowtext(a, (max(object2$temp) + 3), labels = i, bg = "white", col = "red", cex = 0.8)
    }
  }
  
}
