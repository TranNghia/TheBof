########
#Same as the script named fanie_0.36.3.r but modified by Nghia Tran
#Nghia Tran oct 28th 2016. tran.ng91@gmail.com

# rm(list=ls())

#Argugument Legends
#bof()
#object: data frame containing the data to be analysed.
#pos: column number containg the temperature readings.
#control: data frame containing ambiant temperature data. Readings do not need to be at the same interval than object.
#pT: positive temperature threshold.
#nT: negative temperature threshold. Must be negative. 
#plag: positive minimal onbout duration (in reading e.g. to set this value to 4 mins; if obs.lag = 2, plag = 2 ==> 2x2 minutes)
#nlag: negative minimal offbout duration (in reading e.g. to set this value to 4 mins; if obs.lag = 2, plag = 2 ==> 2x2 minutes)
#obs.lag: time between reading (minutes) of the raw data. 
#time.format: Time format for the table to be evaluated. See ?strptime() for formating specifications.
#crit: To be specify if a control temperature is used. Mininal difference in temperature between ambiant and nest temperature.
#inctemp: This argument is used for the first reading or of the first reading when the time serie is disrupted (e.g. night were removed prior to analysing with bof())
#         if the first reading of a new time serie is above or equal to this value (inctemp), typ will be in. Otherwise, it is out.
#nwindow: Within a cooling period, the maximum time (in reading) that the threshold (nT) needs to be attained for the program to detect an offbout.
#pwindow: Within a warming period, the maximum time (in reading) that the threshold (pT) needs to be attained for the program to detect an onbout.  
#nullwindow: If some consecutive temperature reading are of the same values, those reading can be included in the previous warming/cooling period.  
#            The maximum number of reading with no temperature change allowed to be considered in the previous warming/cooling period.


#Output Table Legend
#DateTime
#temp
#tdif: Time difference between line i and i-1 of the data frame object. (Time difference (in minutes) between a given reading and the previous reading)
#dif: Reading difference between line i and i-1 of the data frame object. (difference between a given reading and the previous reading)
#numpos: Warming period sequence number
#numneg: Cooling period sequence number
#numnull: Stable period sequence number
#sum: Temperature difference over the period (warming or cooling period)
#le: Length of the period (warming or cooling period)
#pTle: Within a warming period, the number of reading before the threshold (pT) was reached
#nTle: Within a cooling period, the number of reading before the threshold (nT) was reached
#typ: Whether the program has detected that the bird is inside or outside the nestbox
#num: sequence numbers for the bouts. (>0: on bout, <0: off bout)

#Debugger
# object <- read.table("TestData.csv", header = TRUE, sep = ",")
# pos=2; control=NA; pT=1; nT=-1.4; plag=2; nlag=2; obs.lag=2; time.format="%Y-%m-%d %H:%M:%S" ;crit=1; inctemp = 25; nwindow = 5; pwindow = 6; nullwindow = 2

#' On-bouts and off-bout detection.
#' 
#' \code{bof} Parses a time serie + temperature data set to detect inncubation bouts
#' 

###fonction
bof <- function(object,pos=2,control=NA,pT=1,nT=-1,plag=2,nlag=2,obs.lag=2,time.format="%Y-%m-%d %H:%M:%S",crit=1,inctemp = 25, nwindow = 5, pwindow = 6, nullwindow = 1,...){ 
  
  colnames(object)[pos] <- "temp"
  le=nrow(object)
  object$DateTime <- as.POSIXct(strptime(object$DateTime, format=time.format))
  row.names(object) <- c(1:nrow(object))
  
  object$tdif <- c(NA, difftime(object$DateTime[2:le], object$DateTime[1:(le-1)], units = "mins")) #Changed by NT
  
  ##### Modified/added by NT
  
  if (is.na(control)){
    
    object$dif <- c(NA, (object$temp[2:le] - object$temp[1:(le-1)]))
    object[object$tdif > obs.lag | is.na(object$tdif), ]$dif <- NA
  }   
  #Let's try to eliminate this loop, DONE (see above)
  # for(i in 2:le){
  #   if(object$tdif[i] <= obs.lag & !is.na(object$tdif[i])){
  #     object$dif[i] <- object$temp[i]-object$temp[i-1]
  #   }
  # }
  
  ####
  #This part was not used by NT
  
  # if (!is.na(control))  {
  #   control$DateTime <- as.POSIXct(strptime(control$DateTime,format=time.format))
  #   cont<-interpSpline(Reading~DateTime,control) ###############
  #   object$con <- predict(cont,as.vector(object$DateTime))$y
  #   object$dif0 <- object$temp-object$con
  #   object$co <- ifelse(abs(object$dif0)<=crit,1,0)
  #   #         object <- subset(object,object$co==0) 
  #   object$temp <- object$dif0  ###      ligne added by gab
  #   object$dif=c(NA,object$temp[2:le]-object$temp[1:(le-1)])
  # }
  
  ##### Sequencing the period for which dif == 0
  
  kk <- 1
  dat <- subset(object, object[,"dif"] == 0 & is.na(object[, "dif"]) == FALSE & is.na(object[, "tdif"]) == FALSE )
  # dat <- object[object$dif == 0 & !is.na(object$dif) & !is.na(object$tdif), ]
  # dat <- base::subset(object, object$dif == 0 & is.na(object$dif) == FALSE & is.na(object$tdif) == FALSE )
  rn <- as.numeric(rownames(dat))
  
  if(length(rn) > 0){
    
    if(length(rn) == 1){object$numnull[rn] <- 1
    }else{
      Z <- vector()
      Z[1] <- 1
      for( i in 2:nrow(dat) ){
        if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] == obs.lag) { Z[i] <- kk }
        else { kk <- kk+1; Z[i] <- kk }
      }
      dat$Z <- Z
      object$numnull <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)
    }
  }
  ########
  #Including the null section or not to previous sections (nullwindow argument)
  
  nulllength <- aggregate(object$numnull, list(object$numnull), length)
  colnames(nulllength) <- c("numnull", "lenull")
  object$lenull <- nulllength[match(object$numnull, nulllength$numnull), "lenull"]
  rm(nulllength)
  
  for(i in 2:nrow(object)){
    if(!is.na(object$lenull[i]) & !is.na(object$dif[i-1]) & object$lenull[i] <= nullwindow){
      if(object$dif[i-1] < 0) {object$dif[i] <- -1e-6}
      if(object$dif[i-1] > 0) {object$dif[i] <- 1e-6}
    }
  }
  
  object <- object[ , -c(5,6)]
  
  #########
  #Sequencing the warming periods
  
  kk <- 1
  dat <- subset(object,object[,"dif"] > 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )
  rn <- as.numeric(rownames(dat))
  Z <- vector()
  Z[1] <- 1
  for( i in 2:nrow(dat) ){
    if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] <= obs.lag) { Z[i] <- kk }
    else { kk <- kk+1; Z[i] <- kk }
  }
  dat$Z <- Z
  object$numpos <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)
  
  #Sequencing the cooling periods
  
  dat <- subset(object,object[,"dif"] < 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE)
  rn <- as.numeric(rownames(dat))
  Z <- vector()
  Z[1] <- 1
  kk <- 1
  for (i in 2:nrow(dat)){
    if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] <= obs.lag ){ Z[i] <- kk }
    else {kk <- kk+1; Z[i] <- kk}
  }
  dat$Z <- Z
  object$numneg <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)
  
  #Resequencing the stable periods
  kk <- 1
  object$numnull <- as.integer(NA) 
  dat <- subset(object,object[,"dif"] == 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )
  rn <- as.numeric(rownames(dat))
  
  if(length(rn) > 0){
    
    if(length(rn) == 1){object$numnull[rn] <- 1
    }else{
      Z <- vector()
      Z[1] <- 1
      for( i in 2:nrow(dat) ){
        if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] == obs.lag) { Z[i] <- kk }
        else { kk <- kk+1; Z[i] <- kk }
      }
      dat$Z <- Z
      object$numnull <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)
    }
  }
  ########
  #Calculate cooling and warming periods length
  
  neg <- with(object[is.na(object$numneg)==F,],matrix(c(tapply(dif,numneg,sum),tapply(dif,numneg,length)),byrow=FALSE,ncol=2))
  pos <- with(object[is.na(object$numpos)==F,],matrix(c(tapply(dif,numpos,sum),tapply(dif,numpos,length)),byrow=FALSE,ncol=2))
  object$sum <- ifelse(is.na(object$numneg)==FALSE,neg[match(object$numneg,row(neg)),1],pos[match(object$numpos,row(pos)),1])
  object$le <- ifelse(is.na(object$numneg)==FALSE,neg[match(object$numneg,row(neg)),2],pos[match(object$numpos,row(pos)),2])
  
  
  #NT: We need some way to specify to the program that the threshold must be reached whitin a given time window. 
  #T could rise and reach the threshold but if it takes an hour to reach this therhold, it's likely not due to a bird starting a bout
  ########
  
  object$pTle <- as.integer(NA) #This will become the number of reading it took within a bout (numpos or numneg) to reach nT or pT
  z <- unique(na.exclude(object$numpos))
  
  for(i in z){
    # i = z[2]
    a <- cumsum(object$dif[object$numpos == i & !is.na(object$numpos)]) # A vector of cumulative sum in temperature difference
    
    b <- object$sum[object$numpos == i & !is.na(object$numpos)][1] #The temperature rise within this warming period
    
    if(b > pT){
      object$pTle[object$numpos == i & !is.na(object$numpos)] <- min(which(a > pT))
    }
  }
  
  object$nTle <- as.integer(NA) #This will become the number of reading it took within a bout (numpos or numneg) to reach nT or pT
  z <- unique(na.exclude(object$numneg))
  for(i in z){
    # i = z[2]
    a <- cumsum(object$dif[object$numneg == i & !is.na(object$numneg)])
    
    b <- object$sum[object$numneg == i & !is.na(object$numneg)][1]
    if(b < nT){
      object$nTle[object$numneg == i & !is.na(object$numneg)] <- min(which(a < nT))
    }
  }
  
  #########
  #Finding on and off bout starts
  
  object$typ <- ifelse(object$sum > pT & object$le >= plag & object$pTle <= pwindow, "in",
                       ifelse(object$sum < nT & object$le >= nlag & object$nTle <= nwindow, "out",  NA) )
  
  
  ######
  #for disrupted time series. The first bout fallowing a disrupted time serie is determined using a temperature threshold (argument inctemp).
  
  object$typ[1] <- ifelse(object$temp[1] >= inctemp,  "in", "out" )
  
  for(i in 2:nrow(object)){
    
    if(object$tdif[i] > obs.lag & !is.na(object$tdif[i])){
      
      object$typ[i] <- ifelse(object$temp[i] >= inctemp,  "in", "out" )
    }
  }
  
  #######
  # Filling the blank for typ ==> using the previous available !is.na value
  rn <- as.numeric(rownames(object[is.na(object$typ),]))
  for(i in 1:length(rn)){
    object$typ[rn[i]] <- object$typ[rn[i]-1]
  }
  
  #Sequencing the bouts
  pk=0
  nk=0
  object$num <- as.integer(NA)
  if(object$typ[1] == "out"){nk <- nk-1 ; object$num[1] <- nk
  
  }else{pk <- pk+1 ; object$num[1] <- pk}
  
  
  for (j in 2:le){
    
    # j = 2
    
    if(object$typ[j]=="out" & object$tdif[j] > obs.lag) {nk <- nk - 1 ; object$num[j] <- nk}
    
    if(object$typ[j]=="in" & object$tdif[j] > obs.lag) {pk <- pk + 1 ; object$num[j] <- pk  
    
    }else{ 
      
      if (object$typ[j]=="out" & object$typ[j]==object$typ[j-1]) {object$num[j] <- nk}
      if (object$typ[j]=="out" & object$typ[j]!=object$typ[j-1]) {nk <- nk - 1 ; object$num[j] <- nk }
      
      if (object$typ[j]=="in" & object$typ[j]==object$typ[j-1]) {object$num[j] <- pk}
      if (object$typ[j]=="in" & object$typ[j]!=object$typ[j-1]) {pk <- pk + 1 ; object$num[j] <- pk}   
    }      
  }
  
  class(object) <- c("bof","data.frame")
  object
}

#
summ.bof <- function(object,inter=2){
  if (!inherits(object, "bof"))
    stop("use only with \"bof\" object")
  Xo <- with(object[object$typ=="out",],tapply(temp,num,length))*inter
  O <- with(object[object$typ=="out",],tapply(temp,num,min))
  xo <- object$temp[object$typ=="out"]
  Xi <- with(object[object$typ=="in",],tapply(temp,num,length))*inter
  I <- with(object[object$typ=="in",],tapply(temp,num,max))
  xi <- object$temp[object$typ=="in"]
  out <- data.frame(
    N=c(length(xi),length(xo),nrow(object)),
    Time=c(length(xi),length(xo),nrow(object))*inter,
    Time.p.c. = c(length(xi),length(xo),nrow(object))/nrow(object)*100,
    N.bout=c(-min(object$num),max(object$num),sum(abs(range(object$num)))),
    T.mean=c(mean(xi),mean(xo),mean(object$temp)),
    T.var=c(var(xi),var(xo),var(object$temp)),
    minT=c(min(xi),min(xo),min(object$temp)),
    maxT=c(max(xi),max(xo),max(object$temp)),
    B.L.mean=c(mean(Xi),mean(Xo),NA),
    B.L.var=c(var(Xi),var(Xo),NA),
    B.L.max=c(max(Xi),max(Xo),NA),
    maxBL = c(paste(names(which(Xi==max(Xi))),collapse=","),paste(names(which(Xo==max(Xo))),collapse=","),NA),
    meanT.peak=c(mean(I),mean(O),mean(object$temp)),
    varT.peak=c(var(I),var(O),var(object$temp)),
    minT.peak=c(min(I),min(O),min(object$temp)),
    maxT.peak=c(max(I),max(O),max(object$temp)),
    row.names=c("in","out","total") 
  ) 
  out
}

#fonction globale

glob <- function(path=NA,output.file="out.csv",control.file="",pos=3,pT=2,nT=-3.9,plag=2,nlag=2,obs.lag=2,crit=1,time.format="%Y/%m/%d %H:%M:%S",start.bout="out",...){
  if (!is.na(path))
    setwd(path)
  myFiles <- list.files()
  contro <- ifelse(control.file %in% myFiles,read.csv(control.file),NA)
  out <- NULL  
  for (currFile in myFiles) {
    if (currFile == control.file)
    {}
    if (currFile == output.file)
    {}
    else {
      currTab<- read.csv(currFile)
      m <- bof(currTab,pos=pos,control=contro,pT=pT,nT=nT,plag=plag,nlag=nlag,obs.lag=obs.lag,time.format=time.format,crit=crit)
      x <- as.data.frame(t(summ.bof(m,obs.lag)))
      x$file <- rep(currFile,nrow(x))
      out<-rbind(out,x)
      rm(currTab,m,x)
    }
  }
  write.table(out, file=output.file, sep=",", dec=".", quote=FALSE) 
  cat(paste("Results were saved in ",getwd(),"/",output.file,sep=""),"\n")
  invisible(out)}  

#plot

#NT:  
#object: an output table from the bof function
#nbout: the bout that needs to be printed out. More than one bout can be visualized at once. If so, negative and postive
#numbers must be specifed. ex. to view from bout 44 to 50; nbout = c(44:50, -44:-50)
# object = iB.bof ; tdif = 2; nbout = NA; from = NA; to = NA; time.format = NA; date = NA; date.format = "%Y-%m-%d"; plot.all = FALSE; off.window.num = TRUE; threshold = TRUE

plot.bof<-function(object, tdif = 2, nbout = NA, from = NA, to = NA, time.format = NA, date = NA, date.format = NA, plot.all = TRUE , off.window = TRUE, off.window.num = TRUE,...){
  
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

#Function created by Nghia Tran (2016 nov. 1st)
#Creates a table from an object processed by bof()

table.bof <- function(object, obs.lag = 2){
  
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
  
  
  table <- data.frame(EndTime$Group.1, BeginTime$x, EndTime$x, Duration, minT$x, maxT$x, mean$x, depth$x ,typ$x, strftime(DayStart$x, format = "%H:%M:%S"), strftime(DayEnd$x, format = "%H:%M:%S"))
  colnames(table) <- c("nbout", "BeginTime", "EndTime", "duration","minT", "maxT","meanT", "depthT","typ", "DayStart", "DayEnd")
  table$duration <- as.numeric(table$EndTime - table$BeginTime)
  table <- table[order(table$BeginTime),]
  
  return(table)
}

# Create plots that help visual inspection/correction

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
      rect(a, min(object2$temp) ,max(offbout2$DateTime), max(object2$temp), col = rgb(0, 0.5, 1, alpha=0.1))
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


# A function to help correcting an object processed with bof()
bof.correct <- function(object, numposCorrect = NA, numnegCorrect = NA, correctTo = NA, obs.lag = 2){
  
  if(!is.na(numposCorrect) & is.na(numnegCorrect)){
    object$typ[object$numpos == numposCorrect & !is.na(object$numpos)] <- correctTo
  }
  
  if(!is.na(numnegCorrect) & is.na(numposCorrect)){
    object[object$numneg == numnegCorrect & !is.na(object$numneg),]$typ <- correctTo
  }
  
  # ReSequencing the bouts
  pk=0
  nk=0
  object$num <- as.integer(NA)
  if(object$typ[1] == "out"){nk <- nk-1 ; object$num[1] <- nk
  
  }else{pk <- pk+1 ; object$num[1] <- pk}
  
  
  for (j in 2:nrow(object)){
    
    # j = 2
    
    if(object$typ[j]=="out" & object$tdif[j] > obs.lag) {nk <- nk - 1 ; object$num[j] <- nk}
    
    if(object$typ[j]=="in" & object$tdif[j] > obs.lag) {pk <- pk + 1 ; object$num[j] <- pk  
    
    }else{ 
      
      if (object$typ[j]=="out" & object$typ[j]==object$typ[j-1]) {object$num[j] <- nk}
      if (object$typ[j]=="out" & object$typ[j]!=object$typ[j-1]) {nk <- nk - 1 ; object$num[j] <- nk }
      
      if (object$typ[j]=="in" & object$typ[j]==object$typ[j-1]) {object$num[j] <- pk}
      if (object$typ[j]=="in" & object$typ[j]!=object$typ[j-1]) {pk <- pk + 1 ; object$num[j] <- pk}   
    }      
  }
  
  class(object) <- c("bof","data.frame")
  object
  
}


