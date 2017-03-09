# The main function. This one takes an table containing 2 columns (DateTime, Temperature Values) and determines off/on bouts to create a bof object.

# rm(list=ls())

# Debugger
# object <- read.table("DayData.csv", header = TRUE, sep = ",")
# pos=2; control=NA; pT=1; nT=-1.4; plag=2; nlag=2; obs.lag=2; time.format="%Y-%m-%d %H:%M:%S" ;crit=1; inctemp = 25; nwindow = 5; pwindow = 6; nullwindow = 0


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
#           The maximum number of reading with no temperature change allowed to be considered in the previous warming/cooling period.


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

#' On-bouts and off-bout detection.
#'
#' \code{bof} Parses a time serie + temperature data set to detect inncubation bouts
#'


bof <- function(object,pos=2,control=NA,pT=1,nT=-1,plag=2,nlag=2,obs.lag=2,time.format="%Y-%m-%d %H:%M:%S",crit=1,inctemp = 25, nwindow = 5, pwindow = 6, nullwindow = 1,...){

  colnames(object)[pos] <- "temp"
  le=nrow(object)
  object$DateTime <- as.POSIXct(strptime(object$DateTime, format=time.format))
  # row.names(object) <- c(1:nrow(object)) # instead of using row.names, will use a column as this bug when using fread
  object$rn <- 1:nrow(object)

  object$tdif <- c(NA, difftime(object$DateTime[2:le], object$DateTime[1:(le-1)], units = "mins"))

  if (is.na(control)){

    object$dif <- c(NA, (object$temp[2:le] - object$temp[1:(le-1)]))
    object[object$tdif > obs.lag | is.na(object$tdif), ]$dif <- NA
  }

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

  # Sequencing the period for which dif == 0 ####
  
  dat <- subset(object,object[,"dif"] == 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )
  if(nrow(dat) == 0) {object$numnull <- NA}
  if(nrow(dat > 0)) {
    rn2 <- c(NA, (dat[c(1:nrow(dat)-1), "rn"] + 1))
    dat <- cbind(dat, rn2)
    dat$eq <- ifelse(dat$rn == dat$rn2 & dat$tdif <= obs.lag, 0, 1)
    dat$eq[1] <- 1
    dat$Z <- cumsum(dat$eq)

    object$numnull <- ifelse(object$rn %in% dat$rn, dat$Z[match(object$rn, dat$rn)], NA)
 
    # Including the null section or not to previous sections (nullwindow argument) ####

    nulllength <- aggregate(object$numnull, list(object$numnull), length)
    colnames(nulllength) <- c("numnull", "lenull")
    object$lenull <- nulllength[match(object$numnull, nulllength$numnull), "lenull"]
    rm(nulllength)

    #
    # for(i in 2:nrow(object)){
    #   if(!is.na(object$lenull[i]) & !is.na(object$dif[i-1]) & object$lenull[i] <= nullwindow){
    #     if(object$dif[i-1] < 0) {object$dif[i] <- -1e-6}
    #     if(object$dif[i-1] > 0) {object$dif[i] <- 1e-6}
    #   }
    # }

    rn <- object[object$lenull <= nullwindow & !is.na(object$lenull), "rn"]
    if(length(rn) > 0) {
      rn2 <- rn - 1
      df.null <- data.frame(rn, object[object$rn %in% rn2, "dif"], object[object$rn %in% rn, "dif"])
      colnames(df.null) <- c("rn", "dif.rn2", "dif")
      
      df.null[df.null$dif.rn2 > 0 & !is.na(df.null$dif.rn2), "dif"] <- as.numeric(1e-6)
      df.null[df.null$dif.rn2 < 0 & !is.na(df.null$dif.rn2), "dif"] <- as.numeric(-(1e-6))
  
      object[object$rn %in% rn, "dif"] <- df.null$dif
  
      object <- object[ ,-(which(colnames(object) == "lenull"))]
      rm(df.null)
    }
  }  
# Sequencing the warming periods ####

  dat <- subset(object,object[,"dif"] > 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )

  if(nrow(dat > 0)) {
    rn2 <- c(NA, (dat[c(1:nrow(dat)-1), "rn"] + 1))
    dat <- cbind(dat, rn2)
    dat$eq <- ifelse(dat$rn == dat$rn2 & dat$tdif <= obs.lag, 0, 1)
    dat$eq[1] <- 1
    dat$Z <- cumsum(dat$eq)

    object$numpos <- ifelse(object$rn %in% dat$rn, dat$Z[match(object$rn, dat$rn)], NA)
  }

  # rn <- as.numeric(rownames(dat))
  # Z <- vector()
  # Z[1] <- 1
  # for( i in 2:nrow(dat) ){
  #   if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] <= obs.lag) { Z[i] <- kk }
  #   else { kk <- kk+1; Z[i] <- kk }
  # }
  # dat$Z <- Z

  # object$numpos <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)




  # Sequencing the cooling periods ####

  # dat <- subset(object,object[,"dif"] < 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE)
  # rn <- as.numeric(rownames(dat))
  # Z <- vector()
  # Z[1] <- 1
  # kk <- 1
  # for (i in 2:nrow(dat)){
  #   if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] <= obs.lag ){ Z[i] <- kk }
  #   else {kk <- kk+1; Z[i] <- kk}
  # }
  # dat$Z <- Z
  # object$numneg <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)
  #

  dat <- subset(object,object[,"dif"] < 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )

  if(nrow(dat > 0)) {
    rn2 <- c(NA, (dat[c(1:nrow(dat)-1), "rn"] + 1))
    dat <- cbind(dat, rn2)
    dat$eq <- ifelse(dat$rn == dat$rn2 & dat$tdif <= obs.lag, 0, -1)
    dat$eq[1] <- -1
    dat$Z <- cumsum(dat$eq)

    object$numneg <- ifelse(object$rn %in% dat$rn, dat$Z[match(object$rn, dat$rn)], NA)
  }

  # Resequencing the stable periods ####
  # kk <- 1
  # object$numnull <- as.integer(NA)
  # dat <- subset(object,object[,"dif"] == 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )
  # rn <- as.numeric(rownames(dat))
  #
  # if(length(rn) > 0){
  #
  #   if(length(rn) == 1){object$numnull[rn] <- 1
  #   }else{
  #     Z <- vector()
  #     Z[1] <- 1
  #     for( i in 2:nrow(dat) ){
  #       if( rn[i] == I(rn[i-1]+1) & dat$tdif[i] == obs.lag) { Z[i] <- kk }
  #       else { kk <- kk+1; Z[i] <- kk }
  #     }
  #     dat$Z <- Z
  #     object$numnull <- ifelse(rownames(object) %in% rownames(dat),dat$Z[match(rownames(object),rownames(dat))],NA)
  #   }
  # }

  dat <- subset(object,object[,"dif"] == 0 & is.na(object[,"dif"]) == FALSE & is.na(object[,"tdif"]) == FALSE )

  if(nrow(dat) > 0) {
    rn2 <- c(NA, (dat[c(1:nrow(dat)-1), "rn"] + 1))
    dat <- cbind(dat, rn2)
    dat$eq <- ifelse(dat$rn == dat$rn2 & dat$tdif <= obs.lag, 0, 1)
    dat$eq[1] <- 1
    dat$Z <- cumsum(dat$eq)

    object$numnull <- ifelse(object$rn %in% dat$rn, dat$Z[match(object$rn, dat$rn)], NA)
  }

  # Calculate cooling and warming periods length ####

  neg <- with(object[is.na(object$numneg)==F,], matrix(c(sort(na.exclude(unique(object$numneg)), decreasing = FALSE),tapply(dif,numneg,sum),tapply(numneg,numneg,length)),byrow=FALSE,ncol=3))
  colnames(neg) <- c("numneg", "sum", "le"); neg <- data.frame(neg)
  pos <- with(object[is.na(object$numpos)==F,], matrix(c(na.exclude(unique(object$numpos)),tapply(dif,numpos,sum),tapply(dif,numpos,length)),byrow=FALSE,ncol=3))
  colnames(pos) <- c("numpos", "sum", "le"); pos <- data.frame(pos)

  object$sum <- ifelse(is.na(object$numneg)==FALSE, neg[match(object$numneg,neg$numneg), "sum"], pos[match(object$numpos,pos$numpos), "sum"])
  object$le <- ifelse(is.na(object$numneg)==FALSE, neg[match(object$numneg,neg$numneg), "le"], pos[match(object$numpos,pos$numpos), "le"])

  # object$sum <- ifelse(is.na(object$numneg)==FALSE, neg[match(object$numneg,row(neg)),1], pos[match(object$numpos,row(pos)),1])
  # object$le <- ifelse(is.na(object$numneg)==FALSE, neg[match(object$numneg,row(neg)),2], pos[match(object$numpos,row(pos)),2])


  #NT: We need some way to specify to the program that the threshold must be reached whitin a given time window.
  #T could rise and reach the threshold but if it takes an hour to reach this therhold, it's likely not due to a bird starting a bout
  # pTle ####

  # object$pTle <- as.integer(NA) #This will become the number of reading (i.e. "time") it took within a bout (numpos or numneg) to reach nT or pT
  # z <- unique(na.exclude(object$numpos))
  #
  # for(i in z){
  #   # i = z[2]
  #   a <- cumsum(object$dif[object$numpos == i & !is.na(object$numpos)]) # A vector of cumulative sum in temperature difference
  #
  #   b <- object$sum[object$numpos == i & !is.na(object$numpos)][1] #The temperature rise within this warming period
  #
  #   if(b > pT){
  #     object$pTle[object$numpos == i & !is.na(object$numpos)] <- min(which(a > pT))
  #   }
  # }

  #
  dat <- object[object$sum >= pT & !is.na(object$sum), ]
  if(nrow(dat) > 0){
    if(length(unique(dat$numneg)) > 1) {
      cumsum <- aggregate(dat$dif, by = list(dat$numpos), FUN = cumsum)
      dat$cumsum <- unlist(cumsum$x)
    } else {
      cumsum <- cumsum(dat$dif)
      dat$cumsum <- cumsum
    }    
    dat$a <- ifelse(dat$cumsum >= pT, 0, 1)
    sum <- aggregate(dat$a, by = list(dat$numpos), sum)
    colnames(sum) <- c("numpos", "pTle")
  
    object$pTle <- ifelse(object$numpos %in% sum$numpos, sum$pTle[match(object$numpos, sum$numpos)], NA)
    object[!is.na(object$pTle), "pTle"] <- object[!is.na(object$pTle), "pTle"] + 1
    
  }else{
    object$pTle <- NA  
    }
  
  # nTle ####
  # object$nTle <- as.integer(NA) #This will become the number of reading it took within a bout (numpos or numneg) to reach nT or pT
  # z <- unique(na.exclude(object$numneg))
  # for(i in z){
  #   # i = z[2]
  #   a <- cumsum(object$dif[object$numneg == i & !is.na(object$numneg)])
  #
  #   b <- object$sum[object$numneg == i & !is.na(object$numneg)][1]
  #   if(b < nT){
  #     object$nTle[object$numneg == i & !is.na(object$numneg)] <- min(which(a < nT))
  #   }
  # }
  
  dat <- object[object$sum <= nT & !is.na(object$sum), ]
  if (nrow(dat) > 0) {
    if(length(unique(dat$numneg)) > 1) {
      cumsum <- aggregate(dat$dif, by = list(dat$numneg), FUN = cumsum)
      cumsum <- cumsum[order(cumsum$Group.1, decreasing = TRUE), ]
      dat$cumsum <- unlist(cumsum$x)
    } else {
      cumsum <- cumsum(dat$dif)
      dat$cumsum <- cumsum
    }
    
    dat$a <- ifelse(dat$cumsum <= nT, 0, 1)
    sum <- aggregate(dat$a, by = list(dat$numneg), sum)
    colnames(sum) <- c("numneg", "nTle")
  
    object$nTle <- ifelse(object$numneg %in% sum$numneg, sum$nTle[match(object$numneg, sum$numneg)], NA)
    object[!is.na(object$nTle), "nTle"] <- object[!is.na(object$nTle), "nTle"] + 1
    
  }else{
    object$nTle <- NA  
  }

  # Finding on and off bout starts ####

  object$typ <- ifelse(object$sum >= pT & object$le >= plag & object$pTle <= pwindow, "in",
                       ifelse(object$sum <= nT & object$le >= nlag & object$nTle <= nwindow, "out",  NA) )


  # Disrupted time series ####
  # for disrupted time series. The first bout fallowing a disrupted time serie is determined using a temperature threshold (argument inctemp).

  object$typ[1] <- ifelse(object$temp[1] >= inctemp,  "in", "out" )

  object[object$tdif > obs.lag & !is.na(object$tdif), "typ"] <- ifelse(object[object$tdif > obs.lag & !is.na(object$tdif), "temp"] >= inctemp, "in", "out")

  # for(i in 2:nrow(object)){
  #
  #   if(object$tdif[i] > obs.lag & !is.na(object$tdif[i])){
  #
  #     object$typ[i] <- ifelse(object$temp[i] >= inctemp,  "in", "out" )
  #   }
  # }

  # Filling the blank for typ ####
  # using the previous available !is.na value
  # rn <- as.numeric((object[is.na(object$typ), "rn"]))
  # for(i in 1:length(rn)){
  #   object$typ[rn[i]] <- object$typ[rn[i]-1]
  # }
  #
  #

  rn <- object[!is.na(object$typ), "rn"]
  object$typ <- rep(object$typ[rn], times = diff(c(rn, nrow(object) + 1)))




  # Sequencing the bouts ####
  # pk=0
  # nk=0
  # object$num <- as.integer(NA)
  # if(object$typ[1] == "out"){nk <- nk-1 ; object$num[1] <- nk
  #
  # }else{pk <- pk+1 ; object$num[1] <- pk}
  #
  #
  # for (j in 2:le){
  #
  #   # j = 2
  #
  #   if(object$typ[j]=="out" & object$tdif[j] > obs.lag) {nk <- nk - 1 ; object$num[j] <- nk}
  #
  #   if(object$typ[j]=="in" & object$tdif[j] > obs.lag) {pk <- pk + 1 ; object$num[j] <- pk
  #
  #   }else{
  #
  #     if (object$typ[j]=="out" & object$typ[j]==object$typ[j-1]) {object$num[j] <- nk}
  #     if (object$typ[j]=="out" & object$typ[j]!=object$typ[j-1]) {nk <- nk - 1 ; object$num[j] <- nk }
  #
  #     if (object$typ[j]=="in" & object$typ[j]==object$typ[j-1]) {object$num[j] <- pk}
  #     if (object$typ[j]=="in" & object$typ[j]!=object$typ[j-1]) {pk <- pk + 1 ; object$num[j] <- pk}
  #   }
  # }
  #
  ####
  dat <- data.frame(object$rn, object$tdif, c(object[ , "typ"]), c(NA, object[c(1:I(nrow(object)-1)), "typ"]))
  colnames(dat) <- c("rn", "tdif","a", "b")

  dat$c <- 0
  dat$d <- 0

  dat[dat$a == "in" & dat$tdif > obs.lag & !is.na(dat$tdif), "c"] <- 1
  dat[dat$a == "in" & dat$b == "out" & dat$tdif <= obs.lag & !is.na(obs.lag) & !is.na(dat$b), "c"] <- 1

  dat[dat$a == "out" & dat$tdif > obs.lag & !is.na(dat$tdif), "d"] <- -1
  dat[dat$a == "out" & dat$b == "in" & dat$tdif <= obs.lag & !is.na(obs.lag) & !is.na(dat$b), "d"] <- -1

  dat[1, "c"] <- ifelse(dat[1, "a"] == "in", 1, 0)
  dat[1, "d"] <- ifelse(dat[1, "a"] == "out", -1, 0)

  dat$cumc <- c(cumsum(dat[, "c"]))
  dat$cumd <- c(cumsum(dat[, "d"]))

  object$num <- ifelse(object$typ == "in", dat$cumc[match(object$rn, dat$rn)], dat$cumd[match(object$rn, dat$rn)])
  ####


  class(object) <- c("bof","data.frame")
  object[,-which(colnames(object) == "rn")]
  }
