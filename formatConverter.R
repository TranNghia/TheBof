# A function to convert a space delimited file to a format that can be process with bof. 
# Because the function uses a DateTime column instead of 2 separeted column for date and time

DayNightData <- read.table("DayNightData.txt", header = FALSE)

#Debugger
# table = DayNightData; date.col = 1; time.col = 2; temp.col = 3; date.format = "%m/%d/%y"; time.format = "%H:%M:%S"; output.format = "%Y-%m-%d %H:%M:%S"

txt.converter <- function(table = NULL, date.col = 1, time.col = 2, temp.col = 3, date.format = NULL, time.format = NULL, output.format = "%Y-%m-%d %H:%M:%S") {
  
  DateTime <- strptime(paste(table[ , date.col], table[ , time.col], sep = " "), format = paste(date.format, time.format, sep = " "))
  
  table2 <- data.frame(strftime(DateTime, format = output.format), table[ , temp.col])
  colnames(table2) <- c("DateTime", "Reading")
  
  table2
}

