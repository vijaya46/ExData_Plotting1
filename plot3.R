plot3 <- function() {
  png(filename = "plot3.png", width = 480, height = 480,
      units = "px", bg = "white", res = NA)
  ## Read outcome data
  data <- read.table("pa1data.txt", header=TRUE, sep=";", colClasses = "character")
  
  # now subset for the relevant days
  relevantdata <- subset(data, data$Date == "1/2/2007" || data$Date == "2/2/2007")
  
  #convert the Date from String to Date type
  relevantdata$Date = as.Date(relevantdata$Date, "%d/%m/%Y")
  
  ##combine date and time into one column
  relevantdata <- transform(relevantdata, timestamp=format(as.POSIXct(paste(relevantdata$Date, relevantdata$Time)), 
                                                           "%d/%m/%Y %H:%M:%S"))
  
  # convert timestamp here
  relevantdata$timestamp <- strptime(relevantdata$timestamp, "%d/%m/%Y %H:%M:%S")  
  
  #y axes  
  index = which(names(relevantdata)=="Sub_metering_1")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))
  par(mfrow=c(1,1))
  plot(relevantdata$timestamp, relevantdata[,index], type="l", xlab="", ylab = "Energy sub metering", col="black")
  
  index = which(names(relevantdata)=="Sub_metering_2")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))  
  lines(relevantdata$timestamp, relevantdata[,index], col="red")
  
  index = which(names(relevantdata)=="Sub_metering_3")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))  
  lines(relevantdata$timestamp, relevantdata[,index], col="blue")
  
  legend('topright', c('Sub metering 1', 'Sub metering 2', 'Sub metering 3'), 
         lty=1, col=c('black', 'red', 'blue'))
  dev.off()
}
