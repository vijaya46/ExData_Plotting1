plot4 <- function() {
  png(filename = "plot4.png", width = 480, height = 480,
      units = "px", pointsize=12, bg = "white", res = NA)
  
  
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
  
  
  #2 row 2 cols for plots
  par(mfrow=c(2,2))
  #par(cex=0.7)
  
  #plot1
  index = which(names(relevantdata)=="Global_active_power")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))
  plot(relevantdata$timestamp, relevantdata[,index], type="l", xlab="", ylab = "Global Active Power", col="black")
  
  #plot 2
  index = which(names(relevantdata)=="Voltage")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))  
  plot(relevantdata$timestamp, relevantdata[,index], col="black", type="l", xlab="datetime", ylab = "Voltage")
  
  #plot 3
  index = which(names(relevantdata)=="Sub_metering_1")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))
  plot(relevantdata$timestamp, relevantdata[,index], type="l", xlab="", ylab = "Energy sub metering", col="black")
  
  index = which(names(relevantdata)=="Sub_metering_2")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))  
  lines(relevantdata$timestamp, relevantdata[,index], col="red")

  index = which(names(relevantdata)=="Sub_metering_3")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))  
  lines(relevantdata$timestamp, relevantdata[,index], col="blue")
  
  
  legend('topright', c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), 
         lty=1, col=c('black', 'red', 'blue'), pt.cex=1, bty='n', cex=0.9)
  
  
  #plot 4
  index = which(names(relevantdata)=="Global_reactive_power")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))
  plot(relevantdata$timestamp, relevantdata[,index], col="black", type="l", xlab="datetime", ylab = "Global_reactive_power")
  
  
  dev.off()
}
