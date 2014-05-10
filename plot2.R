plot2 <- function() {
  png(filename = "plot2.png", width = 480, height = 480,
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

  #global_active_power  
  index = which(names(relevantdata)=="Global_active_power")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))

  plot(relevantdata$timestamp, relevantdata[,index], type="l", xlab="", ylab = "Global Active Power(kilowatts)")
  
  dev.off()
}