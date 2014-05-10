plot1 <- function() {
  png(filename = "plot1.png", width = 480, height = 480,
      units = "px", bg = "white", res = NA)
  
  ## Read outcome data
  data <- read.table("pa1data.txt", header=TRUE, sep=";", colClasses = "character")
  
  # now subset
  relevantdata <- subset(data, data$Date == "1/2/2007" || data$Date == "2/2/2007")

  ##ga power
  index = which(names(relevantdata)=="Global_active_power")
  relevantdata[, index] <- suppressWarnings(as.numeric(relevantdata[, index]))
  
  hist(relevantdata[ ,index], col="red", main="Global Active Power", xlab = "Global Active Power(kilowatts)")
  
  dev.off()
}