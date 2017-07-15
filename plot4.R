plot4 <- function()
{
    library(dplyr)
    library(lubridate)
    alldata <- read.csv("household_power_consumption.txt", 
                        na.strings = "?", 
                        sep = ";",
                        stringsAsFactors = FALSE,
                        colClasses = c("character", "character", rep("numeric", 7)))
    # Convert the date so we can filter out only the dates we want
    
    shortdata <- alldata %>% 
        mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
        filter(Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02"))
    
    # convert date and time to a single value
    datetime <-  with(shortdata, ymd(Date) + hms(Time))
    
    # Open the .png file
    png("plot4.png", width = 480, height = 480)
    par(mfcol=c(2,2))
    
    # plot the global active power
    # use plot() to create the plotting area, properly labeled
    plot(datetime, shortdata$Global_active_power, ylab="Global Active Power", xlab = "", type = "n")
    
    # create the line graph
    lines(datetime, shortdata$Global_active_power)
    
    
    # use plot() to create the plotting area, properly labeled
    plot(datetime, shortdata$Sub_metering_1, ylab="Energy sub metering", xlab = "", type = "n")
    
    # create the line graph
    lines(datetime, shortdata$Sub_metering_1)
    lines(datetime, shortdata$Sub_metering_2, col = "red")
    lines(datetime, shortdata$Sub_metering_3, col = "blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           lty=c(1, 1, 1), col=c("black", "red", "blue"), bty = "n")
    
    # plot the voltage
    # use plot() to create the plotting area, properly labeled
    plot(datetime, shortdata$Voltage, ylab = "Voltage", type = "n")
    
    # create the line graph
    lines(datetime, shortdata$Voltage)
    
    # plot the voltage
    # use plot() to create the plotting area, properly labeled
    plot(datetime, shortdata$Global_reactive_power, ylab = "Global_reactive_power", type = "n")
    
    # create the line graph
    lines(datetime, shortdata$Global_reactive_power)
    
    
    dev.off()
}