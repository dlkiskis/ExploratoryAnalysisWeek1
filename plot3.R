plot3 <- function()
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
    png("plot3.png", width = 480, height = 480)
    
    # use plot() to create the plotting area, properly labeled
    plot(datetime, shortdata$Sub_metering_1, ylab="Energy sub metering", xlab = "", type = "n")
    
    # create the line graph
    lines(datetime, shortdata$Sub_metering_1)
    lines(datetime, shortdata$Sub_metering_2, col = "red")
    lines(datetime, shortdata$Sub_metering_3, col = "blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           lty=c(1, 1, 1), col=c("black", "red", "blue"))
    dev.off()
}