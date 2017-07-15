plot2 <- function()
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
    png("plot2.png", width = 480, height = 480)
    
    # use plot() to create the plotting area, properly labeled
    plot(datetime, shortdata$Global_active_power, ylab="Global Active Power (kilowatts)", xlab = "", type = "n")
    
    # create the line graph
    lines(datetime, shortdata$Global_active_power)
    
    dev.off()
}