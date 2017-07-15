plot1 <- function()
{
    library(dplyr)
    alldata <- read.csv("household_power_consumption.txt", 
                         na.strings = "?", 
                        sep = ";",
                        stringsAsFactors = FALSE,
                         colClasses = c("character", "character", rep("numeric", 7)))
    
    # Convert the date so we can filter out only the dates we want
    shortdata <- alldata %>% 
         mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
     filter(Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02"))
    
    # open a .png file and create the histogram
    png("plot1.png", width = 480, height = 480)
    hist(shortdata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
    dev.off()
}