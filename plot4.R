#This function takes the location of the household power data, reads in the data
#and reconstructs plot4 per the course project instructions. 

create_plot4 <- function(directory = getwd()){
    library(lubridate)
    
    #set working directory to input location if not defaulted to the current
    wd = setwd(directory)
    
    #read in household power data
    power = read.table('household_power_consumption.txt', header = T, sep = ';'
                       , stringsAsFactors = F)
    
    #convert Date field to a date class, then subset power to include on;y
    #Feb. 1 and 2, 2007
    power$Date = dmy(power$Date)
    power_subset = power[power$Date >= ymd('2007-02-01') 
                         & power$Date <= ymd('2007-02-02'),]
    power_subset$datetime = ymd_hms(paste(power_subset$Date, power_subset$Time))
    
    #initialize plot graphic
    png('plot4.png')
    
    #reconstruct plot 4 from project instructions - split window into 4
    par(mfrow = c(2,2))
    
    #start with line chart in upper left
    with(power_subset, plot(datetime, as.numeric(Global_active_power),
                            type = 'l', xlab = '',
                            ylab = 'Global Active Power'))
    
    
    #construct upper right chart
    with(power_subset, plot(datetime, as.numeric(Voltage),
                            type = 'l',
                            ylab = 'Voltage'))
    
    #construct lower left chart
    with(power_subset, plot(datetime, as.numeric(Sub_metering_1), type = 'l',
                            ylab = 'Energy sub metering', xlab = ''))
    
    #add Sub_metering_2 and 3 to lower left chart
    points(power_subset$datetime, as.numeric(power_subset$Sub_metering_2), 
           type = 'l',col= 'red')
    points(power_subset$datetime, as.numeric(power_subset$Sub_metering_3), 
           type = 'l',col= 'blue')
    
    #add legend to lower left chart
    legend("topright", legend = names(power_subset)[grepl('^Sub_', names(power_subset))], 
           col = c('black', 'red', 'blue'), lty = c(1, 1, 1), bty = 'n')
    
    #construct lower right chart
    with(power_subset, plot(datetime, as.numeric(Global_reactive_power), type = 'l',
                            ylab = 'Global_reactive_power'))
    
    #close graphics device, saving the png
    dev.off()
    
    #return the working directory to the previous directory
    setwd(wd)
}