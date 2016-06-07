#This function takes the location of the household power data, reads in the data
#and reconstructs plot1 per the course project instructions. 

create_plot1 <- function(directory = getwd()){
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
    
    #initialize plot graphic
    png('plot1.png')
    
    #create histogram as shown in project instructions
    hist(as.numeric(power_subset$Global_active_power), col = 'red',
         xlab = 'Global Active Power (kilowatts)',
         main = 'Global Active Power')
    
    #close graphics device, saving the png
    dev.off()
    
    #return the working directory to the previous directory
    setwd(wd)
}