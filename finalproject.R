require(ggplot2)
library(XML)
require(lubridate)
require(zoo)
require(dplyr)

# created by Daniil Kistanov

# read data in
xml = XML::xmlParse("export.xml")
# make it into a dataframe
data.frm = XML:::xmlAttrsToDataFrame(xml["//Record"])

#convert type into string
data.frm$type = as.character( data.frm$type)

# subset by heartrate data
heartdata = subset( data.frm, type == "HKQuantityTypeIdentifierHeartRate")

#fun fact, you first convert to srt and then into and int
heartdata$value = as.character(heartdata$value)
heartdata$value = as.numeric(heartdata$value)

#now convert date into posix format
heartdata$endDate = ymd_hms(heartdata$endDate)
# I get wrong timezone converstion, subtracted needed amount of seconds
heartdata$endDate = as.POSIXct(heartdata$endDate - 23950)

# add some rows to analyze later
heartdata$month = format(heartdata$endDate,"%m")
heartdata$year = format(heartdata$endDate,"%Y")
heartdata$date = format(heartdata$endDate,"%Y-%m-%d")
heartdata$dayofweek = wday(heartdata$endDate, label = TRUE, abbr = FALSE)
heartdata$hour = hour(heartdata$endDate)
heartdata$minute = minute(heartdata$endDate)
heartdata$time = (heartdata$hour)+(heartdata$minute/60)

# keep only what is needed
keeps = c("endDate", "value", "month", "year", "date", "dayofweek", "hour", "minute", "time")
heartdata = heartdata[keeps]

# clean up environment:)
rm(xml)
rm(data.frm)
rm(keeps)

# split data by day and do final cleanup
days = split(heartdata, heartdata$date)
rm(heartdata)

for(i in 2:1122) {
  

  
  day = days[[i]]
  
  # remove all the days that don't have enough values
  if(nrow(day) < 120) {
    print(nrow(day))
    next
  }
  
  
  day = subset(day, hour < 12)
  day = subset(day, value < 100)
  date = as.Date.character(day$endDate)[1]
  
  # create a running average to smooth out the graph
  running_avg = rollmean(day$value, k = 5, fill = NA)
  night_plot = ggplot(day, aes(x = time, y = running_avg))  + ylim(40, 100) + xlim(0, 12.5) + geom_line()

  night_plot = night_plot + ylab("heart rate")
  night_plot = night_plot + xlab(date)
  ggsave(plot = night_plot, filename = paste(date, ".png"))
  
}

night_plot = night_plot + geom_boxplot(aes(group = cut_width(time, 0.3))) 
