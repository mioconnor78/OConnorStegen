### O'Connor and Stegen
### Data processing
### July 2015, drawing from previous files.

library(plyr)
library(reshape2)

data = read.csv("./OConnorStegendata.csv")
oxygen <- read.csv("./oxygen.csv")

head(data)


## make a datafile of NPP and ER data from oxygen data
## first need to get dawn1, dawn2 and dusk into columns

# first get times sorted out:
oxygen$date.2 <- paste(oxygen$date, oxygen$time)
oxygen$date.2 <- strptime(oxygen$date.2, format = "%y-%m-%d %k:%M")

oxygen.dawn1 <- oxygen[(oxygen$phase == 'dawn1'),-(4:6)]
oxygen.dawn2 <- oxygen[(oxygen$phase == 'dawn2'),-(4:6)]
oxygen.dusk <- oxygen[(oxygen$phase == 'dusk'),-(4:6)]
names(oxygen.dawn1) <- c('Tank', 'O2.d1', 'TempC.d1', 'weather.d1','air.temp.d1', 'week', 'sensor.d1', 'time.d1')
names(oxygen.dawn2) <- c('Tank', 'O2.d2', 'TempC.d2', 'weather.d2', 'air.temp.d2', 'week', 'sensor.d2', 'time.d2')
names(oxygen.dusk) <- c('Tank', 'O2.dsk', 'TempC.dsk', 'weather.dsk','air.temp.dsk', 'week', 'sensor.dsk', 'time.dsk')
oxygen1 <- merge(oxygen.dawn1, oxygen.dawn2, by.x = c('week', 'Tank'), by.y = c('week', 'Tank'))
oxygen2 <- merge(oxygen1, oxygen.dusk, by.x = c('week', 'Tank'), by.y = c('week', 'Tank'))
head(oxygen2)

#calculate NPP and ER (hourly)
oxygen2$NPP <- (oxygen2$O2.dsk - oxygen2$O2.d1)/as.numeric(oxygen2$time.dsk - oxygen2$time.d1)
oxygen2$ER <- -(oxygen2$O2.d2 - oxygen2$O2.dsk)/as.numeric(oxygen2$time.d2 - oxygen2$time.dsk)
oxygen2$GPP <- oxygen2$NPP + oxygen2$ER

head(oxygen2)
