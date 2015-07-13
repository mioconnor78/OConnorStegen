### O'Connor and Stegen
### December 14
### creating temperature file 
### starting fresh, but following code from previous versions


setwd("/Users/maryoconnor/Documents/temporary files/OConnorStegen")
library (plyr)


T.hourly <- read.csv('hourlytanktemps.csv')

head(T.hourly)
T.hourly[26:50,]

# create a unique identifier for each time T was sampled
T.hourly$time.no <- seq(1, length(T.hourly$Day), 1)
T.hourly1 <- T.hourly[c(36,1:35)]
T.hourly2 <- T.hourly1[,-(2:5)]
T.hourly2 <- T.hourly2[,-32]
T.hourly2 <- T.hourly2[-(823:1314), ]
T.hourly3 <- stack(T.hourly2)

length(T.hourly2[,1])
T.hourly3 <- T.hourly3[-(1:822),]
timeID <- T.hourly2$time.no
T.hourly3$timeID <- timeID
head(T.hourly3)
names(T.hourly3) <- c('temp', 'Tank', 'timeID')
T.hourly3 <- subset(T.hourly3, T.hourly3$Tank!='time.no', select=1:3, drop = TRUE)

dates <- T.hourly1[,(1:5)]
T.hourly4 <- merge(T.hourly3, dates, by.x = 'timeID', by.y = 'time.no', all = F)
T.hourly4$Day <- as.character(T.hourly4$Day)

# cumulative average temperature: 
temp.cum <- ddply(T.hourly4, .(Tank), summarize, mean(temp, na.rm = TRUE))
head(temp.cum)
plot(T.hourly3[T.hourly3$Tank == 'X1',]$temp ~ T.hourly3[T.hourly3$Tank == 'X1',]$timeID)
plot(temp.cum$Tank,temp.cum$..1)
levels(T.hourly3$Tank)

temp.daily <- ddply(T.hourly4, .(Tank, timeID), summarize, mean(temp, na.rm = TRUE))
dim(temp.daily)
