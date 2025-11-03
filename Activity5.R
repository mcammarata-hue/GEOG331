#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)  

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#######Question 2########
#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365)) 

######Question 3 and 4########
#plot discharge
plot(datD$decYear, datD$discharge, type="l", 
     xlab="Year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

######Basic Formatting#######
#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#######Question 5#######
#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="2017", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)    

dat2017 <- subset(datD, year == 2017)
daily_2017 <- aggregate(dat2017$discharge, by=list(dat2017$doy),
                        FUN = "mean",
                        na.rm = TRUE)
colnames(daily_2017) <- c("doy", "dailyave_2017")
lines(daily_2017$doy, daily_2017$dailyave_2017, col = "magenta", lwd=1.5)

# add when months start
month_begin <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec") 

axis(1, at = month_begin, labels = month_name)
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "magenta"),#colors
       pch=c(NA,15,15),#symbols
       bty="n")#no legend border

#####Question 7#####
#determine amount of data per day 
datperday <- aggregate(datP$hour,
                      by = list(datP$year, datP$doy),
                      FUN = length)
colnames(datperday) <- c("year", "doy", "observations")
#find what days have all 24 hour readings
datperday$full_24 <- datperday$observation == 24
#merge datasets
datD <- merge(datD, datperday, by = c("year", "doy"), all.x = TRUE)
#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#load ggplot
library(ggplot2)
#make plot
ggplot(na.omit(datD), aes(x = doy, y= discharge))+
  geom_line(color = "grey")+
  geom_point(data = subset(datD, full_24 == FALSE),
             aes(color = full_24),
             size = 1)+
  geom_point(data = subset(datD, full_24 == TRUE),
             aes(color = full_24),
             size =1)+
  scale_color_manual(values = c("FALSE" = "darkgrey", "TRUE" = "red"),
                     labels = c("<24 Hour Data", "Full 24 Hour Data"))+
  labs(
    x= "DOY",
    y = "Discharge",
    color = "Data", 
    title = "Full 24 Hour Data"
  )

############################################
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))

#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

######Question 8######
#determine rain event days 
unique(datP$doy)
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 340 & datD$doy < 342 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 340 & datP$doy < 342 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))

#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

###############################
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

######Question 9#####
library(dplyr)
#filter for 2016 by season
data2016 <- datD %>% filter(year == 2016)
data2016 <- data2016 %>% 
  mutate(month=month(datetime),
         season=case_when(
    month %in% c(6,7,8) ~ "Summer", 
  month %in% c(9,10,11) ~ "Fall", 
  month %in% c(12,1,2) ~ "Winter",
  month %in% c(3,4,5) ~ "Spring"
  )
)
#make violin plot for 2016
ggplot(data= data2016 %>% filter(!is.na(season)), aes(season,discharge)) + 
  geom_violin() +
  labs(title = "2016 Discharge by Season", 
       x = "Season",
       y = "Discharge")

#filter for 2017 by season
data2017 <- datD %>% filter(year == 2017)
data2017 <- data2017 %>%
  mutate(month=month(datetime),
         season=case_when(
           month %in% c(6,7,8) ~ "Summer", 
           month %in% c(9,10,11) ~ "Fall", 
           month %in% c(12,1,2) ~ "Winter",
           month %in% c(3,4,5) ~ "Spring"
         )
  )
#make violin plot for 2017
ggplot(data= data2017 %>% filter(!is.na(season)), aes(season,discharge)) + 
  geom_violin() +
  labs(title = "2017 Discharge by Season", 
       x = "Season",
       y = "Discharge")