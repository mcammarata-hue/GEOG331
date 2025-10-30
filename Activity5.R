########DELETE BEFORE SUBMITTING#########
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