########TESTING YOUR CODE#########
#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}
  
#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

########UNDERSTANDING THE NATURE OF YOUR QA/QC########
####Question 1
####Question 2
##read in the data file
#skip the first 3 rows 
#windows
datW <- read.csv("Z:/mcammarata/GitHub/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#FOR MAC -> SET WORKING DIRECTORY AND ASSIGN CSV AS DATW
datW <- read.csv("bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)

#preview data
print(datW[1,])
#-----get sensor info from file for windows------
sensorInfo <-   read.csv("Z:/mcammarata/GitHub/data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
#-----get sensor info from file for mac------
sensorInfo <-   read.csv("bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

####Question 3 

#########DATA QA/QC##########
###Using Packages
#---use install.packages to install lubridate
install.packages(c("lubridate"))


library(lubridate)

###Working with dates
#---convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

###Checking Missing Data
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

########SETTING UP TESTS FOR QA/QC########
###Visual checks 
#make a plot with filled in points (using pch)
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

# QA/QC ifelse function
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

###Realistic Values
#extreme range of the data and throughout the percentiles
quantile(datW$air.tempQ1)

#####QUESTION 4#####
#days with low air temperature
datW[datW$air.tempQ1 < 8,]  
#days with really high air temperature
datW[datW$air.tempQ1 > 33,] 

###Measurements Outside of Sensor Capabilities
#precipitation and lightning strikes plot
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#mark plot
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot only when there is precipitation 
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

######QUESTION 5#####
#test lightscale is numeric
assert(is.numeric(lightscale), "error: lightscale is not numeric")
#test lightscale reflects subset of datW
assert(length(lightscale) == nrow(datW), "error")

#####QUESTION 6#####
#filter out variables
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
#filter wind speed values
datW$wind.speedQ2 <- ifelse(datW$wind.speed  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$wind.speed > 5, NA, datW$air.tempQ1))
#verify filter 
assert(length(datW$wind.speedQ2) == nrow(datW), "error")

#new wind speed plot 
plot(datW$DD , datW$wind.speedQ2, 
     xlab = "Day of Year", ylab = "Wind Speed and Lightning",
     type="n",
     ylim = c(0, max(datW$wind.speedQ2, na.rm=TRUE)))

#plot only when there is wind speed 
points(datW$DD[datW$wind.speedQ2 > 0], datW$wind.speedQ2[datW$wind.speedQ2 > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)
#add lines
lines(datW$DD[datW$wind.speedQ2 >0],
      datW$wind.speedQ2[datW$wind.speedQ2 >0],
      col= rgb(95/255,158/255,160/255,.5))
###########Finishing Your QA/QC###########

#####QUESTION 7######
#soil moisture plot
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & Soil Moisture",
     type="n",
     ylim = c(0, 6))
#plot only when there is precipitation 
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot only when there is soil moisture     
points(datW$DD[datW$soil.moisture > 0], datW$soil.moisture[datW$soil.moisture > 0],
       col= "tomato3", pch=19)

#soil temperature plot
plot(datW$DD , datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temp and Soil Temp",
     type="n")
#plot only when there is air temp
points(datW$DD[datW$air.tempQ2 > 0], datW$air.tempQ2[datW$air.tempQ2 > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot only when there is soil temp    
points(datW$DD[datW$soil.temp > 0], datW$soil.temp[datW$soil.temp > 0],
       col= "tomato3", pch=19)
#####QUESTION 8######
#find averages
avg_airT <- mean(datW$air.temperature, na.rm=TRUE)
avg_wind <- mean(datW$wind.speed, na.rm=TRUE)
avg_soilM <- mean(datW$soil.moisture, na.rm = TRUE)
avg_soilT <- mean(datW$soil.temp, na.rm=TRUE)
total_precip <- sum(datW$precipitation, na.rm = TRUE)
#find number of observations 
airT <- sum(!is.na(datW$air.temperature))
wind <- sum(!is.na(datW$wind.speed))
soilM <- sum(!is.na(datW$soil.moisture))
soilT <- sum(!is.na(datW$soil.temp))
precip <- sum(!is.na(datW$precipitation))

table <- data.frame(Value = (c(avg_airT, avg_wind, avg_soilM, 
                               avg_soilT, total_precip)),
                    Observations = (c(airT, wind, soilM, soilT, precip)),
                    Variable = (c('Air Temp', 'Wind Speed', 'Soil Moisture', 'Soil Temp', 
                    'Total Precip')))

table
#####QUESTION 9######
par(mfrow=c(2,2))

#air temp
plot(datW$DD, datW$air.temperature, type ="b", pch=19,
     xlab="Day of Year", ylab="Air Temperature",
     main="Air Temperature")
#soil moisture
plot(datW$DD, datW$soil.moisture, type ="b", pch=19,
     xlab="Day of Year", ylab="Soil Moisture",
     main="Soil Moisture")
#Soil temperature
plot(datW$DD, datW$soil.temp, type ="b", pch=19,
     xlab="Day of Year", ylab="Soil Temperature",
     main="Soil Temperature")
#Precipitation
plot(datW$DD, datW$precipitation, type ="b", pch=19,
     xlab="Day of Year", ylab="Precipitation",
     main="Precipitation")