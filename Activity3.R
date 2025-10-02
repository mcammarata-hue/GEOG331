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
datW <- read.csv("Z:/mcammarata/GitHub/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])
#-----get sensor info from file------
sensorInfo <-   read.csv("Z:/mcammarata/GitHub/data/bewkes/bewkes_weather.csv",
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
#Determine Lightscale Reflects subset of datW
assert(length(lightscale) == nrow(datW), "error")
#test
assert(length(lightscale) == )

#####QUESTION 6#####
#filter out variables
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
#filter wind speed values
datW$wind.speedQ2 <- ifelse(datW$wind.speed  >= 1 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$wind.speed > 5, NA, datW$air.tempQ1))
#verify filter 
assert()

###########Finishing Your QA/QC###########

#####QUESTION 7######
#####QUESTION 8######
#####QUESTION 9######
