##########Project Code###########
#Load in csv files
bryometa <- read.csv("BryoMetadata.csv", header = T)
dbryo <- read.csv("DeepBryo.csv", header = T)

#merge data
x <- merge(dbryo, bryometa, by.x = "image.id", by.y = "Specimen_ID")

#load packages
library(lubridate)
library(ggplot2)
library(dplyr)

#####Create Violin Plot for D.depressa + R.doma in FL and AL
#filter data for D.depressa
filterD <- x %>% 
  filter(Taxon == "D.depressa", category == "autozooid", State %in% c('AL','FL'))
#filter data
filterR <- x %>% 
  filter(Taxon == "R.doma", category == "autozooid", State %in% c('AL','FL')) 

#make violin plot for D.depressa
ggplot(data= filterD, aes(x = State, y = area, fill = State)) + 
  geom_violin() + 
  labs(title = "D.depressa Autozooid Area", 
       x = "State",
       y = "Area")
#make violin plot for R.doma
ggplot(data= filterR, aes(x = State, y = area, fill = State)) + 
  geom_violin() + 
  labs(title = "R.doma Autozooid Area", 
       x = "State",
       y = "Area")


##########Future Work##########
#median 
m <- function(x) mean(x, na.rm = TRUE)
#find average autozooid area per colony 
avg_a <- tapply(dbryo$area[which(dbryo$category=="autozooid")], 
                dbryo$image.id[which(dbryo$category=="autozooid")], m)

