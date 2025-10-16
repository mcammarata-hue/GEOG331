install.packages("tidyverse")
#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

versicolor <- subset(iris, Species == "versicolor")

#list of pairs
mylist <- list(
  c("versicolor$Sepal.Length ~ versicolor$Sepal.Width"),
  c("versicolor$Petal.Length ~ versicolor$Petal.Width"),
  c("versicolor$Sepal.Length ~ versicolor$Petal.Length")
)

#summary list
summarylist <- list()

#loop
for(i in 1:length(mylist)) {
  model <- lm(mylist[[i]])
  summarylist[[i]] <- summary(model)
}

summarylist
#####################################
##### Part 2: data in dplyr     #####
#####################################

height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
#install dplyr
install.packages("dplyr")
library(dplyr)
#join data
join_iris <- left_join(iris, height, by = "Species")

head(join_iris)
#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
install.packages("ggplot2")
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()
#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
         geom_point()+
         theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, 
                 color = Species, size = Petal.Length)) +
  geom_point()+
  labs(title = "Sepal Length vs. Width By Species",
       x = "Sepal Length",
       y = "Sepal Width",
       size = "Petal Length") +
  theme_classic()
#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		