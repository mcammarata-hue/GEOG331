#################
#Linear Regression Example Using Iris Data
################
rm(list = ls())

#subset the virginica species
flower <- iris[iris$Species == "virginica",]

#scatter plot sepal length vs. petal length 
plot(flower$Sepal.Length, flower$Petal.Length, pch=19,
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Iris virginica")

#fit regression model 
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

#Plot residuals 
plot(flower$Sepal.Length, summary(fit)$residuals, pch=19,
     xlab = "Sepal Length", ylab = "Residuals")
abline(h = 0)

#check normality of residuals 
hist(summary(fit)$residuals, col = "purple4",
     main = "Residual Dsitribution", xlab = "Residuals")

#qqnorm or qqline can provide another visual check 
qqnorm(summary(fit)$residuals, pch=19)
qqline(summary(fit)$residuals, pch=19)

#user Shaprio wilks test to check normality
shapiro.test(summary(fit)$residuals)
