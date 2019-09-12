# Statistical Data Analysis Project [29.05.2018]
# Ömer Faruk KOÇ

#read dataset
fuelConsumptionData <- read.csv("C:/Users/omerf/Desktop/fuelConsumptionData.csv")
View(fuelConsumptionData)

#dataset optimization
# convert mpg to kmL
fuelConsumptionData$mpg = round(235/fuelConsumptionData$mpg,2)
colnames(fuelConsumptionData)[colnames(fuelConsumptionData)=="mpg"] <- "kmL"
View(fuelConsumptionData)


#transmission assign 1 to automatic and 0 to manual
fuelConsumptionData$transmission <- as.factor(fuelConsumptionData$transmission)
levels(fuelConsumptionData$transmission) <- c("Manual", "Automatic")
View(fuelConsumptionData)
fuelConsumptionData1 = fuelConsumptionData

#Analysis
#Probability Distributions - Normality Test
shapiro.test(fuelConsumptionData$kmL)
print("p value > 0.05 kmL it is normally distributed")
x <- fuelConsumptionData$kmL;h<-hist(x,breaks=10, col="blue", xlab="100 KM/L", main="Histogram of 100 KM/L")
xfit<-seq(min(x),max(x),length=40);yfit<-dnorm(xfit,mean=mean(x),sd=sd(x));yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, type="l", col="red", lwd=2)


#Confidence interval for mean
sdkmL = sd(fuelConsumptionData$kmL)
lengthkmL = length(fuelConsumptionData$kmL)
df = (lengthkmL-1) * sdkmL / sqrt(lengthkmL)
confidenceLevel = 0.90
error = qt(confidenceLevel, df )
rangelower = mean(fuelConsumptionData$kmL) - error
rangeupper = mean(fuelConsumptionData$kmL) + error
cat("Standart Deviation is",sdkmL)
cat("Population sample is",lengthkmL,">30")
cat("Degree's of freedom is",df)
cat("Confidence Level is", confidenceLevel)
cat(rangelower,"< Confidence Interval For Mean <",rangeupper); cat("\nPopulation Mean is ",mean(fuelConsumptionData$kmL))


#Determining Sample size for mean
n = (qnorm((1-confidenceLevel)/2) * sdkmL / error) ^ 2
cat(n, "sample values with",sdkmL^2,"standard deviations can be found")


# Hypotesis Testing
aggregate(kmL~transmission, data = fuelConsumptionData, mean)
automaticCars = subset(fuelConsumptionData1, transmission == "Automatic")
manualCars = subset(fuelConsumptionData1, transmission == "Manual")
par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
hist(automaticCars$kmL, main = "Distribution kmL - automatic transmission", xlab = "kmL"); abline(v = mean(automaticCars$kmL), col = "red")
hist(manualCars$kmL, main = "Distribution kmL - manual transmission", xlab = "kmL"); abline(v = mean(manualCars$kmL), col = "red")
par(mfrow = c(1, 1))
#two tail
t.test(manualCars$kmL, automaticCars$kmL); print("automaticCars and manualCars fuel Consumption is difference")
#one tail
t.test(manualCars$kmL, mu = mean(fuelConsumptionData$kmL)); print("the average of the kmL values of the manualCars differs from the average of the dataset")


#Indepency Analysis for categorical data - chiSquared
library(MASS)
library(arules)
fuelConsumptionData$hp <- discretize(fuelConsumptionData$hp,breaks = 3,labels = c("lowHP","midHP","highHP"))
tbl = table(fuelConsumptionData$transmission, fuelConsumptionData$hp) 
chisq.test(tbl);cat("As the p-value = 0.1283 is greater than the .05 significance level, we do not reject the null hypothesis that the transmission values is independent of the horsepower of the cars.")


#Analysis of variance ANOVA
anovaModel <- aov(kmL ~ hp+transmission+gear, data = fuelConsumptionData)
summary(anovaModel); par(mfrow = c(2, 2))
plot(anovaModel); par(mfrow = c(1, 1))


# Linear Regression
library(ggplot2)
ggplot(fuelConsumptionData, aes(x=factor(transmission),y=kmL,fill=factor(transmission)))+
  geom_boxplot(notch=F)+ 
  scale_x_discrete("Transmission")+
  scale_y_continuous("100 Km/L")+
  ggtitle("KML by Transmission Type")
with(fuelConsumptionData, plot(hp, kmL, type = "n", main = "kmL vs. hp - by transmission type")) # no data
with(automaticCars, points(hp, kmL, col = "red", pch = 20))
with(manualCars, points(hp, kmL, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend

automaticCars = subset(fuelConsumptionData1, transmission == "Automatic")
manualCars = subset(fuelConsumptionData1, transmission == "Manual")

automaticCarsModel = lm(kmL ~ hp, data = automaticCars)
manualCarsModel = lm(kmL ~ hp, data = manualCars)

abline(automaticCarsModel, col = "red", lwd = 2)
abline(manualCarsModel, col = "blue", lwd = 2)

