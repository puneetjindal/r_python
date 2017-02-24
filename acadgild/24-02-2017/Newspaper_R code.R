Read the data
library(readr)
NewspaperData <- read_csv("~/Desktop/r_python/acadgild/24-02-2017/NewspaperData.csv")
View(NewspaperData)
attach(NewspaperData)

# Package "Lattice" is used for dotplot graph
library("lattice") 
dotplot(sunday, main="Dot Plot of Sunday Circulations",col="dodgerblue4")
dotplot(daily, main="Dot Plot of Daily Circulations", col="dodgerblue4")


# BoxPlot
boxplot(sunday,col="dodgerblue4")
boxplot(daily,col="dodgerblue4")

#ScatterPlot & adding Line segments to the points and drawing the fitted line on the plot
plot(daily,sunday,main="Scatter Plot ", col="Dodgerblue4", col.main="Dodgerblue4", col.lab="Dodgerblue4",
     xlab="Daily Circulations", ylab="Sunday Circulations", pch=20)
segments(daily, sunday, daily, 100+daily*1.3)
abline(coef=c(100,1.3), col="red")


# Linear Regression Model
reg.model<-lm(sunday~daily,data=NewspaperData)
summary(reg.model)
anova(reg.model)

# Layout command is used To plot the graphs in 2*2 matrix form
layout(matrix(c(1,2,3,4),2,2))
par(mfrow=c(1,1))
#The below "Plot" command gives four graphs like residual V fitted , Normal QQ Plot,...
plot(reg.model) 

#Confidence Intervals for the slopes
confint(reg.model,level=0.95)

#Predicted values and the prediction Intervals for the Input observations
predict(reg.model,interval="predict")


#Confidence Interval for mean response for new observations,daily circulations =(500000)
predict(reg.model,newdata= data.frame(daily=c(500000)),interval="confidence",level=0.95)
predict(reg.model,newdata= data.frame(daily=c(500000)),interval="confidence",level=0.99)


#Predicted values and Prediction Interval for mean response for  new observations,daily circulations =(500000,2000000)
predict(reg.model,newdata= data.frame(daily=c(500000)),interval="predict",level=0.95)
predict(reg.model,newdata= data.frame(daily=c(500000)),interval="predict",level=0.99)

