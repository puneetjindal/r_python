library('moments')
library('extremes')
library('car')

houseprices <- read.csv("~/Desktop/SA1 examples/houseprices.csv")
View(houseprices)
attach(houseprices)
#par(las = 1, mfrow = c(2,1))
boxplot(Price,horizontal = T,col = "blue",main = "Boxplot House Prices",xlab = "House Prices")
hist(Price,col = "blue",main = "Histogram House Prices",
     xlab = "House Prices",ylab = "Probability", prob= T)
#curve plotting
curve(dnorm(x,mean(Price),sd(Price)),add = T, col = "red", lwd = 3)
skewness(Price)
#0.8749042
summary(Price)

par(las = 1, mfrow = c(1,1))
qqPlot(Price, distribution = "norm")
qqline(Price,col = "red")
sd(Price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#16860  112000  151900  163900  205200  446400

quantile(Price, c(1-0.8524,0.9107,0.8413))
#14.76%   91.07%   84.13% 
#101206.2 261190.9 232934.0 
y = Living.Area
par(las = 1, mfrow = c(2,1))
boxplot(y,horizontal = T,col = "red",main = "Boxplot Living Area",
        xlab = "Area")
hist(y,col = "red",main = "Histogram Living Area",xlab = "Area",ylab = "Probability", prob= T)
curve(dnorm(x,mean(y),sd(y)),add = T, col = "green", lwd = 3)
skewness(y)
#0.8066481
log.Living.Area = log(Living.Area)
y.log = log.Living.Area
par(las = 1, mfrow = c(2,1))
boxplot(y.log,horizontal = T,col = "red",main = "Boxplot Log Living Area",
        xlab = "Log Area")
hist(y.log,col = "red",main = "Histogram Log Living Area",
     xlab = "Log Area",ylab = "Probability", prob= T)
curve(dnorm(x,mean(y.log),sd(y.log)),add = T, col = "green", lwd = 3)
skewness(y.log)
par(las = 1, mfrow = c(1,1))
qqPlot(log.Living.Area,main = "QQ Plot Log Living Area")
qqnorm(log.Living.Area)
qqline(y.log,col = "red")


#To generate a new variable as a linear combination of two normal variables plot a qqplot, use the following commands:
X1<-rnorm(500,15,3)
X2<-rnorm(500,25,5)
Y<-3*X1+4*X2 library(car) qqPlot(Y)

"
??? dnorm(data value, mu, sigma) : gives the density i.e. the function returns the
height of the normal distribution, at some value along the x-axis
??? pnorm(data value, mu, sigma) : gives the distribution function
??? qnorm(quantile, mu, sigma) : gives the Quantile function for calculating
critical values
??? rnorm(n,mu,sigma): generates ???n??? samples from a Normal distribution with mean=mu and standard deviation=sigma
??? qqnorm(variable) : Without ???extRemes?????? package will create a plot but without bands and with extRemes package will create a plot with bands.
??? qqPlot(variable,distribution=???norm???) :With ???car??? package creates a normal probability plot with bands
??? qqline(variable,col="red") with or without extRemes package will draw a red line passing through the qqplot
??? To generate a new variable as a linear combination of two normal variables plot a qqplot, use the following commands:
X1<-rnorm(500,15,3)
X2<-rnorm(500,25,5)
Y<-3*X1+4*X2 library(car) qqPlot(Y)

To draw a Histogram with a Normal curve superposed on it: hist(variable,prob=TRUE)
curve(dnorm(x,mean(variable),sd(variable)),col="red",add=TRUE)

CLT states that
Whatever the population distribution is, the sample mean will be normally distributed with ~ N(population mean,  population-std /sqrt(sample size))
Provided each data point is independent and sample size is large enough
"
