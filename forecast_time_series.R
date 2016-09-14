#Forecasting Times Series with R
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign,forecast,stats)
#airline <- read.csv("http://online.sfsu.edu/mbar/ECON312_files/AirlineData.csv")

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
autoroute=read.table("http://freakonometrics.free.fr/autoroute.csv",header=TRUE,sep=";")
View(autoroute)
X=autoroute$a100
T=1:length(X)
plot(T,X,type="l",xlim=c(0,120))
plot(AgTH7$CheckIn,AgTH7$Tot_ConfirmedRooms,type="l",xlim=c(0,60))
reg=lm(X~T)
abline(reg,col="red")
Y<- residuals(reg)
acf(Y,lag=36,lwd=3)
Z=diff(Y,12)
acf(Z,lag=36,lwd=3)
pacf(Z,lag=36,lwd=3)
model1=arima(Z,order=c(0,0,1))
model1


##Demand Predictions for Uber Pickup in Manhattan
#HW 1: Descriptive Analysis



##
kings <-scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries<- ts(kings)
plot.ts(kingstimeseries)

births <-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthstimeseries)

souvenir <-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
plot.ts(souvenirtimeseries)

logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
