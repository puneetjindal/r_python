library('gdata')
library('moments')
library('Amelia')
library('extRemes')
library('dummies')
library('ggplot2')
library('randomForest')
library('miscTools')
library('car')
library('stats')
library('corpcor')
library('corrplot')
library('MASS')
library("usdm")


####### extracting data
data <- read.xls('~/Downloads/Data Scientist Initial Screening Test.xlsx', sheet = 3, verbose = TRUE)
View(data)
data$X<-NULL

summary(data)
attach(data)

######Check if any missing value############
missmap(data, main = "Missing values vs observed")

########working with categorical variables#########
sapply(data, class)

################# Univariate Analysis ##############
#Distribution of price variable
boxplot(price,horizontal = T,col = "blue",main = "Car Prices Distribution",xlab = "Car Prices")
# Transofrmation to log seems to 
boxplot(log(price),horizontal = T,col = "blue",main = "Car Prices Distribution",xlab = "Car Prices")
hist(price,col = "blue",main = "Car Prices Distribution",xlab = "Car Prices",ylab = "Probability", prob= T, breaks = 15)
hist(log(price),col = "blue",main = "Car Prices Distribution",xlab = "Car Prices",ylab = "Probability", prob= T)
#curve plotting
curve(dnorm(x,mean(log(price)),sd(log(price))),add = T, col = "red", lwd = 3)
qqPlot(log(price),main = "QQ Price")
log_price <- log(price)
mean(log_price)
sd(log_price)
data$log_price<-log(data$price)


#Distribution of Windowed Make model count ##########
boxplot(Windowed_make_model_count,horizontal = T,col = "blue",main = "Windowed_make_model_count Distribution",xlab = "Windowed_make_model_count")
hist(Windowed_make_model_count,col = "blue",main = "Windowed_make_model_count Distribution",xlab = "Windowed_make_model_count",ylab = "Probability", prob= T)
hist(sqrt(Windowed_make_model_count),col = "blue",main = "Windowed_make_model_count Distribution",xlab = "Windowed_make_model_count",ylab = "Probability", prob= T)


#Distribution of count ##########
boxplot(count,horizontal = T,col = "blue",main = "count Distribution",xlab = "count")
hist(count,col = "blue",main = "count Distribution",xlab = "count",ylab = "Probability", prob= T)


#Distribution of car_age ##########
boxplot(car_age,horizontal = T,col = "blue",main = "car_age Distribution",xlab = "car_age")
hist(car_age,col = "blue",main = "car_age Distribution",xlab = "car_age",ylab = "Probability", prob= T)
mean(data$car_age<0)
#99.5 %
data$car_age[data$car_age<0] <- 0
#check again the percentage of negative values
mean(data$car_age<0)
hist(sqrt(data$car_age),col = "blue",main = "car_age Distribution",xlab = "car_age",ylab = "Probability", prob= T)
curve(dnorm(x,mean(sqrt(data$car_age)),sd(sqrt(data$car_age))),add = T, col = "red", lwd = 3)
qqPlot(sqrt(data$car_age),main = "QQ Age")
data$sqrt_car_age<-sqrt(data$car_age)


#Distribution of kms_bucket ##########
boxplot(kms_bucket,horizontal = T,col = "blue",main = "kms_bucket Distribution",xlab = "kms_bucket")
hist(kms_bucket,col = "blue",main = "kms_bucket Distribution",xlab = "kms_bucket",ylab = "Probability", prob= T)
hist(sqrt(data$kms_bucket),col = "blue",main = "car_age Distribution",xlab = "car_age",ylab = "Probability", prob= T)
curve(dnorm(x,mean(sqrt(data$kms_bucket)),sd(sqrt(data$kms_bucket))),add = T, col = "red", lwd = 3)
qqPlot(sqrt(data$kms_bucket),main = "QQ Age")
data$sqrt_kms_bucket<- sqrt(data$kms_bucket)

###########Bivariate Analysis##############
##### checking correlation between continuos variables

cor(data[,c(4,5,6,7,8)])
cor2pcor(cor(data[,c(4,5,6,7,8)]))
M <- cor(data[,c(4,5,6,7,8)])
corrplot(M, method="circle")
corrplot(M, method="number")
#for more https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html



#dividing the data into samples
sub <- sample(nrow(data), floor(nrow(data) * 1))
training <- data[sub, ]
testing <- data[-sub, ]


# create first linear regression model
lm_model1=lm(price~count+car_age+kms_bucket+Windowed_make_model_count,data=training)
summary(lm_model1)
"
Residual standard error: 46560 on 8471 degrees of freedom
Multiple R-squared:  0.1468,	Adjusted R-squared:  0.1463 
F-statistic: 364.2 on 4 and 8471 DF,  p-value: < 2.2e-16
"

lm_model2=lm(log_price~count+sqrt_car_age+kms_bucket+Windowed_make_model_count,data=training)
summary(lm_model2)
"
Residual standard error: 0.5664 on 8471 degrees of freedom
Multiple R-squared:  0.3507,	Adjusted R-squared:  0.3504 
F-statistic:  1144 on 4 and 8471 DF,  p-value: < 2.2e-16
"

#########creating dummy variable ########
training$price<-NULL
training$car_age<-NULL
training$kms_bucket<-NULL
testing$price<-NULL
testing$car_age<-NULL
testing$kms_bucket<-NULL

lm_model3=lm(log_price~.,data=training)
summary(lm_model3)
qqPlot(lm_model3, main="QQ Plot") #qq plot for studentized resid

# Assessing Outliers
exclude <- outlierTest(lm_model3,cutoff =0.05, n.max = 50, order = TRUE) # Bonferonni p-value for most extreme obs
#outliers where age and price seems to be not in sync after visual inspection of outliers
outliers<-data[c(1742,8234,9498,243,1953,589,247,20,6144,10002,6152,1851,2012,595,2027,236,2157,
            2271,602,2405,145,2333,2033,6150,2046,144,627,2385,259,2290,2817,9335,9431,264,
            9572,277,2550,265,2880,601,6425,6273,2284,1129,2255,2815,2005,2102,3063,6444),]
View(outliers)
training <- training[-c(1742,8234,9498,243,1953,589,247,20,6144,10002,6152,1851,2012,595,2027,236,2157,
                        2271,602,2405,145,2333,2033,6150,2046,144,627,2385,259,2290,2817,9335,9431,264,
                        9572,277,2550,265,2880,601,6425,6273,2284,1129,2255,2815,2005,2102,3063,6444),]

plot(data$price~data$car_age)
plot(data$log_price~data$sqrt_car_age)


##########Now building the model again after removing top 50 outliers
lm_model4=lm(log_price~.,data=training)
summary(lm_model4)
qqPlot(lm_model4, main="QQ Plot") #qq plot for studentized resid

leveragePlots(lm_model4) # leverage plots 

# Influential Observations
# added variable plots
avPlots(lm_model4)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(training)-length(lm_model4$coefficients)-2))
plot(lm_model4, which=4, cook.levels=0.02)
# Influence Plot
influencePlot(lm_model4, id.method="identify", main="Influence Plot",
              sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(lm_model4, main="QQ Plot")
# distribution of studentized residuals
sresid <- studres(lm_model4)
hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=50)
yfit<-dnorm(xfit)
lines(xfit, yfit) 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(lm_model4)
# plot studentized residuals vs. fitted values
spreadLevelPlot(lm_model4)


# Evaluate Collinearity
vif(training) # variance inflation factors
sqrt(vif(training)) # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(lm_model4)
# Ceres plots
ceresPlots(lm_model4)

# Test for Autocorrelated Errors
durbinWatsonTest(lm_model4)

# Global test of model assumptions
library('gvlma')
gvmodel <- gvlma(lm_model4)
summary(gvmodel) 

##########Now building the model again after removing top 50 outliers
training$Windowed_make_model_count<-NULL
lm_model5=lm(log_price~.,data=training)
summary(lm_model5)
qqPlot(lm_model5, main="QQ Plot") #qq plot for studentized residual

# Global test of model assumptions
library('gvlma')
gvmodel <- gvlma(lm_model5)
summary(gvmodel)

plot(lm_model5)

lm_model6 <- lm(log_price~.,data=training)
step <- stepAIC(lm_model6, direction="both")
step$anova # display results 
