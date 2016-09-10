if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(mice,VIM,UsingR)

#mice means multivariate imputation by chained equations

# use air quality data
data <- airquality

## remove some data points to make the example more tough
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

#class(data)
sapply(data, class)
str(data)

data <- data[-c(5,6)]
summary(data)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data, 2, pMiss)
apply(data, 1, pMiss)

# gives better understanding on pattern of data
md.pattern(data)

# frequency plots on missing data patterns
aggr_plot <- aggr(data, col=c('blue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.3, gap=.1, 
                  ylab=c("Histogram of missing data","Pattern"))

# margin plot helps us to understand on whether data is missing completely at random or not
marginplot(data[c(1,2)])

##Imputation of missing data
tempData <- mice(data,
                 m=5,# number of imputed datasets
                 maxit=50, # max iterations
                 meth='pmm',#predictive mean matching as imputation method
                 seed=500)
summary(tempData)

#  other methods in mice
methods(mice)

#checking imputation values
tempData$imp$Ozone

## checking which imputation method was applied
tempData$method

#complete data with first imputation
completedData_1 <- complete(tempData,1)
completedData_2 <- complete(tempData,2)
completedData_3 <- complete(tempData,3)
completedData_4 <- complete(tempData,4)
completedData_5 <- complete(tempData,5)

##Inspecting the distribution of original and imputed data

xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
#scatterplot and plot Ozone against all the other variables

densityplot(tempData)
#The density of the imputed data for each imputed dataset is showed in magenta while the density of the observed data is showed in blue. Again, under our previous assumptions we expect the distributions to be similar.

stripplot(tempData, pch = 20, cex = 1.2)



modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))

modelFit1


## now running multiple iterations with different number of imputations
tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))
