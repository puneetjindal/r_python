install.packages('pacman')
library('pacman')

p_load('mice','VIM','HotDeckImputation','hot.deck')

data("airquality")
data <- airquality
sapply(data,class)

data$Month <- as.factor(data$Month)
data$Day <- as.factor(data$Day)

#We remove few values in a separate dataset so that we can compare our imputation effectiveness later by comparing with the original 
data[4:10,3] <- rep(NA,7)  ## replicate values of NA 7 times
data[1:5,4] <- NA


#What are missing values?
"""
Missing values are a common occurrence, and you need to have a strategy for treating them. 
A missing value can signify a number of different things in your data. Perhaps the data
was not available or not applicable or the event did not happen. It could be that the 
person who entered the data did not know the right value, or missed filling in. 
Data mining methods vary in the way they treat missing values. Typically, they ignore 
the missing values, or exclude any records containing missing values, or replace missing
values with the mean, or infer missing values from existing values.
"""

#What is the motivation of handling missing values?
##imputation is the process of replacing missing data with substituted values
"""
Because missing data can create problems for analyzing data, imputation is seen as a way
to avoid pitfalls involved with listwise deletion of cases that have missing values. 
That is to say, when one or more values are missing for a case, most statistical packages
default to discarding any case that has a missing value, which may introduce bias or 
affect the representativeness of the results. Imputation preserves all cases by replacing
missing data with an estimated value based on other available information. Once all 
missing values have been imputed, the data set can then be analysed using standard 
techniques for complete data.[2] Imputation theory is constantly developing and thus 
requires consistent attention to new information regarding the subject. There have been 
many theories embraced by scientists to account for missing data but the majority of 
them introduce large amounts of bias. A few of the well known attempts to deal with 
missing data include: hot deck and cold deck imputation; listwise and pairwise deletion;
mean imputation; regression imputation; last observation carried forward; stochastic 
imputation; and multiple imputation.
"""


#How to check for missing values multiple patterns of missing values in a dataset?
"""
There are two types of missing data:

MCAR: missing completely at random. This is the desirable scenario in case of missing data.
MNAR: missing not at random. Missing not at random data is a more serious issue and in this case it might be wise to check the data gathering process further and try to understand why the information is missing. For instance, if most of the people in a survey did not answer a certain question, why did they do that? Was the question unclear?

"""

summary(data)
dim(data)

complete.cases(data)
View(data[complete.cases(data),])

library('mice')
md.pattern(data[,-c(5,6)])
##The output tells us that 104 samples are complete, 34 samples miss only the Ozone measurement, 4 samples miss only the Solar.R value and so on.
##A perhaps more helpful visual representation can be obtained using the VIM package as follows

library('VIM')
?aggr
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
## As we can see plot on the right hand side shows bottom to top the increasing number of missingness


marginplot(data[c(1,2)])
## red boxplot for a particular axis is the distribution for the scenarios where perpendicular axis values are missing
#If our assumption of MCAR data is correct, then we expect the red and blue box plots to be very similar.

## Little's test for MCAR
p_load('BaylorEdPsych')
LittleMCAR(x = data)
##Segregate various missing value patterns
#For each pattern, compare observed variable mean vector with MLE of it,weighted by covariances
##Rationale: If each pattern produces a different mean, MCAR is unlikely
##Small p-value would imply NOT MCAR


# Q What are multiple type of techniques to impute missing values?

#Deletion based methods
"""## complete case analysis or listwise deletion

This is the simplest way to handle missing values where only those cases with no missing
values are considered 

Pros - if the missing pattern is MCAR, then the estimates will not be biased
COns - if the missing attern is not MCAR, then the estimates will be biased

"""
lr_model1 <- lm(Wind~.,data = data)
summary(lr_model1)

complete_cases = data[complete.cases(data),]
lr_model2 <- lm(Wind~.,data = complete_cases)
summary(lr_model2)
## Note lm has this feature as na.omit and na.exclude both do casewise deletion with respect to both predictors and criterions.


#Comapring above 2 models, There seems to be no difference in the models since the data is following MCAR


### Available case method or pairwise deletion
#This method uses only the data which is available
cov(data[,-c(5,6)])
cov(complete_cases[,-c(5,6)])
cov(data[,-c(5,6)], use = "complete.obs")


## cons of this method when correlations are computed using different cases,
#the resulting patterns can be ones that are impossible to produce with complete data and eventually incorrect statistics

##Single Imputation Methods
###Mean/mode substitution
# computing the mean column wise and replacing the NA values in the column with mean/median based on the data distribution
##this might distort the distribution of the data by making it more peaked around mean and thereby reducing variance
## this will provide biased estimates inspite of the type of missingness

install.packages('HotDeckImputation')
library(HotDeckImputation)

imputed_data <- impute.mean(as.matrix(data))


###Regression Imputation
##missing values are replaced by a predicted score generated by a regression model based on the non-missing data
# lm() and predict() functions one by one taking one variable at a time

###Hot Deck Imputation
##This method sorts respondents and non-respondents into a number of imputation subsets according to a user-specied set of covariates
## Missing values are replaced with values from matching respondents that are similar wrt to covariates
## its based on the research here by https://www.researchgate.net/profile/Dieter_Joenssen
#imputed_data <- impute.NN_HD(as.matrix(data[,-c(5:6)]),distance="man")
out <- hot.deck(data[,-c(5,6)])
hd2amelia(out)


###k-Nearest Neighbor Imputation
# This method is quite effective and simple
##The advantage of the knn approach is that it assumes data are missing at random (MAR) meaning,
#missing data only depends on the observed data; which in turn means, the knn approach is able to 
#take advantage of multivariate relationships in the completed data
##The disadvantage of this approach is it does not include a component to model random variation;
#consequently uncertainty in the imputed value is underestimated
p_load('DMwR')
knnImputation(data,k=2)
## try yaimpute and VIM package as well as these also have knn functions built


##Model-Based Methods

###Maximum Likelihood
##Either simply this can be achieved by the following commands
#The maximum likelihood method (implemented in R by the package mvnmle) is concerned only with a
#complete variance/covariance matrix based on maximum likelihood values from the available data
p_load(mvnmle)
cov(data[,-c(5,6)])
output<- mlest(data[,-c(5,6)],iterlim = 150)
##output$muhat - MLE of the mean vector.
##output$sigmahat- MLE of the variance-covariance matrix.
#This techniques doesnt give you the values to be imputed but instead mean and 
#covariance matrix as the output so that you can use these metrics to build your models or estimate the missing values
## That is why For many analyses like Principal Component Analysis, Structural Equation Modeling, an estimate of the mean vector and covariance matrix are needed


## Maximum Likelihood by Expectation Maximization algorithm
#E-M Algorithm, which stands for Expectation-Maximization. It is an interative procedure
#in which it uses other variables to impute a value (Expectation), then checks whether
#that is the value most likely (Maximization). If not, it re-imputes a more likely value. This goes on until it reaches the most likely value.
##EM imputations are better than mean imputations because they preserve the relationship with other variables, which is vital if you go on to use something like Factor Analysis or Regression. They still underestimate standard error, however, so once again, this approach is only reasonable if the percentage of missing data are very small (under 5%) and if the standard error of individual items is not vital (as when combining individual items into an index).
##EM is often used as a starting point for Multiple Imputation
##EM is a maximum likelihood procedure that works with the relationship
##between the unknown parameters of the data model and the missing data
##If we knew the missing values, then estimating the model parameters would be straightforward
#Similarly, if we knew the parameters of the data model, then it would be possible to obtain unbiased predictions for the missing values
#This suggests an approach in which we first estimate the parameters, then
#estimate the missing values, then use the filled in data set to re-estimate the
#parameters, then use the re-estimated parameters to estimate missing values,and so on
#When the process finally converges on stable estimates we stop iterating

##EM Algorithm for multivariate data in detail
#Let us assume a multivariate normal model
#Suppose that we have a data set with five variables (X1 to X5), with missing data on one or more variables
#The algorithm first performs a straightforward regression imputation procedure
#where it imputes values of X1, for example, from the other four variables, using
#the parameter estimates of means, variances, and covariances or correlations from the available data
#After imputing data for every missing observation in the data set, EM
#calculates a new set of parameter estimates
#The estimated means are simply the means of the variables in the imputed data set
#EM corrects biased estimation by estimating variances and covariances that
#incorporate the residual variance from the regression
#Now that we have a new set of parameter estimates, we repeat the imputation
#process to produce another set of data
#From that new set we re-estimate our parameters, as above, and then impute yet another set of data
#This process continues in an iterative fashion until the estimates converge


###Multiple imputation
##mice methods - Multiple Imputation by Chained Equations. Multiple imputation is a technique for analyzing incomplete datasets where missing data are a concern
p_load('mice','lattice')
imp<-mice(data,m=5,max = 2, method = 'pmm')
mat<-complete(imp)
mat
#bwplot(imp)   what this plot says

##Multiple imputations use simulation models that take from a set of possible responses,
#and impute in succession to try to come up with a variance/confidence interval that one
#can use to better understand the differences between imputed datasets, depending on the
#numbers that the simulation chooses to use for the missing data.


