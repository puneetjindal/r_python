############ data exploration in R   #################
## installing packages in R
install.packages("pacman")
library('pacman')

# install if required and then load the library
p_load('ggplot2','car','corpcor','caret','doParallel','Amelia','randomForest','reshape')

## package management in R
install.packages('packrat')
library('packrat')

data("iris")

str(iris)
## convert different variable types
is.numeric(iris$Species)
class(iris)
class(iris$Sepal.Length)

sapply(iris,class)
colnames(iris)

summary(iris)
#vector , matrix, dataframe, numeric, factor

## converting numeric to categorical
iris$pl_cat <- cut(as.numeric(iris$Petal.Length),pretty(iris$Petal.Length,5))
plot(iris$Species,iris$pl_cat)
boxplot(iris$Sepal.Length~iris$Species)

chisq.test(table(iris$pl_cat,iris$Species))

hist(as.numeric(sqrt(iris$Petal.Width)))

######Check if any missing value############
library('Amelia')
missmap(iris, main = "Missing values vs observed")

##reshape
library('reshape2')
require(reshape2)
x = data.frame(subject = c("John", "Mary"), 
               time = c(1,1),
               age = c(33,NA),
               weight = c(90, NA),
               height = c(2,2))
molten = melt(x, id = c("subject", "time"))
molten = melt(x, id = c("subject", "time"), na.rm = TRUE)
dcast(molten, formula = time + subject ~ variable)
## sorting data
data <- iris[order(iris$Petal.Length),]

###
####Technique 1 RandomForests
library('randomForest')
set.seed(17)
rf_model <- randomForest(Species~., data=iris[,c(1,2,3,4,5)],mtry=2,ntree=10000,keep.forest=FALSE,
                         importance=TRUE,do.trace=100)
print(rf_model)
importance(rf_model)
importance(rf_model, type=1)
varImpPlot(rf_model)



## plot charts
qplot(Petal.Width,Sepal.Length, data = iris, colour = Species)


########### correlation analysis ################
# Read the dataset
CigaretteConsumption <- read.csv('/Users/puneetjindal/Desktop/r_python/data exploration/CigaretteConsumption.csv')
#rm(cigarette_consumption)
attach(CigaretteConsumption)

#Scatter Plot matirix
pairs(CigaretteConsumption[,2:8],col="dodgerblue4",pch=20)

#Correlation Matrix
cor(CigaretteConsumption[,2:8])

### Partial Correlation matrix: Install and load the package called "corpcor"
install.packages('corpcor')
library(corpcor)
cor2pcor(cor(CigaretteConsumption[,2:8]))


# parallelization in R
library('doParallel')
system.time(foreach(i=1:100000) %do% sum(tanh(1:i)))

cl <- makeCluster(3)  
registerDoParallel(cl)  
system.time(foreach(i=1:100000) %dopar% sum(tanh(1:i)))  
stopCluster(cl)  
help(system.time)
##What does 'user' and 'system' measure?