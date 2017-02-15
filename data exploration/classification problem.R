# Load packages
install.packages('ggplot2', dep = TRUE)
install.packages('ggthemes')
install.packages('scales')
install.packages('dplyr')
install.packages('mice')
install.packages('randomForest')
install.packages('caret')
library('lazyeval')
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('caret')

train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)
sapply(full, class)

##feature engineering
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#assign names
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'rare_title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')

table(full$Fsize)

ggplot(full, aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

full$FsizeD[full$Fsize == 1] <- 'single'
full$FsizeD[(full$Fsize >1) & (full$Fsize< 5)] <- 'small'
full$FsizeD[full$Fsize>=5] <- 'large' 

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# This variable appears to have a lot of missing values
full$Cabin[1:28]
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

sapply(full[1:891,],function(x) sum(is.na(x)))

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Show number of missing Age values
sum(is.na(full$Age))

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)

par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Show new number of missing Age values
sum(is.na(full$Age))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age


# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)


# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full)


# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

library('doParallel')
cl <- makeCluster(3)
registerDoParallel(cl)

# Set a random seed
set.seed(754)
train$Survived <- as.factor(train$Survived)
# Build the model (note: not all possible variables are used)

# Create model with default paramters
trcontrol <- trainControl(method="repeatedcv", number=10, repeats=3,search = 'grid')
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(1:15),.ntree=c(500,1000, 1500, 2000, 2500))
rf_gridsearch <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
                         Title + FsizeD + Child + Mother, data=train[,c(2,3,5,6,7,8,10,12,13,16,19,20)], method="rf",
                       metric=metric, tuneGrid=tunegrid, trControl=trcontrol,importance=TRUE)
print(rf_gridsearch)
rf_gridsearch$finalModel
plot(rf_gridsearch)

'''
mtry is indeed bound by the number of variables in your model, as it specifies the size of the variable subset that
is randomly picked for each random forest iteration. 

http://stats.stackexchange.com/questions/82162/kappa-statistic-in-plain-english
as a means for evaluating the prediction performance of classifiers. 
The Kappa statistic (or value) is a metric that compares an Observed Accuracy with an Expected Accuracy (random chance).
The kappa statistic is used not only to evaluate a single classifier, but also to evaluate classifiers amongst themselves
In essence, the kappa statistic is a measure of how closely the instances classified by the machine learning classifier matched the data labeled as ground truth, controlling for the accuracy of a random classifier as measured by the expected accuracy
Poor agreement = 0.20 or less
Fair agreement = 0.20 to 0.40
Moderate agreement = 0.40 to 0.60
Good agreement = 0.60 to 0.80
Very good agreement = 0.80 to 1.00

'''
varImpPlot(rf_gridsearch$finalModel,type=2)
