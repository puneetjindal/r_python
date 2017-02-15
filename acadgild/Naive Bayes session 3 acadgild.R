#install.packages('ElemStatLearn')
#install.packages('klaR')
#install.packages('caret')

## install package manager library
install.packages('pacman')
library('pacman')

## install if required and load the mentioned packages
p_load('ElemStatLearn','klaR','caret','e1071')

## load dataset
data(spam)
sub = sample(nrow(spam), floor(nrow(spam) * 0.7)) 
train = spam[sub,]
test = spam[-sub,]
xTrain = train[,-58]
yTrain = train$spam
xTest = test[,-58]
yTest = test$spam
model1 = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=3,repeats = 5)) 
confusionMatrix(predict(model1$finalModel,xTest)$class,yTest)

model2 = train(xTrain,yTrain,'nb')
confusionMatrix(predict(model2$finalModel,xTest)$class,yTest)
