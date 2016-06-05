library('gdata')
library('moments')
library('Amelia')
library('extRemes')
library('ggplot2')
library('randomForest')
library('miscTools')
library('car')
library('stats')
library('corpcor')
library('corrplot')
library('MASS')
library("usdm")

data <- read.csv("C:/Users/neeru gupta/Desktop/r_python/Test Data.csv")
View(data)
sapply(data, class)
str(data)

library('Hmisc')
describe(data)
summary(data$churn_flag)
CrossTable(data$churn_flag)

######Check if any missing value############
missmap(data, main = "Missing values vs observed")

View(cor(data[,c(11:120)]))
cor2pcor(cor(data))
M <- cor(data[,c(11:20)])
corrplot(M, method="circle")
corrplot(M, method="number")
# First Correlogram Example
library('corrgram')
corrgram(cor(data[,c(11:20)]), order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

# 2-Way Frequency Table 
# 2-Way Cross Tabulation
library('gmodels')
CrossTable(data$recency, data$churn_flag) 

#dividing the data into samples
sub <- sample(nrow(data), floor(nrow(data) * 1))
train <- data[sub, ]
test <- data[-sub, ]

train$churn_flag<-as.factor(train$churn_flag)
View(train)
sapply(train$consistency_mnth_cd,class)
train$consistency_mnth_cd<-as.factor(train$consistency_mnth_cd)
train$consistency_qtr_cd<- as.factor(train$consistency_qtr_cd)
train$expect_visit_flag<- as.factor(train$expect_visit_flag)
train$visits_per_chg_yoy<- as.factor(train$visits_per_chg_yoy)
train$salespervisit <- train$sales/train$visits

# create first linear regression model
lm_model1=glm(churn_flag ~.,family=binomial(link='logit'),data=train[,c(2,11:120)])
summary(lm_model1)

lm_model2=glm(churn_flag~recency+visits+visits_last_12months+colli_q1+freq+visit_months+consistency_qtr_cd,
              family=binomial(link='logit'),data=train)
summary(lm_model2)


library('gmodels')
CrossTable(train$churn_flag)
#32% churn in the data 


# extract categories from continuous variables
train$promo_sales_per <- as.numeric(train$promo_sales_per)
train$psp_cat <- cut(train$promo_sales_per,pretty(train$promo_sales_per,5))
hist(train$promo_sales_per)
boxplot(train$promo_sales_per~train$churn_flag)
CrossTable(train$psp_cat,train$churn_flag)
chisq.test(table(train$psp_cat,train$churn_flag))

train$margin_per <- as.numeric(train$margin_per)
train$mp_cat <- cut(train$margin_per,pretty(train$margin_per,5))
hist(train$margin_per)
boxplot(train$margin_per~train$churn_flag)
CrossTable(train$mp_cat,train$churn_flag)
chisq.test(table(train$mp_cat,train$churn_flag))

train$food_visits_per <- as.numeric(train$food_visits_per)
train$fvp_cat <- cut(train$food_visits_per,pretty(train$food_visits_per,5))
hist(train$food_visits_per)
boxplot(train$food_visits_per~train$churn_flag)
CrossTable(train$fvp_cat,train$churn_flag)
chisq.test(table(train$fvp_cat,train$churn_flag))

train$food_sales_per <- as.numeric(train$food_sales_per)
train$fsp_cat <- cut(train$food_sales_per,pretty(train$food_sales_per,5))
hist(train$food_sales_per)
#outliers here at low percentages i.e. very few are low percentages
hist(sqrt(train$food_sales_per))
boxplot(train$food_sales_per)
boxplot(train$food_sales_per~train$churn_flag)
CrossTable(train$fsp_cat,train$churn_flag)
chisq.test(table(train$fsp_cat,train$churn_flag))

#food_promo_per
train$food_promo_per <- as.numeric(train$food_promo_per)
train$fpp_cat <- cut(train$food_promo_per,pretty(train$food_promo_per,5))
hist(train$food_promo_per)
boxplot(train$food_promo_per)
boxplot(train$food_promo_per~train$churn_flag)
CrossTable(train$fpp_cat,train$churn_flag)
chisq.test(table(train$fpp_cat,train$churn_flag))
# need to check whether sqrt tranformation will work here or not

#non_food_promo_per
train$non_food_promo_per <- as.numeric(train$non_food_promo_per)
train$nfpp_cat <- cut(train$non_food_promo_per,pretty(train$non_food_promo_per,5))
hist(train$non_food_promo_per)
# log tranformatin helps into near normal
#outliers in case of no churn
hist(log(train$non_food_promo_per))
boxplot(train$non_food_promo_per)
boxplot(train$non_food_promo_per~train$churn_flag)
CrossTable(train$nfpp_cat,train$churn_flag)
chisq.test(table(train$nfpp_cat,train$churn_flag))


#food_colli_per
train$food_colli_per <- as.numeric(train$food_colli_per)
train$fcp_cat <- cut(train$food_colli_per,pretty(train$food_colli_per,5))
hist(train$food_colli_per)
boxplot(train$food_colli_per)
# Outliers below 50%
boxplot(train$food_colli_per~train$churn_flag)
CrossTable(train$fcp_cat,train$churn_flag)
chisq.test(table(train$fcp_cat,train$churn_flag))


#food_pieces_per
train$food_pieces_per <- as.numeric(train$food_pieces_per)
train$fpip_cat <- cut(train$food_pieces_per,pretty(train$food_pieces_per,5))
hist(train$food_pieces_per)
boxplot(train$food_pieces_per)
# Outliers below 50%
boxplot(train$food_pieces_per~train$churn_flag)
CrossTable(train$fpip_cat,train$churn_flag)
chisq.test(table(train$fpip_cat,train$churn_flag))

# Scatterplot of viits vs. sales for each combination of food visits per and food sales per
# in each facet, churn flag is represented by shape and color
qplot(visits, sales, data=train, shape=churn_flag, color=churn_flag,facets=fvp_cat ~ fsp_cat,
      xlab="visits", ylab="sales")
qplot(visits, sales, data=train, shape=churn_flag, color=churn_flag,xlab="visits", ylab="sales")

qplot(recency, sales/visits, data=train, shape=churn_flag, color=churn_flag,
      xlab="recency", ylab="sales per vist")


qplot(recency, salespervisit, data=train, shape=churn_flag, color=churn_flag,
      xlab="recency", ylab="sales per vist")

library('randomForest')
set.seed(17)
rf_train <- train[,c(2,11:128)]
train_mod.rf <- randomForest(train$churn_flag ~ ., 
                             data=rf_train,mtry=2,ntree=1000,keep.forest=FALSE,
                             importance=TRUE,do.trace=10)
print(train_mod.rf)
importance(train_mod.rf)
importance(train_mod.rf, type=1)

qplot(recency, visits, data=train, shape=churn_flag, color=churn_flag,
      xlab="recency", ylab="visits")
qplot(recency, visits_last_12months, data=train, shape=churn_flag, color=churn_flag,
      xlab="recency", ylab="visits_last_12months")

# Kernel density plots
qplot(recency, data=train, geom="density", fill=churn_flag, alpha=I(.6),
      main="Distribution of recency", xlab="recency",
      ylab="Density")

# Boxplots of recency by 
# observations (points) are overlayed
qplot(churn_flag, recency, data=train, geom=c("boxplot"),
      fill=churn_flag, main="recency by churn",
      xlab="", ylab="recency")

qplot(churn_flag, salespervisit, data=train, geom=c("boxplot"),
      fill=churn_flag, main="salespervisit by churn",
      xlab="", ylab="salespervisit")


ggplot(train, aes(train$recency, train$salespervisit)) + geom_point(aes(colour=factor(train$churn_flag)))

ggplot(train, aes(train$recency, train$visits)) + geom_boxplot(aes(colour=factor(train$churn_flag)))

ggplot(train, aes(train$recency, train$sales)) + geom_boxplot(aes(colour=factor(train$churn_flag)))

ggplot(train, aes(train$sales, train$recency)) + geom_boxplot(aes(colour=factor(train$churn_flag)))


ggplot(train, aes(recency, colour = factor(churn_flag))) +
  geom_density(alpha = 0.1) +xlim(55, 70)


library('stats')
# apply lda
r <- lda(formula = churn_flag ~ ., data = train, prior = c(1,1)/2)
ir.pca <- prcomp(train[,c(11:70)],center = TRUE,scale. = TRUE) 
sapply(train,class)



library('xgboost')
library('MatrixModels')
library('Matrix')
library('readr')
library('stringr')
library('caret')
library('car')

require('xgboost')
require('Matrix')
require('data.table')
if (!require('vcd')) install.packages('vcd')
library('vcd')
levels(train$churn_flag)
xgb_train <- train[,c(2,11:129)]
sparse_matrix <- sparse.model.matrix(churn_flag~.-1, data = xgb_train)
head(sparse_matrix)
output_vector = xgb_train[,'churn_flag'] == "1"
bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 15,
               eta = 0.5, nthread = 2, nround = 20,objective = "binary:logistic",
               gamma=10,lambda=1, alpha=1, verbose = 1)

importance_matrix <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
View(importance)
library("Ckmeans.1d.dp")
xgb.plot.importance(importance_matrix[1:50,])
ggplot(train, aes(xgb_train$visits_last_6months, train$recency)) + 
  geom_point(aes(colour=factor(xgb_train$churn_flag)))


bst_cv <- xgb.cv(data = sparse_matrix, label = output_vector, nfold = 5,
              nrounds = 20, objective = "binary:logistic",
              early.stop.round = 3, maximize = FALSE)
xgb.plot.deepness(model = bst)
xgb.plot.tree(feature_names = sparse_matrix@Dimnames[[2]], model = bst)


lm_model3=glm(churn_flag~recency+visits+visits_last_6months+p_sales_per_chg_yoy+freq_per_chg_yoy+
                freq+tenure..in.days.+p_visits_last6mnths+p_sales_per_chg_6_6,
              family=binomial(link='logit'),data=train)
summary(lm_model3)

library('e1071')

svm_model <- svm(churn_flag~recency+visits+visits_last_6months+p_sales_per_chg_yoy+freq_per_chg_yoy+
                   freq+tenure..in.days.+p_visits_last6mnths+p_sales_per_chg_6_6, data=train,
                 kernel="radial", cost=1, gamma=0.5)
summary(svm_model)

pred <- predict(svm_model,train)
mean(pred==train$churn_flag)


sub <- sample(nrow(train), floor(nrow(train) * .75))
final_train <- train[sub, ]
final_test <- train[-sub, ]
svm_model1 <- svm(churn_flag~recency+visits+visits_last_6months+p_sales_per_chg_yoy+freq_per_chg_yoy+
                   freq+tenure..in.days.+p_visits_last6mnths+p_sales_per_chg_6_6, data=final_train,
                 kernel="radial", cost=1, gamma=0.5)
summary(svm_model1)

pred1 <- predict(svm_model1,final_test)
mean(pred1==final_test$churn_flag)
table(pred1,final_test$churn_flag)

library('ROCR')
library('SDMTools')
svm_model1.confusion<-confusion.matrix(final_test$churn_flag,pred1)
svm_model1.accuracy<-prop.correct(svm_model1.confusion)


library('MASS')
r <- lda(churn_flag~recency+visits+visits_last_6months+p_sales_per_chg_yoy+freq_per_chg_yoy+
           freq+tenure..in.days.+p_visits_last6mnths+p_sales_per_chg_6_6, 
         data = final_train)
r$prior
r$counts
r$means
r$scaling
r$svd

lda_model_final <- lda(churn_flag~recency+visits+visits_last_6months+p_sales_per_chg_yoy
                       +freq_per_chg_yoy+freq+tenure..in.days.+p_visits_last6mnths+
                         p_sales_per_chg_6_6, final_train)
plda = predict(object = lda_model_final, # predictions
               newdata = final_test)
head(plda$class) # classification result
head(plda$posterior, 3)
head(plda$x, 3)

ggplot(final_test, aes(plda$x[1], colour = factor(churn_flag))) +
  geom_point(alpha = 0.1) +xlim(55, 70)

qplot(plda$x, data=final_test, geom="density", fill=churn_flag, alpha=I(.6),
      main="Distribution of recency", xlab="recency",
      ylab="Density")

lda_model_final2 <- lda(churn_flag~., final_train)
plda2 = predict(object = lda_model_final2, # predictions
               newdata = final_test)
head(plda2$class) # classification result
head(plda2$posterior, 3)
head(plda2$x, 3)

qplot(plda2$x, data=final_test, geom="density", fill=churn_flag, alpha=I(.6),
      main="Distribution of recency", xlab="recency",
      ylab="Density")
# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(final_test$churn_flag, plda2$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

library('klaR')
partimat(churn_flag~recency+visits,data=final_train,method="lda") 

qda_model_final2 <- qda(churn_flag~recency+visits+visits_last_6months+p_sales_per_chg_yoy+freq_per_chg_yoy+
                          freq+tenure..in.days.+p_visits_last6mnths+p_sales_per_chg_6_6, final_train)
pqda2 = predict(object = qda_model_final2, # predictions
                newdata = final_test)
head(pqda2$class) # classification result
head(pqda2$posterior, 3)
head(pqda2$x, 3)

length(pqda2)
#Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(final_test$churn_flag, pqda2$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

mean(pqda2$class==final_test$churn_flag)
