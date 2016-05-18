"
Employing statistical techniques, conduct a preliminary prognosis of Hypertension/hypotension,
based on the level of hemoglobin and genetic history of the individual
"

bpdata<- read.csv("~/Downloads/Data - Classification of Patients with Abnormal Blood Pressure (Complete).csv")
sapply(bpdata, class)

# Changing the data type of variables according to data dictionary
bpdata$Train_Test_Flag <- as.factor(bpdata$Train_Test_Flag)
bpdata$Patient_Number <- as.factor(bpdata$Patient_Number)
bpdata$Blood_Pressure_Abnormality <- as.factor(bpdata$Blood_Pressure_Abnormality)
bpdata$Sex <- as.factor(bpdata$Sex)
bpdata$Pregnancy <- as.factor(bpdata$Pregnancy)
bpdata$Smoking <- as.factor(bpdata$Smoking)
bpdata$Chronic_kidney_disease <- as.factor(bpdata$Chronic_kidney_disease)
bpdata$Adrenal_and_thyroid_disorders <- as.factor(bpdata$Adrenal_and_thyroid_disorders)
bpdata$Level_of_Stress <- as.factor(bpdata$Level_of_Stress)

#cross checking the class
sapply(bpdata, class)

#Correlation Analysis
###  Examining correlations among variables Spearman correlations
library('PerformanceAnalytics')
chart.Correlation(data.frame(bpdata$Level_of_Hemoglobin,bpdata$Genetic_Pedigree_Coefficient,bpdata$Age,
                             bpdata$BMI,bpdata$Physical_activity,bpdata$salt_content_in_the_diet, 
                             bpdata$alcohol_consumption_per_day),
                  method="spearman",histogram=TRUE,pch=16)
##############Needs more exploration bivariate analysis#######

######Check if any missing value############
library('Amelia')
missmap(bpdata, main = "Missing values vs observed")

################# Trying clustering on the data########
df_cl <- subset(bpdata, select = -c(Train_Test_Flag,Patient_Number) )
d <- dist(as.matrix(df_cl))   # find distance matrix 
fit1 <- hclust(d)                # apply hirarchical clustering 
plot(fit1)
groups <- cutree(fit1, k=5) # cut tree into 5 clusters
rect.hclust(fit1, k=5, border="blue")
# Hierarchical Agglomerative  - Ward Hierarchical Clustering 
d <- dist(df_cl, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward")
plot(fit2) # display dendogram
groups <- cutree(fit2, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit2, k=5, border="red")

# K-Means Clustering with 5 clusters
fit3 <- kmeans(df_cl, 5)
library("cluster")
clusplot(df_cl, fit3$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
# Centroid Plot against 1st 2 discriminant functions
library("fpc")
plotcluster(df_cl, fit3$cluster) 

# Summary of the data
summary(bpdata)
library('psych')
# Column Names analyse each one by one
colnames(train)

#=================== Training Data ====================================
train <- subset(bpdata[bpdata$Train_Test_Flag==1,], select = -c(Train_Test_Flag,Patient_Number) )
summary(train)
library('Hmisc')
library('moments')
library('car')
library('gmodels') # for cross table

#=================== Data Exploration ====================================
attach(train)
describe(train) 
str(train)

# Level_of_Hemoglobin
mean(Level_of_Hemoglobin)
sd(Level_of_Hemoglobin)
skewness(Level_of_Hemoglobin)
kurtosis(Level_of_Hemoglobin)
# extract categories from continuous variables
train$LH_cat <- cut(as.numeric(train$Level_of_Hemoglobin),pretty(train$Level_of_Hemoglobin,5))
hist(as.numeric(train$Level_of_Hemoglobin))
cdplot(train$Blood_Pressure_Abnormality ~ train$Level_of_Hemoglobin, data=train)
plot(train$LH_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$LH_cat)
boxplot(train$Level_of_Hemoglobin~train$Blood_Pressure_Abnormality)
CrossTable(train$LH_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$LH_cat,train$Blood_Pressure_Abnormality))

# Genetic_Pedigree_Coefficient
mean(Genetic_Pedigree_Coefficient)
sd(Genetic_Pedigree_Coefficient)
skewness(Genetic_Pedigree_Coefficient)
kurtosis(Genetic_Pedigree_Coefficient)

train$GPC_cat <- cut(as.numeric(train$Genetic_Pedigree_Coefficient),pretty(train$Genetic_Pedigree_Coefficient,5))
hist(as.numeric(train$Genetic_Pedigree_Coefficient))
cdplot(train$Blood_Pressure_Abnormality ~ train$Genetic_Pedigree_Coefficient, data=train)
plot(train$GPC_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$GPC_cat)
boxplot(train$Genetic_Pedigree_Coefficient~train$Blood_Pressure_Abnormality)
CrossTable(train$GPC_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$GPC_cat,train$Blood_Pressure_Abnormality))

# Age
mean(train$Age)
sd(train$Age)
skewness(train$Age)
kurtosis(train$Age)

train$age_cat <- cut(as.numeric(train$Age),pretty(train$Age,5))
hist(as.numeric(train$Age))
cdplot(train$Blood_Pressure_Abnormality ~ train$Age, data=train)
plot(train$age_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$age_cat)
boxplot(train$Age~train$Blood_Pressure_Abnormality)
CrossTable(train$age_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$age_cat,train$Blood_Pressure_Abnormality))

# BMI
mean(train$BMI)
sd(train$BMI)
skewness(train$BMI)
kurtosis(train$BMI)
train$bmi_cat <- cut(as.numeric(train$BMI),pretty(train$BMI,5))
hist(as.numeric(train$BMI))
cdplot(train$Blood_Pressure_Abnormality ~ train$BMI, data=train)
plot(train$bmi_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$bmi_cat)
boxplot(train$BMI~train$Blood_Pressure_Abnormality)
CrossTable(train$bmi_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$bmi_cat,train$Blood_Pressure_Abnormality))

# Sex
plot(train$Sex)
plot(train$Sex,train$Blood_Pressure_Abnormality)
# 2-Way Cross Tabulation
CrossTable(train$Blood_Pressure_Abnormality,train$Sex)
results <- chisq.test(table(train$Blood_Pressure_Abnormality,train$Sex))
results$expected
results$residuals
results

# Pregnancy
#needed Data Imputation
plot(train$Pregnancy)
plot(train$Pregnancy, train$Blood_Pressure_Abnormality)
# 2-Way Cross Tabulation
CrossTable(train$Blood_Pressure_Abnormality, train$Pregnancy)
results <- chisq.test(table(train$Blood_Pressure_Abnormality, train$Pregnancy))
results$expected
results$residuals
results

# Smoking
plot(train$Smoking)
plot(train$Smoking, train$Blood_Pressure_Abnormality)
# 2-Way Cross Tabulation
CrossTable(train$Blood_Pressure_Abnormality, train$Smoking)
results <- chisq.test(table(train$Blood_Pressure_Abnormality, train$Smoking))
results$expected
results$residuals
results

# Physical_activity
mean(train$Physical_activity)
sd(train$Physical_activity)
skewness(train$Physical_activity)
kurtosis(train$Physical_activity)

train$pa_cat <- cut(as.numeric(train$Physical_activity),pretty(train$Physical_activity,5))
hist(as.numeric(train$Physical_activity))
cdplot(train$Blood_Pressure_Abnormality ~ train$Physical_activity, data=train)
plot(train$pa_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$pa_cat)
boxplot(train$Physical_activity~train$Blood_Pressure_Abnormality)
CrossTable(train$pa_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$pa_cat,train$Blood_Pressure_Abnormality))


# salt_content_in_the_diet
mean(train$salt_content_in_the_diet)
sd(train$salt_content_in_the_diet)
skewness(train$salt_content_in_the_diet)
kurtosis(train$salt_content_in_the_diet)

train$sc_cat <- cut(as.numeric(train$salt_content_in_the_diet),pretty(train$salt_content_in_the_diet,5))
hist(as.numeric(train$salt_content_in_the_diet))
cdplot(train$Blood_Pressure_Abnormality ~ train$salt_content_in_the_diet, data=train)
plot(train$sc_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$sc_cat)
boxplot(train$salt_content_in_the_diet~train$Blood_Pressure_Abnormality)
CrossTable(train$sc_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$sc_cat,train$Blood_Pressure_Abnormality))

# alcohol_consumption_per_day
#######################Data Imputation Pending
mean(train$alcohol_consumption_per_day)
sd(train$alcohol_consumption_per_day)
skewness(train$alcohol_consumption_per_day)
kurtosis(train$alcohol_consumption_per_day)

train$ac_cat <- cut(as.numeric(train$alcohol_consumption_per_day),pretty(train$alcohol_consumption_per_day,5))
hist(as.numeric(train$alcohol_consumption_per_day))
cdplot(train$Blood_Pressure_Abnormality ~ train$alcohol_consumption_per_day, data=train)
plot(train$ac_cat,train$Blood_Pressure_Abnormality)
plot(train$Blood_Pressure_Abnormality, train$ac_cat)
boxplot(train$alcohol_consumption_per_day~train$Blood_Pressure_Abnormality)
CrossTable(train$ac_cat,train$Blood_Pressure_Abnormality)
chisq.test(table(train$ac_cat,train$Blood_Pressure_Abnormality))

# Level_of_Stress
plot(train$Level_of_Stress)
plot(train$Level_of_Stress,train$Blood_Pressure_Abnormality)
# 2-Way Cross Tabulation
CrossTable(train$Blood_Pressure_Abnormality,train$Level_of_Stress)
results <- chisq.test(table(train$Blood_Pressure_Abnormality,train$Level_of_Stress))
results$expected
results$residuals
results

# Chronic_kidney_disease
plot(train$Chronic_kidney_disease)
plot(train$Chronic_kidney_disease,train$Blood_Pressure_Abnormality)
# 2-Way Cross Tabulation
CrossTable(train$Blood_Pressure_Abnormality,train$Chronic_kidney_disease)
results <- chisq.test(table(train$Blood_Pressure_Abnormality,train$Chronic_kidney_disease))
results$expected
results$residuals
results

# Adrenal_and_thyroid_disorders
plot(train$Adrenal_and_thyroid_disorders)
plot(train$Adrenal_and_thyroid_disorders,train$Blood_Pressure_Abnormality)
# 2-Way Cross Tabulation
CrossTable(train$Blood_Pressure_Abnormality,train$Adrenal_and_thyroid_disorders)
results <- chisq.test(table(train$Blood_Pressure_Abnormality,train$Adrenal_and_thyroid_disorders))
results$expected
results$residuals
results
library("ineq")
ineq(train$Adrenal_and_thyroid_disorders,type = "Gini")

#====================== Data Selection ===================================#
####Technique 1 RandomForests
library('randomForest')
set.seed(17)
train_mod <- subset(train[,1:14], select = -c(alcohol_consumption_per_day,Pregnancy) )
train_mod.rf <- randomForest(train$Blood_Pressure_Abnormality ~ ., 
                         data=train_mod,mtry=2,ntree=10000,keep.forest=FALSE,
                         importance=TRUE,do.trace=100)
print(train_mod.rf)
importance(train_mod.rf)
importance(train_mod.rf, type=1)

####other techniques to try out
#http://stats.stackexchange.com/questions/490/variable-selection-procedure-for-binary-classification/606#606


#====================== Test Data ===================================
#ApplicantIncome	int	In
test <- subset(bpdata[bpdata$Train_Test_Flag==2,], select = -c(Train_Test_Flag,Patient_Number) )
test_mod <- subset(test[,1:14], select = -c(alcohol_consumption_per_day,Pregnancy) )
library('Amelia')
missmap(test_mod, main = "Missing values vs observed")

#learning from training
logitmodel1<-glm(train$Blood_Pressure_Abnormality~., data=train_mod,family=binomial("logit"))
summary(logitmodel1)
#learning from training


#predicting the test data
logitmodel1.probs<-predict(logitmodel1, test_mod, type = "response")
logitmodel1.labels<-test_mod$Blood_Pressure_Abnormality
logitmodel1.pred = rep(0, length(logitmodel1.probs))
logitmodel1.pred[logitmodel1.probs > 0.5] = 1

library('ROCR')
library('caret')
library("e1071")
library("SDMTools")

logitmodel1.confusion<-confusion.matrix(logitmodel1.labels,logitmodel1.pred)
logitmodel1.accuracy<-prop.correct(logitmodel1.confusion)

#roc analysis for test data
logitmodel1.prediction<-prediction(logitmodel1.probs,logitmodel1.labels)
logitmodel1.performance<-performance(logitmodel1.prediction,"tpr","fpr")
logitmodel1.auc<-performance(logitmodel1.prediction,"auc")@y.values[[1]]
plot(logitmodel1.performance, colorize = TRUE, text.adj = c(-0.2,1.7))

logitmodel2<-glm(train$Blood_Pressure_Abnormality~ train$Level_of_Hemoglobin+train$Genetic_Pedigree_Coefficient, data=train_mod,family=binomial("logit"))
summary(logitmodel2)


############ Manual binning categories after looking at woe of different independent variables
library('smbinning')

result=smbinning(df=train_mod,y="Blood_Pressure_Abnormality",x="Level_of_Hemoglobin",p=0.05) 

train_mod$WOE_hemo <- 0
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin<=10] <- -0.805
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>10 & train_mod$Level_of_Hemoglobin<=11] <- 0.674
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>11 & train_mod$Level_of_Hemoglobin<=12.3] <- 1.030
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>12.3 & train_mod$Level_of_Hemoglobin<=14.6] <- 0.674
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>14.6 & train_mod$Level_of_Hemoglobin<=17.6] <- -4.051
train_mod$WOE_hemo <- as.factor(train_mod$WOE_hemo)

train_mod$WOE_gene <- 0
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient<=0.2] <- -1.702147311
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>0.2 & train_mod$Genetic_Pedigree_Coefficient<=0.4] <- 0.959965795
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>0.4 & train_mod$Genetic_Pedigree_Coefficient<=0.6] <- 1.432570206
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>0.6 & train_mod$Genetic_Pedigree_Coefficient<=0.8] <- 0.761019353
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>0.8 & train_mod$Genetic_Pedigree_Coefficient<=1] <- -2.398174954
train_mod$WOE_gene <- as.factor(train_mod$WOE_gene)

train_mod$WOE_BMI <- 0
train_mod$WOE_BMI[train_mod$BMI<=18] <- 0.224665095
train_mod$WOE_BMI[train_mod$BMI>18 & train_mod$BMI<=26] <- 0.050281661
train_mod$WOE_BMI[train_mod$BMI>26 & train_mod$BMI<=34] <- -0.337350971
train_mod$WOE_BMI[train_mod$BMI>34 & train_mod$BMI<=43] <- 0.224665095
train_mod$WOE_BMI[train_mod$BMI>43 & train_mod$BMI<=50] <- -0.237839105
train_mod$WOE_BMI <- as.factor(train_mod$WOE_BMI)

train_mod$WOE_phy <- 0
train_mod$WOE_phy[train_mod$Physical_activity<=9510] <- -0.234
train_mod$WOE_phy[train_mod$Physical_activity>9510 & train_mod$Physical_activity<=20300] <- 0.375764441
train_mod$WOE_phy[train_mod$Physical_activity>20300 & train_mod$Physical_activity<=29900] <- -0.053735601
train_mod$WOE_phy[train_mod$Physical_activity>29900 & train_mod$Physical_activity<=40600] <- 0.125083325
train_mod$WOE_phy[train_mod$Physical_activity>40600 & train_mod$Physical_activity<=49800] <- -0.234
train_mod$WOE_phy <- as.factor(train_mod$WOE_phy)

test_mod$WOE_hemo <- 0
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin<=10] <- -0.805
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>10 & test_mod$Level_of_Hemoglobin<=11] <- 0.674
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>11 & test_mod$Level_of_Hemoglobin<=12.3] <- 1.030
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>12.3 & test_mod$Level_of_Hemoglobin<=14.6] <- 0.674
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>14.6 & test_mod$Level_of_Hemoglobin<=17.6] <- -4.051
test_mod$WOE_hemo <- as.factor(test_mod$WOE_hemo)

test_mod$WOE_gene <- 0
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient<=0.2] <- -1.702147311
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>0.2 & test_mod$Genetic_Pedigree_Coefficient<=0.4] <- 0.959965795
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>0.4 & test_mod$Genetic_Pedigree_Coefficient<=0.6] <- 1.432570206
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>0.6 & test_mod$Genetic_Pedigree_Coefficient<=0.8] <- 0.761019353
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>0.8 & test_mod$Genetic_Pedigree_Coefficient<=1] <- -2.398174954
test_mod$WOE_gene <- as.factor(test_mod$WOE_gene)

test_mod$WOE_BMI <- 0
test_mod$WOE_BMI[test_mod$BMI<=18] <- 0.224665095
test_mod$WOE_BMI[test_mod$BMI>18 & test_mod$BMI<=26] <- 0.050281661
test_mod$WOE_BMI[test_mod$BMI>26 & test_mod$BMI<=34] <- -0.337350971
test_mod$WOE_BMI[test_mod$BMI>34 & test_mod$BMI<=43] <- 0.224665095
test_mod$WOE_BMI[test_mod$BMI>43 & test_mod$BMI<=50] <- -0.237839105
test_mod$WOE_BMI <- as.factor(test_mod$WOE_BMI)

test_mod$WOE_phy <- 0
test_mod$WOE_phy[test_mod$Physical_activity<=9510] <- -0.234
test_mod$WOE_phy[test_mod$Physical_activity>9510 & test_mod$Physical_activity<=20300] <- 0.375764441
test_mod$WOE_phy[test_mod$Physical_activity>20300 & test_mod$Physical_activity<=29900] <- -0.053735601
test_mod$WOE_phy[test_mod$Physical_activity>29900 & test_mod$Physical_activity<=40600] <- 0.125083325
test_mod$WOE_phy[test_mod$Physical_activity>40600 & test_mod$Physical_activity<=49800] <- -0.234
test_mod$WOE_phy <- as.factor(test_mod$WOE_phy)

test_mod$Pregnancy <- as.factor(test_mod$Pregnancy)
test_mod$Sex <- as.factor(test_mod$Sex)
test_mod$Chronic_kidney_disease <- as.factor(test_mod$Chronic_kidney_disease)
test_mod$Smoking <- as.factor(test_mod$Smoking)
test_mod$Adrenal_and_thyroid_disorders <- as.factor(test_mod$Adrenal_and_thyroid_disorders)
test_mod$Blood_Pressure_Abnormality <- as.factor(test_mod$Blood_Pressure_Abnormality)
str(test_mod)

#learning from training
logitmodel3<-glm(train_mod$Blood_Pressure_Abnormality~., data=train_mod,family=binomial("logit"))
summary(logitmodel3)

#predicting the test data
logitmodel3.probs<-predict(logitmodel3, test_mod, type = "response")
logitmodel3.labels<-test_mod$Blood_Pressure_Abnormality
logitmodel3.pred = rep(0, length(logitmodel3.probs))
logitmodel3.pred[logitmodel3.probs > 0.5] = 1

logitmodel3.confusion<-confusion.matrix(logitmodel3.labels,logitmodel3.pred)
logitmodel3.accuracy<-prop.correct(logitmodel3.confusion)

#roc analysis for test data
logitmodel3.prediction<-prediction(logitmodel3.probs,logitmodel3.labels)
logitmodel3.performance<-performance(logitmodel3.prediction,"tpr","fpr")
logitmodel3.auc<-performance(logitmodel3.prediction,"auc")@y.values[[1]]
plot(logitmodel3.performance, colorize = TRUE, text.adj = c(-0.2,1.7))

########Model 4###########
logitmodel4<-glm(Blood_Pressure_Abnormality~ Age+Sex + 
                   WOE_hemo + WOE_gene + WOE_phy,
                 data=train_mod,family=binomial("logit"))
summary(logitmodel4)

#predicting the test data
logitmodel4.probs<-predict(logitmodel4, test_mod, type = "response")
logitmodel4.labels<-test_mod$Blood_Pressure_Abnormality
logitmodel4.pred = rep(0, length(logitmodel4.probs))
logitmodel4.pred[logitmodel4.probs > 0.5] = 1

logitmodel4.confusion<-confusion.matrix(logitmodel4.labels,logitmodel4.pred)
logitmodel4.accuracy<-prop.correct(logitmodel4.confusion)

#roc analysis for test data
logitmodel4.prediction<-prediction(logitmodel4.probs,logitmodel4.labels)
logitmodel4.performance<-performance(logitmodel4.prediction,"tpr","fpr")
logitmodel4.auc<-performance(logitmodel4.prediction,"auc")@y.values[[1]]
plot(logitmodel4.performance, colorize = TRUE, text.adj = c(-0.2,1.7))


################### More effort needed in feature enginnering. Weight of evidences needs to be captured as variables#####
########Contnuous variables need to be converted to groups###############
library('devtools')
install_github("riv","tomasgreif")
library('woe')
library('rpart')
install_github("woe","tomasgreif")
library('woe')
library('riv')
library('Causata')
options(digits=2)

train_mod$WOE_hemo<-NULL
train_mod$WOE_BMI<-NULL
train_mod$WOE_phy<-NULL
train_mod$WOE_gene<-NULL
colnames(train_mod)
sapply(train_mod, class)


iv.mult(train_mod,"Blood_Pressure_Abnormality",TRUE)
iv.plot.summary(iv.mult(train_mod,"Blood_Pressure_Abnormality",TRUE))
iv.str(train_mod,"Sex","Blood_Pressure_Abnormality",verbose = TRUE)
iv.plot.woe(iv.mult(train_mod,"Blood_Pressure_Abnormality",vars=c("Sex"),summary=FALSE))

iv.mult(train_mod,"Blood_Pressure_Abnormality",vars=c("Level_of_Hemoglobin","Genetic_Pedigree_Coefficient"))
iv.plot.woe(iv.mult(train_mod,"Blood_Pressure_Abnormality",vars=c("Level_of_Hemoglobin","Genetic_Pedigree_Coefficient"),summary=FALSE))

categories <- iv.num(train_mod,"Level_of_Hemoglobin","Blood_Pressure_Abnormality",rcontrol=rpart.control(cp=.006), verbose = TRUE)
iv.plot.woe(categories)
train_mod$WOE_hemo <- 0
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin<9.265] <- 0.0000
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=9.265 & train_mod$Level_of_Hemoglobin<9.745] <- -0.2452
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=9.745 & train_mod$Level_of_Hemoglobin<10.63] <- 0.5426
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=10.63 & train_mod$Level_of_Hemoglobin<12.04] <- 1.1776
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=12.04 & train_mod$Level_of_Hemoglobin<12.5] <- 0.0043
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=12.5 & train_mod$Level_of_Hemoglobin<14.02] <- 1.0159
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=14.02 & train_mod$Level_of_Hemoglobin<14.18] <- -1.8745
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=14.18 & train_mod$Level_of_Hemoglobin<14.59] <- 0.5639
train_mod$WOE_hemo[train_mod$Level_of_Hemoglobin>=14.59] <- -4.0467
train_mod$WOE_hemo <- as.factor(train_mod$WOE_hemo)

             
test_mod$WOE_hemo <- 0
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin<9.265] <- 0.0000
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=9.265 & test_mod$Level_of_Hemoglobin<9.745] <- -0.2452
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=9.745 & test_mod$Level_of_Hemoglobin<10.63] <- 0.5426
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=10.63 & test_mod$Level_of_Hemoglobin<12.04] <- 1.1776
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=12.04 & test_mod$Level_of_Hemoglobin<12.5] <- 0.0043
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=12.5 & test_mod$Level_of_Hemoglobin<14.02] <- 1.0159
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=14.02 & test_mod$Level_of_Hemoglobin<14.18] <- -1.8745
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=14.18 & test_mod$Level_of_Hemoglobin<14.59] <- 0.5639
test_mod$WOE_hemo[test_mod$Level_of_Hemoglobin>=14.59] <- -4.0467
test_mod$WOE_hemo <- as.factor(test_mod$WOE_hemo)


############# Lets check whether any improvement in the model#####
# Converted Level_of_Hemoglobin continuous variable with categorcial variable
train_mod <- subset(train_mod, select = -c(Level_of_Hemoglobin) )
test_mod <- subset(test_mod, select = -c(Level_of_Hemoglobin) )
logitmodel5<-glm(Blood_Pressure_Abnormality~.,data=train_mod,family=binomial("logit"))
summary(logitmodel5)

#predicting the test data
logitmodel5.probs<-predict(logitmodel5, test_mod, type = "response")
logitmodel5.labels<-test_mod$Blood_Pressure_Abnormality
logitmodel5.pred = rep(0, length(logitmodel5.probs))
logitmodel5.pred[logitmodel5.probs > 0.5] = 1

logitmodel5.confusion<-confusion.matrix(logitmodel5.labels,logitmodel5.pred)
logitmodel5.accuracy<-prop.correct(logitmodel5.confusion)

#roc analysis for test data
logitmodel5.prediction<-prediction(logitmodel5.probs,logitmodel5.labels)
logitmodel5.performance<-performance(logitmodel5.prediction,"tpr","fpr")
logitmodel5.auc<-performance(logitmodel5.prediction,"auc")@y.values[[1]]
plot(logitmodel5.performance, colorize = TRUE, text.adj = c(-0.2,1.7))


################### Working with Genetic factor#########
categories <- iv.num(train_mod,"Genetic_Pedigree_Coefficient","Blood_Pressure_Abnormality",rcontrol=rpart.control(cp=.005), verbose = TRUE)
iv.plot.woe(categories)
categories$class
#10 Levels: (;0.135) <0.135;0.185) <0.185;0.205) <0.205;0.415) <0.415;0.425) <0.425;0.435) <0.435;0.505) ... <0.825;)
#[1] -3.84 -0.41 -0.13  0.94  0.00 -0.78  2.34  1.27 -0.19 -3.26

train_mod$WOE_gene <- 0
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient<0.135] <- -3.84
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.135 & train_mod$Genetic_Pedigree_Coefficient<0.185] <- -0.41
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.185 & train_mod$Genetic_Pedigree_Coefficient<0.205] <- -0.13
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.205 & train_mod$Genetic_Pedigree_Coefficient<0.415] <- 0.94
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.415 & train_mod$Genetic_Pedigree_Coefficient<0.425] <- 0.00
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.425 & train_mod$Genetic_Pedigree_Coefficient<0.435] <- -0.78
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.435 & train_mod$Genetic_Pedigree_Coefficient<0.505] <- 2.34
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.505 & train_mod$Genetic_Pedigree_Coefficient<0.755] <- 1.27
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.755 & train_mod$Genetic_Pedigree_Coefficient<0.825] <- -0.19
train_mod$WOE_gene[train_mod$Genetic_Pedigree_Coefficient>=0.825] <- -3.26
train_mod$WOE_gene <- as.factor(train_mod$WOE_gene)

test_mod$WOE_gene <- 0
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient<0.135] <- -3.84
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.135 & test_mod$Genetic_Pedigree_Coefficient<0.185] <- -0.41
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.185 & test_mod$Genetic_Pedigree_Coefficient<0.205] <- -0.13
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.205 & test_mod$Genetic_Pedigree_Coefficient<0.415] <- 0.94
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.415 & test_mod$Genetic_Pedigree_Coefficient<0.425] <- 0.00
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.425 & test_mod$Genetic_Pedigree_Coefficient<0.435] <- -0.78
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.435 & test_mod$Genetic_Pedigree_Coefficient<0.505] <- 2.34
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.505 & test_mod$Genetic_Pedigree_Coefficient<0.755] <- 1.27
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.755 & test_mod$Genetic_Pedigree_Coefficient<0.825] <- -0.19
test_mod$WOE_gene[test_mod$Genetic_Pedigree_Coefficient>=0.825] <- -3.26
test_mod$WOE_gene <- as.factor(test_mod$WOE_gene)

############# Lets check whether any improvement in the model#####
# Converted Genetic_Pedigree_Coefficient continuous variable with categorcial variable
train_mod <- subset(train_mod, select = -c(Genetic_Pedigree_Coefficient) )
test_mod <- subset(test_mod, select = -c(Genetic_Pedigree_Coefficient) )
logitmodel6<-glm(Blood_Pressure_Abnormality~.,data=train_mod,family=binomial("logit"))
summary(logitmodel6)
# Null model
logitmodel6_null = glm(Blood_Pressure_Abnormality ~ 1,data=train_mod,family = binomial(link="logit"))

#predicting the train data
logitmodel7.probs<-predict(logitmodel6, train_mod, type = "response")
logitmodel7.labels<-train_mod$Blood_Pressure_Abnormality
logitmodel7.pred = rep(0, length(logitmodel7.probs))
logitmodel7.pred[logitmodel7.probs > 0.5] = 1

logitmodel7.confusion<-confusion.matrix(logitmodel7.labels,logitmodel7.pred)
logitmodel7.accuracy<-prop.correct(logitmodel7.confusion)

#roc analysis for test data
logitmodel7.prediction<-prediction(logitmodel7.probs,logitmodel7.labels)
logitmodel7.performance<-performance(logitmodel7.prediction,"tpr","fpr")
logitmodel7.auc<-performance(logitmodel7.prediction,"auc")@y.values[[1]]
plot(logitmodel7.performance, colorize = TRUE, text.adj = c(-0.2,1.7))
######## Seems to be overfit 98%

#predicting the test data
logitmodel6.probs<-predict(logitmodel6, test_mod, type = "response")
logitmodel6.labels<-test_mod$Blood_Pressure_Abnormality
logitmodel6.pred = rep(0, length(logitmodel6.probs))
logitmodel6.pred[logitmodel6.probs > 0.5] = 1

library('ROCR')
library('SDMTools')
logitmodel6.confusion<-confusion.matrix(logitmodel6.labels,logitmodel6.pred)
logitmodel6.accuracy<-prop.correct(logitmodel6.confusion)
accuracy(logitmodel6.labels,logitmodel6.pred, threshold = 0.5)

library('caret')
confusionMatrix(logitmodel6.labels,logitmodel6.pred)


#roc analysis for test data
logitmodel6.prediction<-prediction(logitmodel6.probs,logitmodel6.labels)
logitmodel6.performance<-performance(logitmodel6.prediction,"tpr","fpr")
logitmodel6.auc<-performance(logitmodel6.prediction,"auc")@y.values[[1]]
plot(logitmodel6.performance, colorize = TRUE, text.adj = c(-0.2,1.7))

logitmodel6.confusion
confint(logitmodel6)
## odds ratios only
exp(coef(logitmodel6))

with(logitmodel6, null.deviance - deviance)
with(logitmodel6, df.null - df.residual)
with(logitmodel6, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(logitmodel6)

library('aod')
wald.test(b = coef(logitmodel6), Sigma = vcov(logitmodel6),Terms = 12:14)


### Use anova to compare multiple models to the previous one.
anova(logitmodel6,logitmodel6_null,logitmodel5,test="Chisq")

library('lmtest')
lrtest(logitmodel6)

#Plot of standardized residuals
plot(fitted(logitmodel6),rstandard(logitmodel6))

# plot predicted to actual response
plot(test_mod$Blood_Pressure_Abnormality ,logitmodel6.pred,pch = 16,
     xlab="Predicted probability of 1 response",ylab="Actual response")

#Check for overdispersion
#Overdispersion is a situation where the residual deviance of the glm is large relative to the residual degrees of freedom
#http://rcompanion.org/rcompanion/e_07.html
summary(logitmodel6)$deviance / summary(logitmodel6)$df.residual


###########Apply Random Forests#############
rfmodel1 = randomForest(Blood_Pressure_Abnormality ~ ., 
                                         data=train_mod,ntree=5000, 
                                         importance=TRUE)
rfmodel1
summary(rfmodel1)
#predicting the test data
rfmodel1.probs<-predict(rfmodel1, test_mod, type = "response")
rfmodel1.labels<-test_mod$Blood_Pressure_Abnormality
rfmodel1.pred = rep(0, length(rfmodel1.probs))
rfmodel1.pred[rfmodel1.probs > 0.5] = 1

library('ROCR')
library('SDMTools')
rfmodel1.confusion<-confusion.matrix(rfmodel1.labels,rfmodel1.pred)
rfmodel1.accuracy<-prop.correct(rfmodel1.confusion)
accuracy(rfmodel1.labels,rfmodel1.pred, threshold = 0.5)

library('caret')
confusionMatrix(rfmodel1.labels,rfmodel1.pred)

###############Apply conditional trees#############
library('partykit')
ctree_model1 <- ctree(Blood_Pressure_Abnormality ~ ., data=train_mod)
plot(ctree_model1)
ctree_model2 <- ctree(Blood_Pressure_Abnormality ~ ., data=train)
plot(ctree_model2)


##############gbm ################
library('gbm')
set.seed(33)

modelGBM <- gbm(as.factor(Blood_Pressure_Abnormality)~.,
                     data=train[1:200,c(1:3)],
                     distribution='bernoulli',
                     n.trees=40,
                     interaction.depth=3,
                     shrinkage=0.05,
                     bag.fraction=0.8,
                     keep.data=FALSE,
                     cv.folds=2)

##################xgboost########################
library('xgboost')
library('readr')
library('stringr')
library('caret')
library('car')

colnames(train)
ohe_feats = c('Level_of_Stress', 'Sex', 'Chronic_kidney_disease')
dummies <- dummyVars(~ Sex +  Level_of_Stress + Chronic_kidney_disease, data = train)
dummies

df_all_ohe <- as.data.frame(predict(dummies, newdata = train))
df_all_combined <- cbind(train[,-c(which(colnames(train) %in% ohe_feats))],df_all_ohe)

xgb <- xgboost(data = data.matrix(train[,1:5]), 
               label = train$Blood_Pressure_Abnormality, 
               eta = 0.1,
               max_depth = 15, 
               nround=2, 
               subsample = 0.5,
               seed = 1,
               objective = "binary:logistic",
               nthread = 3
)
y_pred <- predict(xgb, data.matrix(test[,2:5]))
?xgboost
require('xgboost')
require('Matrix')
require('data.table')
sparse_matrix <- sparse.model.matrix(Blood_Pressure_Abnormality~.-1, data = train_mod)
head(sparse_matrix)








#############################
################### Working with BMI continued#########
categories <- iv.num(train_mod,"BMI","Blood_Pressure_Abnormality",rcontrol=rpart.control(cp=.0009), verbose = TRUE)
iv.plot.woe(categories)
categories$class



#############################


#str(train_mod)
train_mod_woe <- iv.replace.woe(train_mod,iv=iv.mult(train_mod,"Blood_Pressure_Abnormality"), verbose = TRUE)
?iv.replace.woe
# Replace WoE for all numeric variables- ultimate one-liner

#
ggplot(bpdata,aes(bpdata$Level_of_Hemoglobin,bpdata$Genetic_Pedigree_Coefficient)) geom_point(aes(colour=factor(bpdata$Blood_Pressure_Abnormality)))

#gene
#heamo
#dependent variables 