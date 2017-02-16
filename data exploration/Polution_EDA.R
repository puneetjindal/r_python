
RKPuram_un = read.csv(file="", sep="," )
View(RKPuram_un)

names(RKPuram_un)[names(RKPuram_un)=="BEN"] <- "Benzene"
names(RKPuram_un)[names(RKPuram_un)=="TOL"] <- "Toluene"
names(RKPuram_un)[names(RKPuram_un)=="PXY"] <- "P_Xylene"
names(RKPuram_un)[names(RKPuram_un)=="AT"] <- "TEMP"
# names(RKPuram_un)[names(RKPuram_un)=="temp"] <- "TEMP1"
names(RKPuram_un)[names(RKPuram_un)=="BP"] <- "Bar.Pressure"


# converting factors into numeric

RKPuram_un$NO = as.numeric(as.character(RKPuram_un$NO))
RKPuram_un$CO = as.numeric(as.character(RKPuram_un$CO))
RKPuram_un$NO2 = as.numeric(as.character(RKPuram_un$NO2))
RKPuram_un$O3 = as.numeric(as.character(RKPuram_un$O3))
RKPuram_un$SO2 = as.numeric(as.character(RKPuram_un$SO2))
RKPuram_un$Benzene = as.numeric(as.character(RKPuram_un$Benzene))
RKPuram_un$Toluene = as.numeric(as.character(RKPuram_un$Toluene))
RKPuram_un$Ethylben = as.numeric(as.character(RKPuram_un$Ethylben))
# RKPuram_un$M_P_Xylene = as.numeric(as.character(RKPuram_un$M_P_Xylene))
RKPuram_un$P_Xylene = as.numeric(as.character(RKPuram_un$P_Xylene))
RKPuram_un$O_Xylene = as.numeric(as.character(RKPuram_un$O_Xylene))
RKPuram_un$NH3 = as.numeric(as.character(RKPuram_un$NH3))
RKPuram_un$CH4 = as.numeric(as.character(RKPuram_un$CH4))
RKPuram_un$NMHC = as.numeric(as.character(RKPuram_un$NMHC))
RKPuram_un$THC = as.numeric(as.character(RKPuram_un$THC))
RKPuram_un$RH = as.numeric(as.character(RKPuram_un$RH))
RKPuram_un$TEMP = as.numeric(as.character(RKPuram_un$TEMP))
RKPuram_un$WS = as.numeric(as.character(RKPuram_un$WS))
RKPuram_un$VWS = as.numeric(as.character(RKPuram_un$VWS))
RKPuram_un$WD = as.numeric(as.character(RKPuram_un$WD))
RKPuram_un$SR = as.numeric(as.character(RKPuram_un$SR))
RKPuram_un$Bar.Pressure = as.numeric(as.character(RKPuram_un$Bar.Pressure))
RKPuram_un$NOx = as.numeric(as.character(RKPuram_un$NOx))
RKPuram_un$SPM = as.numeric(as.character(RKPuram_un$SPM))
RKPuram_un$PM2.5 = as.numeric(as.character(RKPuram_un$PM2.5))
RKPuram_un$PM10 = as.numeric(as.character(RKPuram_un$PM10))
RKPuram_un$PD_PM2.5 = as.numeric(as.character(RKPuram_un$PD_PM2.5))
RKPuram_un$PD_PM10 = as.numeric(as.character(RKPuram_un$PD_PM10))
RKPuram_un$PD_NO2 = as.numeric(as.character(RKPuram_un$PD_NO2))
RKPuram_un$PD_SO2 = as.numeric(as.character(RKPuram_un$PD_SO2))
RKPuram_un$PD_CO = as.numeric(as.character(RKPuram_un$PD_CO))



#Remove Columns without data
RKPuram = subset(RKPuram_un,select = c("WS","TEMP","WD","RH","SR","Bar.Pressure","VWS","NO2","SO2","PM2.5",
                                       "PM10","CO","PD_NO2","PD_SO2","PD_PM2.5","PD_PM10","PD_CO"))
View(RKPuram)
str(RKPuram)
# Handling NA for Generating Correlation matrix  

RKPuram = na.omit(RKPuram)
row.names(RKPuram)= 1:nrow(RKPuram)
RKPuram = RKPuram[-c(313),]
row.names(RKPuram)= 1:nrow(RKPuram)

str(RKPuram)

# Validating Normality of data  - Qnorm plot 
par(mfrow= c(2,4))
qqnorm((RKPuram$NO2), main =  "Normality check NO2 ")
qqnorm(log(RKPuram$CO), main =  "Normality check CO")
qqnorm(log(RKPuram$PM2.5), main =  "Normality check PM2.5")
qqnorm(log(RKPuram$PM10), main =   "Normality check PM10")
qqnorm(log(RKPuram$WS), main =   "Normality check WS")

par(mfrow= c(2,4))
qqnorm((RKPuram$NO2), main =  "NO2" )
qqnorm((RKPuram$SO2), main =  "SO2")
qqnorm((RKPuram$PM2.5), main =  "PM2.5")
qqnorm((RKPuram$PM10), main =   "PM10")
qqnorm((RKPuram$CO), main =  "CO" )
qqnorm((RKPuram$WS), main =   "WS")
qqnorm((RKPuram$TEMP), main =   "TEMP")
qqnorm((RKPuram$RH), main =   "RH")

title("
      Q-Q Plot -  Normality Check - RK Puram", outer=TRUE)


install.packages("moments")
library(moments)
skewness((RKPuram$NO2))
skewness(RKPuram$SO2)
skewness(RKPuram$CO)
skewness(RKPuram$PM2.5)
skewness(RKPuram$TEMP)
skewness(RKPuram$PM10)



# Normality analysis via Histogram
par(mfrow= c(2,4))
hist(RKPuram$NO2, main ="NO2",xlab="") 
hist(RKPuram$SO2, main = "SO2",xlab="")
hist(RKPuram$PM2.5, main="PM2.5",xlab="")
hist(RKPuram$PM10, main ="PM10",xlab="")
hist(RKPuram$CO, main = "CO",xlab="")
hist(RKPuram$WS, main = "WS",xlab="")
hist(RKPuram$TEMP, main = "TEMP",xlab="")
hist(RKPuram$RH, main = "RH",xlab="")
title("
      Histogram to check data distribution - RK Puram", outer=TRUE)

#Outlier checking using Boxplot ********************


# Calculating Percentile of outlier data
#Empirical Cumulative Distribution Plot

ecdf(RKPuram$NO2)(80)
ecdf(RKPuram$SO2)(80)
ecdf(RKPuram$CO)(2)
ecdf(RKPuram$PM2.5)(60)
ecdf(RKPuram$PM10)(100)

install.packages("ggplot")
install.packages("ggplot2")

RKPuram$City = "Delhi_RKPuram"
library("ggplot2")
ggplot(RKPuram,aes(x= RKPuram$City, y=RKPuram$CO)) + 
  geom_boxplot(outlier.colour = "blue") +  
  scale_y_continuous( breaks =c(seq(0 , ceiling(max(RKPuram$CO))+10 , ceiling(max(RKPuram$CO/20))))) +
  labs( x = "Delhi_RKPuram", y = "CO Readings", title = "RKPuram CO Analysis (Apr'15-May'15) 
        shows data are above Std 2 ???g/m???")                                      

ggplot(RKPuram,aes(x= RKPuram$City, y=RKPuram$NO2)) + 
  geom_boxplot(outlier.colour = "blue") +  
  scale_y_continuous( breaks =c(seq(0 , ceiling(max(RKPuram$NO2))+40 , 20))) +
  labs( x = "Delhi_RKPuram", y = "NO2 Readings", title = "RKPuram NO2 level Analysis (Apr'15 - May'15)
        shows data are above Std 80 ???g/m???")

ggplot(RKPuram,aes(x= RKPuram$City, y=RKPuram$SO2)) + 
  geom_boxplot(outlier.colour = "blue") +  
  scale_y_continuous( breaks =c(seq(0 , 300 ,10 ))) +
  labs( x = "Delhi_RKPuram", y = "SO2 Readings", title = "RKPuram SO2 level Analysis (Apr'15 - May'15)
        shows data are above Std 80 ???g/m???")

par(mfrow=c(2,4))
p1 = boxplot(RKPuram$NO2, main = "NO2", col = "orange")
p2 = boxplot(RKPuram$SO2, main = "SO2",  col = "orange")
p4 = boxplot(RKPuram$PM2.5, main = " PM2.5", col = "orange")
p5 = boxplot(RKPuram$PM10, main = "PM10", col = "orange")
p3 = boxplot(RKPuram$CO, main = "CO",  col = "orange")
p6 = boxplot(RKPuram$WS, main = "WS", col = "orange")
p6 = boxplot(RKPuram$TEMP, main = "TEMP",  col = "orange")
p6 = boxplot(RKPuram$RH, main = "RH",  col = "orange")
title("
      Boxplot Analysis - RK Puram", outer=TRUE)


ggplot(RKPuram,aes(x= RKPuram$City, y=RKPuram$PM2.5)) + 
  geom_boxplot(outlier.colour = "blue") +  
  scale_y_continuous( breaks =c(seq(0 , ceiling(max(RKPuram$PM2.5))+50 , 25))) +
  labs( x = "Delhi_RKPuram", y = "PM2.5 Readings", title = "RKPuram PM2.5 level Analysis (Apr'15 - May'15)
        Shows data are above Std 60 ???g/m???")

ggplot(RKPuram,aes(x= RKPuram$City, y=RKPuram$PM10)) + 
  geom_boxplot(outlier.colour = "blue") +  
  scale_y_continuous( breaks =c(seq(0 , ceiling(max(RKPuram$PM10))+50 ,50))) +
  labs( x = "Delhi_RKPuram", y = "PM2.5 Readings", title = "RKPuram PM10 level Analysis (Apr'15 - May'15)
        shows data are above Std 100 ???g/m???")


# Outlier Analysis - Varaiable

outlier_upper=function(x){
  q = quantile(x)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  QR = q3-q1
  return(q3+1.5*QR);
}

outlier_lower=function(x){
  q = quantile(x)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  QR = q3-q1
  return(q1-1.5*QR);
}

# outlier limits validation ------------------
NO2_upper = outlier_upper(RKPuram$NO2)
NO2_lower = outlier_lower(RKPuram$NO2)
SO2_upper = outlier_upper(RKPuram$SO2)
SO2_lower = outlier_lower(RKPuram$SO2)
CO_upper = outlier_upper(RKPuram$CO)
CO_lower = outlier_lower(RKPuram$CO)
PM2.5_upper = outlier_upper(RKPuram$PM2.5)
PM2.5_lower = outlier_lower(RKPuram$PM2.5)
NOx_upper = outlier_upper(RKPuram$NOx)

NOx_lower = outlier_lower(RKPuram$NOx)
Benzene_upper = outlier_upper(RKPuram$Benzene)
Benzene_lower = outlier_lower(RKPuram$Benzene)
NH3_upper = outlier_upper(RKPuram$NH3)
NH3_lower = outlier_lower(RKPuram$NH3)
NO_upper = outlier_upper(RKPuram$NO)
NO_lower = outlier_lower(RKPuram$NO)
O3_upper = outlier_upper(RKPuram$O3)
O3_lower = outlier_lower(RKPuram$O3)
#P_Xylene_upper = outlier_upper(RKPuram$P_Xylene)
#P_Xylene_lower = outlier_lower(RKPuram$P_Xylene)
Toluene_upper = outlier_upper(RKPuram$Toluene)
Toluene_lower = outlier_lower(RKPuram$Toluene)
PM10_upper = outlier_upper(RKPuram$PM10)
PM10_lower = outlier_lower(RKPuram$PM10)
WS_upper = outlier_upper(RKPuram$WS)
WS_lower = outlier_lower(RKPuram$WS)


# Outlier data
RKPuram[RKPuram$NO2>NO2_upper | RKPuram$NO2<NO2_lower , ]
RKPuram[RKPuram$SO2>SO2_upper | RKPuram$SO2<SO2_lower , ]
RKPuram[RKPuram$CO>CO_upper | RKPuram$CO<CO_lower , ]
RKPuram[RKPuram$PM2.5> PM2.5_upper | RKPuram$PM2.5< PM2.5_lower , ]
RKPuram[RKPuram$NOx>NOx_upper | RKPuram$NOx<NOx_lower , ]
RKPuram[RKPuram$PM10> PM10_upper | RKPuram$PM10< PM10_lower , ]
RKPuram[RKPuram$NH3> NH3_upper | RKPuram$NH3< NH3_lower , ]
RKPuram[RKPuram$NO> NO_upper | RKPuram$NO< NO_lower , ]
# RKPuram[RKPuram$P_Xylene> P_Xylene_upper | RKPuram$P_Xylene< P_Xylene_lower , ]
RKPuram[RKPuram$Benzene> Benzene_upper | RKPuram$Benzene< Benzene_lower , ]
RKPuram[RKPuram$Toluene> Toluene_upper | RKPuram$Toluene< Toluene_lower , ]
RKPuram[RKPuram$O3> O3_upper | RKPuram$O3< O3_lower , ]

RKPuram = subset( RKPuram, RKPuram$CO<=CO_upper & RKPuram$CO>=CO_lower)
RKPuram = subset( RKPuram, RKPuram$NO2<=NO2_upper & RKPuram$NO2>=NO2_lower)
RKPuram = subset( RKPuram, RKPuram$SO2<=SO2_upper & RKPuram$SO2>=SO2_lower)
RKPuram = subset( RKPuram, RKPuram$PM2.5<=PM2.5_upper & RKPuram$PM2.5>=PM2.5_lower)
RKPuram = subset( RKPuram, RKPuram$PM10<=PM10_upper & RKPuram$PM10>=PM10_lower)

row.names(RKPuram)= 1:nrow(RKPuram)
RKPuram = subset( RKPuram, RKPuram$SO2<300)
RKPuram = subset( RKPuram, RKPuram$CO<19)
RKPuram = subset( RKPuram, RKPuram$PM2.5<530)
RKPuram = subset( RKPuram, RKPuram$PM10<610)



tt1=RKPuram
str(tt1)


## Creating Development and Validation Sample
set.seed(1)
set.seed(200)
tt1$random <- runif(nrow(tt1), 0, 1);
tt1 <- tt1[order(tt1$random),]
tt1.dev <- tt1[which(tt1$random <= 0.75),]
tt1.val <- tt1[which(tt1$random > 0.75),]
c(nrow(tt1.dev), nrow(tt1.val))

str(tt1.dev)

View(tt1.dev)

ttr.dev = tt1[row.names(tt1.dev),]
ttr.val = tt1[row.names(tt1.val),]

head(ttr.dev)
head(tt1.dev)
str(tt1.val)

plot(tt1.dev$PM2.5)
plot(tt1.val$PM2.5)

---------
  
  #RKPuram
  str(ttr.dev)
full_model_PM2.5<-lm(PM2.5~ RH + WD + WS + VWS + SR +TEMP + Bar.Pressure ,data = ttr.dev)
full_model_PM2.5<-lm(PM2.5~ RH + WD + WS + VWS + SR +TEMP + Bar.Pressure ,data = ttr.dev)

summary(full_model_PM2.5)

str(ttr.dev)
full_model1_PM2.5<-lm(log(PM2.5)~  WS + TEMP +RH,data = ttr.dev)
full_model1_PM2.5<-lm(log(PM2.5)~  log(PD_PM2.5)+WS+RH ,data = ttr.dev)

install.packages("caret")
library(caret)
train_control <- trainControl(method="LOOCV")
model <- train(log(PM2.5)~ WS + TEMP +RH, data=tt1, trControl=train_control, method="lm")
model <- train(log(PM2.5)~ log(PD_PM2.5)+WS+RH, data=tt1, trControl=train_control, method="lm")
print(model)
rmse(exp(model$pred[1]),exp(model$pred[2]))
((sum(abs((exp(model$pred[1])-exp(model$pred[2]))/exp(model$pred[2])) ))/nrow(model$pred))*100

summary(full_model1_PM2.5)
sum(abs((model$pred[1]-model$pred[2])/model$pred[2])*100)/nrow(model$pred)
sum(abs((exp(model$pred[1])-exp(model$pred[2]))/exp(model$pred[2]))*100)/nrow(model$pred)


head(tt1)
nrow(model$pred)
rmse(exp(model$pred[1]),exp(model$pred[2]))
# PM2.5 = -2772.18 -51.74 WS -1.5292 RH -6.83 TEMP + 4.39 Bar Pressure

#Global Validation of Linear Models Assumptions
par(mfrow=c(2,2))
install.packages("gvlma")
library(gvlma)
gvlma::gvlma(full_model1_PM2.5)
plot(full_model1_PM2.5)

install.packages("car")
library(car)
vif(full_model1_PM2.5)
AIC(full_model1_PM2.5)

#Prediction with Test Data
predicted=predict(full_model1_PM2.5,newdata=ttr.val)

#RMSE calculation
install.packages("dplyr")
library(dplyr)
RMSE=(exp(predicted)-ttr.val$PM2.5)**2 %>% 
  mean() %>% 
  sqrt()
RMSE
mean(abs(((exp(predicted)-ttr.val$PM2.5)/ttr.val$PM2.5)))*100
mean(abs((model$pred[,1]-model$pred[,2])/model$pred[,2])*100)
par(mfrow = c(1,2))
plot(ttr.val$PM2.5, exp(predicted) ,xlab= " PM2.5-Actual", ylab = " PM2.5-predicted" , main = " RKPuram -MLR Model Fit
     W/o PD_PM2.5",col = "red")
abline(0, 1)

plot(ttr.val$PM2.5, exp(predicted) ,xlab= " PM2.5-Actual", ylab = " PM2.5-predicted" , main = " RKPuram -MLR Model Fit
     With PD_PM2.5",col = "red")
abline(0, 1)