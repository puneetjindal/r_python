if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(openxlsx,stringr,dplyr,mice,UsingR,ggplot2,reshape2,foreach)


elections_hypothesis <- read.xlsx("C:/Users/puneet.jindal/Desktop/elections_hypothesis.xlsx",
                                  sheet = "total_2014")

sapply(elections_hypothesis,class)

summary(elections_hypothesis)
elections_hypothesis$Liabilities <- as.numeric(gsub(",", "", elections_hypothesis$Liabilities))
elections_hypothesis$Total.Assets <- as.numeric(gsub(",", "", elections_hypothesis$Total.Assets))
elections_hypothesis$is_winner <- as.factor(elections_hypothesis$is_winner)

summary(elections_hypothesis)
hist(elections_hypothesis$Total.Assets)
boxplot(elections_hypothesis$Total.Assets)
elections_hypothesis[elections_hypothesis$Total.Assets < max(elections_hypothesis$Total.Assets)]

max_ta <- max(elections_hypothesis$Total.Assets)
eh_1<-filter(elections_hypothesis, elections_hypothesis$Total.Assets<max_ta)
boxplot(eh_1$Total.Assets)

max_ta <- max(eh_1$Total.Assets)
eh_2<-filter(eh_1, eh_1$Total.Assets<max_ta)
boxplot(eh_2$Total.Assets)
hist(eh_2$Total.Assets)

### outliers removal and replace with NA
source("http://goo.gl/UUyEzD")
outlierKD(elections_hypothesis, Total.Assets)

elections_hypothesis$Total.Assets[is.na(elections_hypothesis$Total.Assets)] = median(elections_hypothesis
                                                                                   $Total.Assets, na.rm=TRUE)

ggplot(aes(y = Total.Assets, x = is_winner), data = elections_hypothesis) + geom_boxplot()

t.test(Total.Assets ~ is_winner, data=elections_hypothesis) 

sample_1 <- elections_hypothesis[sample(1:nrow(elections_hypothesis), 50,replace=FALSE),] 
sample_2 <- elections_hypothesis[sample(1:nrow(elections_hypothesis), 50,replace=FALSE),] 

tempData <- mice(elections_hypothesis,m=5,method = "pmm")
densityplot(tempData)
complete_data <- complete(tempData,1)

t.test(complete_data$Total.Assets, mu=mean(complete_data$Total.Assets))


tempData <- mice(sample_1,m=5,method = "pmm")
complete_data <- complete(tempData,1)
t.test(complete_data$Total.Assets, mu=mean(complete_data$Total.Assets))
sample_3 <- elections_hypothesis[sample(1:nrow(elections_hypothesis), 50,replace=FALSE),]


##Step 1 remove outliers
##step 2 calculation population mean
##step 3 null hypothesis likelihood of win > 0.5 alternate hypothesis is the other one
##Step 4  
## Null hypothesis is winners have higher total assets greater than non winners

for(i in 1:100){
  temp_sample <- elections_hypothesis[sample(1:nrow(elections_hypothesis), 1500,replace=FALSE),] 
  y <- t.test(Total.Assets ~ is_winner, data=temp_sample)
  print (y$p.value) }

prop.test(table(elections_hypothesis$Total.Assets, elections_hypothesis$is_winner), correct=FALSE)


