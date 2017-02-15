install.packages("Rserve")
library("Rserve")

# data exploration of a file
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage('gdata')
usePackage('moments')
usePackage('Amelia')
usePackage('extRemes')
usePackage('dummies')
usePackage('ggplot2')
usePackage('randomForest')
usePackage('miscTools')
usePackage('car')
usePackage('stats')
usePackage('corpcor')
usePackage('corrplot')
usePackage('MASS')
usePackage("usdm")
usePackage("psych")
usePackage("Hmisc")
usePackage("MASS")
usePackage("lattice")
usePackage('caTools')
usePackage('dplyr')
usePackage('mice')
usePackage('forecast')
usePackage('rPython')
install.packages("rPython")
usePackage('devtools')
usePackage('caret')
usePackage('xgboost')
usePackage('readr')
usePackage('shiny')

############read the data ############
mydata<- read.csv("~/data explore csv.csv")
View(mydata)
missmap(mydata, main = "Missing values vs observed")

mydata <- mydata[,c(2:22)]
View(mydata)

describe(mydata)
#  27 hotels
# res. status 15% cancelled, 82% modified, 4% new, 3 observations need to be eliminated based on objective
# 

#################aggregate() and then plot


summary(mydata$new_book_time)
 # Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#  "2014-05-15" "2014-06-20" "2014-06-25" "2014-06-24" "2014-06-28" "2014-06-30" 

summary(mydata$new_checkin)
#  Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#  "2013-06-30" "2014-06-29" "2014-07-12" "2014-07-27" "2014-08-01" "2016-08-19" 

############Remove the conformation number########
#Seems of no use. can be explored later for if a particular series is cancelled
mydata$Confirmation <- NULL


########working with categorical variables#########
sapply(mydata, class)
####### Cleaning the factor to date format
mydata$new_export_date<- as.Date(mydata$Export.Date, format = "%m/%d/%Y")
#we can remove export date. it probably can be used as a benchamrk that 
#all the booking date has to be earlier than this
mydata$new_book_time<- as.Date(mydata$Book.Time, format = "%m/%d/%Y")
mydata$new_checkin<- as.Date(mydata$CheckIn, format = "%m/%d/%Y")
mydata$new_checkout<- as.Date(mydata$CheckOut, format = "%m/%d/%Y")


################# Univariate Analysis ##############
hist(mydata$Persons)
boxplot(mydata$Persons, horizontal=TRUE)

hist(mydata$Rooms)
boxplot(mydata$Rooms, horizontal=TRUE)

hist(mydata$Room.Amount)
boxplot(mydata$Room.Amount, horizontal=TRUE)

################# Bivariate Analysis ##############
boxplot(mydata$Persons~mydata$Market.Segment)
boxplot(mydata$Room.Amount~mydata$Market.Segment)

ggplot(mydata, aes(mydata$new_book_time, mydata$Persons)) + 
  geom_line() + xlab("Booking Date") + ylab("Persons count distribution")
ggplot(mydata, aes(mydata$new_book_time, mydata$Rooms)) + 
  geom_line() + xlab("Booking Date") + ylab("Rooms count distribution")

ggplot(mydata, aes(mydata$new_checkin, mydata$Persons)) + 
  geom_line() + xlab("Checkin Date") + ylab("Persons count distribution")
ggplot(mydata, aes(mydata$new_checkin, mydata$Rooms)) + 
  geom_line() + xlab("Checkin Date") + ylab("Rooms count distribution")


ggplot(mydata, aes(mydata$Persons, mydata$Channel)) + geom_point(aes(colour=factor(mydata$Res.Status))) +
  scale_colour_manual(values=Palette1)


