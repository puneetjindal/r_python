#what is data exploration in R
##https://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/#one

data <- mtcars

#puneet_jindal_2014@cba.isb.edu

#How to load data file(s)?
# Read CSV into R
MyData <- read.csv(file="acadgild/20-02-2017/final_data.csv", header=TRUE, sep=";")

#Read a Tab seperated file
Tabseperated <- read.table("c:/TheDataIWantToReadIn.txt", sep="\t", header=TRUE)




##
#data structures in R
# homogenous  - vector(1d), matrix(2d) , array(n-dimensional)
# heterogenous - list(1d), dataframe(2d)

#vector
a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

a[c(2,4)] # 2nd and 4th elements of vector
a[2:4]

# matrices
#Lets generate 5 x 4 numeric matrix
y<-matrix(1:20, nrow=5,ncol=4)

# another example
cells <- c(1,26,24,68,67,87)
rnames <- c("R1", "R2","R3")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells, nrow=3, ncol=2, byrow=TRUE,dimnames=list(rnames, cnames)) 

mymatrix[,2] # 4th column of matrix
mymatrix[,3] # 4th column of matrix

mymatrix[3,] # 3rd row of matrix
mymatrix[4,] # 4rd row of matrix

mymatrix[1:5,1:3] # rows 2,3,4 of columns 1,2,3
mymatrix[1:2,1:2]

##Error in x[1:2, 1:2] : incorrect number of dimensions


###arrays  multidimensional
x <- array(1:20, dim=c(1,4,5))
##TODOpractise


### List An ordered collection of objects (components). lists
# example of a list with 4 components -
# a string, a numeric vector, a matrix, and a scaler
w1 <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)


##TODOpractise
#Student performance problem statment
#a) his name
#b) 5 months of performance- a, b, c, b,f
#c) list(w1,w2....,wn) of students in my class subsequently for the whole class/school
##TODOpractise MongoDB how MongoDB stores it and enables filtering,projections,aggregations


# example of a list containing two lists
v <- c(list1,list2) 
#Identify elements of a list using the [[]] convention. 
mylist[[2]] # 2nd component of the list
mylist[["mynumbers"]] # component named mynumbers in list

## dataframes in R
#A data frame is more general than a matrix, in that different columns can have different modes 
#(numeric, character, factor, etc.).
d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") # variable names 

mydata[3:5,] # columns 3,4,5 of data frame
mydata[c("ID","Color")] # columns ID and Age from data frame
#mydata$X1 # variable x1 in the data frame 
MyData$X <- NULL

## factor
# variable gender with 20 "male" entries and
# 30 "female" entries
gender <- c(rep("male",20), rep("female", 30))
gender <- factor(gender)
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
# R now treats gender as a nominal variable
summary(gender) 

##Factors represent a very efficient way to store character values, because each unique character value is stored only once, and the data itself is stored as a vector of integers.
mons = c("March","April","January","November","January","September","October","September","November",
         "August","January","November","November","February","May","August","July","December","August",
         "August","September","November","February","April")

mons = factor(mons)
table(mons)
#Although the months clearly have an ordering, this is not reflected in the output of the table function.
#Additionally, comparison operators are not supported for unordered factors.
mons = factor(mons,levels=c("January","February","March","April","May","June","July","August",
                            "September","October","November","December"),ordered=TRUE)
mons[1] < mons[2]
table(mons)




## data type conversions
#convert a variable to different data type
is.numeric()
is.character()
is.vector()
is.matrix()
is.data.frame()

#as.numeric()  very important
as.character()
as.vector()
#as.matrix()   very important 
as.data.frame()
#http://stackoverflow.com/questions/5158790/should-i-use-a-data-frame-or-a-matrix

###
##
#Take a numeric vector vec <- c(1,2,5,3,5,7)
#  vec_fac<- as.factor(vec)
## vec <- as.numeric(vec_fac)---
#http://www.dummies.com/programming/r/how-to-convert-a-factor-in-r/


a=c(10,9,3,8,8,10,4,10,10)
is.numeric(a)
a_fac <- as.factor(a)
a_int <- as.numeric(a_fac)


##no brainer functions in R
# str(), summary(), dim(), colnames(), names(), class(),head(),tail()

length(object) # number of elements or components
str(object)    # structure of an object
class(object)  # class or type of an object
names(object)  # names
ls()       # list current objects
rm(object) # delete an object






#### Hmisc and describe function
install.packages('Hmisc')
library('Hmisc')
help("describe")

library('moments')
## mean(), median(), range(), var() ,quartiles in moments package





### Control Structures in R
#ifelse returns a value with the same shape as test which is filled with elements 
#selected from either yes or no depending on whether the element of test is TRUE or FALSE. 
x <- -5
if(x >= 0){
  print("Non-negative number")
} else {
  print("Negative number")
}

x <- 0
if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else
  print("Zero")






### looping in R
z <- c(-2:3)
for (x in z){
  if (x < 0) {
    print("Negative number")
  } else if (x > 0) {
    print("Positive number")
  } else
    print("Zero")
}






## functions in R
square_int <- function(x) {
  square <- x * x
  return(square)
}
square_int(-2)






### merge datasets in R
#cbind() and rbind() functions
c(object,object,...)       # combine objects into a vector
cbind(object, object, ...) # combine objects as columns very frequently used
rbind(object, object, ...) # combine objects as rows 

## adding columns
#To merge two data frames (datasets) horizontally, use the merge function.
# merge two data frames by ID
total <- merge(dataframeA,dataframeB,by="ID")
# merge two data frames by ID and Country
total <- merge(dataframeA,dataframeB,by=c("ID","Country")) 


#To join two data frames (datasets) vertically, use the rbind function
authors <- data.frame(
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

m1 <- merge(authors, books, by.x = "surname", by.y = "name")





### apply, sapply and lapply functions in R
#apply
"""
When do we use apply? When we have some structured blob of data that we wish to perform
operations on. Here structured means in some form of matrix. The operations may be 
informational, or perhaps transforming, subsetting, whatever to the data.
We tell apply to traverse row wise or column wise by the second argument. 
In this case we expect to get three numbers at the end, the mean value for each column,
so tell apply to work along columns by passing 2 as the second argument. 
"""
apply(mtcars, 2, mean)
apply(mtcars, 1, mean)

# But when it comes to iterate over multiple columns or multiple rows at once 
#These two functions work in a similar way, traversing over a set of data like a list 
#or vector, and calling the specified function for each item.

sapply(1:3, function(x) x^2)
#lapply is very similar, however it will return a list rather than a vector:
lapply(1:3, function(x) x^2)

sapply(mtcars,class)
lapply(mtcars,class)
## just try to look at unlist function in R


### parallel computing in R
install.packages('doParallel')  
library(doParallel) 
library(foreach)

getPrimeNumbers <- function(n) {  
  n <- as.integer(n)
  if(n > 1e6) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  for(i in last.prime:floor(sqrt(n)))
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
  }
  which(primes)
}

system.time(result <- foreach(i=10:100000000) %do% getPrimeNumbers(i))

library(doParallel)  
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  
system.time(result <- foreach(i=10:100000000) %dopar% getPrimeNumbers(i))
stopCluster(cl)
## overhead



## reshaping dataframes in R(tidy and reshape package in R)
#gather(), separate() and spread(), from tidyr,
#melt(), colsplit() and dcast(), from reshape2.
install.packages('tidyr')
library('tidyr')
library('reshape2')
#columns to values or remember like wide to long format
set.seed(10)
messy <- data.frame(id = 1:4,
                    trt = sample(rep(c('control', 'treatment'), each = 2)),
                    work.T1 = runif(4),
                    home.T1 = runif(4),
                    work.T2 = runif(4),
                    home.T2 = runif(4))
View(messy)
gathered.messy <- gather(messy, key, value, -id, -trt)
head(gathered.messy)
## same result from melt function
molten.messy <- melt(messy, 
                     variable.name = "key",
                     value.names = "value",
                     id.vars = c("id", "trt"))
head(molten.messy)

"""
 gather function has brought messy into a long data format with a warning by treating all 
columns as variable, while melt() has treated trt as an ???id variables???. Id columns 
are the columns that contain the identifier of the observation that is represented
as a row in our data set. Indeed, if melt() does not receive any id.variables 
specification, then it will use the factor or character columns as id variables. 
gather() requires the columns that needs to be treated as ids, all the other columns 
are going to be used as key-value pairs.
gather() cannot handle matrices or arrays, while melt() can
"""
sapply(gathered.messy,class)
###Split a column: separate() vs colsplit()
##
##tidy <- separate(gathered.messy,key, into = c("location", "time"), sep = "\.") 
res.tidy <- cbind(molten.messy[1:2], colsplit(molten.messy[, 3], ".", c("location", "time")),molten.messy[4])


tidy <- separate(gathered.messy,
                 key, into = c("location", "time"), sep = "\.") 
res.tidy <- cbind(molten.messy[1:2], 
                  colsplit(molten.messy[, 3], "\.", c("location", "time")),
                  molten.messy[4])

head(tidy)
###colsplit() operates only on a single column we usecbind() to insert the new two 
#columns in the data frame. separate() performs all the operation at once reducing the
#possibility of making mistakes.

##long to the wide format: spread() vs dcast()

set.seed(14)
stocks <- data.frame(time = as.Date('2009-01-01') + 0:9,
                     X = rnorm(10, 0, 1),
                     Y = rnorm(10, 0, 2),
                     Z = rnorm(10, 0, 4))
stocksm <- gather(stocks, stock, price, -time)
spread.stock <- spread(stocksm, stock, price)
head(spread.stock)

cast.stock <- dcast(stocksm, formula = time ~ stock, value.var = "price")
head(cast.stock)


head(tips)
m.tips <- melt(tips)
#dcast() to get information on the average total bill, tip and group size per day and time:
dcast(m.tips, day+time ~ variable, mean)

#Averages per smoker or not in the group.
dcast(m.tips, smoker ~ variable, mean)
##we use tidyr gather() and separate() functions to quickly tidy our data and reshape2dcast() to aggregate them.





##How to transpose a Data set?
t(dataset)




## aggregate function in R
aggdata <-aggregate(mtcars, by=list(mtcars$cyl,mtcars$gear), FUN=mean, na.rm=TRUE)



### product sums in R
cumsum(1:10)
cumprod(1:10)
## share in tomorrow lecture on prod sums 

## generally useful in times series data


## sorting dataframes in R
newdata <- mtcars[order(mtcars$mpg),] 
# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mpg, -cyl),] 



## sampling in R especially random sampling
# select variables v1, v2, v3
myvars <- c("d", "f")
newdata <- mydata[myvars]

# exclude variables v1, v2, v3
myvars <- names(mydata) %in% c("v1", "v2", "v3")
newdata <- mydata[!myvars]

# first 5 observations
newdata <- mydata[1:5,]

# based on variable values
newdata <- mydata[ which(mydata$gender=='F'
                         & mydata$age > 65), ]

# or
attach(mydata)
newdata <- mydata[ which(gender=='F' & age > 65),]
detach(mydata) 

# using subset function
newdata <- subset(mydata, age >= 20 | age < 10,select=c(ID, Weight)) 

# take a random sample of size 50 from a dataset mydata
# sample without replacement
mysample <- mydata[sample(1:nrow(mydata), 50,replace=FALSE),] 
## createDataPartition() in caret


## remove duplicates in R
unique()

a <- c(rep("A", 3), rep("B", 3), rep("C",2))
b <- c(1,1,2,4,1,1,2,2)
df <-data.frame(a,b)
duplicated(df)
df[!duplicated(df), ]


library(dplyr)
df %>% distinct

## check this command
#df %>% distinct(a, .keep_all = TRUE)



## data visualization in R
#https://www.analyticsvidhya.com/blog/2015/07/guide-data-visualization-r/
#https://www.analyticsvidhya.com/blog/2015/08/cheat-sheet-data-visualization-r/


## walkthrough kaggle

#Let me know any doubts.

#few important use cases you will encounter at your workplace
#https://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
#http://blog.h2o.ai/2016/04/fast-csv-writing-for-r/?_ga=1.128718863.2013057348.1469853273
## introduction to dplyr package in R

##data.table package in R

#I am reachable at puneet_jindal_2014@cba.isb.edu