d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
View(mydata)
colnames(mydata)
colnames(mydata)[3]
names(mydata)

## renaming columns
names(mydata) <- c("ID","Color","Passed") # variable names 
names(mydata)[2] <- "col_name"

### to find the index of dataframe
row.names(mydata)

## length of data frame 
nrow(mydata)
length(colnames(mydata))
dim(mydata)   ## dimesnion of the dataframe

#####
#create a new dataframe with column d and e
df_new = data.frame()
df_new <- cbind(d,e)
View(df_new)
df_new <- cbind(df_new,f)
## try rbind also

rm(df_new)
ls()
#rm(ls())


## identify datatypes of columns
class(d)
class(mydata)
sapply(mydata,class)
mydata$g <- as.integer(mydata$d) 

### identify NA values
is.na(mydata)
is.na(mydata$d)
colSums(is.na(mydata))

library(Amelia)
?Amelia

## 
is.numeric(mydata$e)

## indexing and selecting data based on column condition of dataframe
mydata[mydata$f==TRUE,]

###sorrt a dataframe by column
mydata[order(-g),]
#http://www.statmethods.net/management/sorting.html

#### mice imputation library
library("mice")

########data management
mydata$agecat <- ifelse(mydata$age > 70,c("older"), c("younger")) 

mydata$agecat[age > 75] <- "Elder"
mydata$agecat[age > 45 & age <= 75] <- "Middle Aged"
mydata$agecat[age <= 45] <- "Young"
x[(x>8) | (x<5)]

