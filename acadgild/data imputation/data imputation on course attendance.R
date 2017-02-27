library(readr)
data <- read_csv("~/Downloads/HMXPC13_DI_v2_5-14-14.csv", 
                 col_types = cols(certified = col_character(), 
                                  explored = col_character(), 
                                  incomplete_flag = col_character(),
                                  last_event_DI = col_date(format = "%Y-%m-%d"),
                                  nchapters = col_integer(), registered = col_character(),
                                  start_time_DI = col_date(format = "%Y-%m-%d"),
                                  viewed = col_character()))
colnames(data)
unique(data$course_id)
data$userid_DI<- NULL

unique(data$registered)
data$registered<- NULL

library('plyr')
table(data$explored)[1]
# % explored

library('Hmisc')
describe(data)

data$YoB <- as.factor(data$YoB)

data$roles<- NULL
data$nchapters<-NULL
54097700/641138

data$incomplete_flag<- NULL