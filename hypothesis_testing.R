# Z Table http://www.stat.ufl.edu/~athienit/Tables/Ztable.pdf
setwd('~/r_python/')
library('data.table')
library('ggplot2')
data <- fread('~/Downloads/CLT.csv')
mean(sample(data[,Marks],size = 50,replace = T))
mean(data[,Marks])
sd(sample(data[,Marks],size = 50,replace = T))
sd(data[,Marks])/sqrt(1000)
