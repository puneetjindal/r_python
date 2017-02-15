library('e1071')
library('tm')
install.packages('C:/Users/puneet.jindal/Downloads/tm.corpus.Reuters21578_2015.01.21.tar.gz', repos = NULL, type="source")
library(tm.corpus.Reuters21578)
data("Reuters21578")
data("Reuters21578_DTM")


summary(Reuters21578)
inspect(Reuters21578[5])

docs <- tm_map(Reuters21578, removePunctuation)  
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, stopwords("english"))  

## remove any othr words which might not be of use. This will be useful in iterations
#docs <- tm_map(docs, removeWords, c("department", "email"))   

library('SnowballC')   
docs <- tm_map(docs, stemDocument)   

docs <- tm_map(docs, stripWhitespace)  
docs <- tm_map(docs, PlainTextDocument)  

dtm <- DocumentTermMatrix(docs)   
inspect(dtm[1:5, 1:20])   
inspect(Reuters21578_DTM[1:5, 1:20])

tdm <- TermDocumentMatrix(docs)   
tdm
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq) 
#If you prefer to export the matrix to Excel:   
m <- as.matrix(dtm)   
dim(m)   
#write.csv(m, file="dtm.csv")  dont run this on local system



dtms <- removeSparseTerms(dtm, 0.5) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms) 


str