library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

# Library for parallel processing

library(doMC)
registerDoMC(cores=detectCores()) 

df<- read.csv("File Path",row.names=NULL,stringsAsFactors = FALSE)
glimpse(df)

set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)

df$Ratings <- as.factor(df$Ratings)
corpus <- Corpus(VectorSource(df$Tweet))
corpus
inspect(corpus[1:3])
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus.clean)

# Inspect the dtm
inspect(dtm[40:50, 10:15])

df.train <- df[1:12748,]
df.test <- df[12749:16998,]

dtm.train <- dtm[1:12748,]
dtm.test <- dtm[12749:16998,]

corpus.clean.train <- corpus.clean[1:12748]
corpus.clean.test <- corpus.clean[12749:16998]

dim(dtm.train)
fivefreq <- findFreqTerms(dtm.train, 100)
length((fivefreq))

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)


dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)


convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Naive Bayes 

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
system.time( classifier <- naiveBayes(trainNB, df.train$Ratings, laplace = 1) )
system.time( pred <- predict(classifier, newdata=testNB) )
table("Predictions"= pred,  "Actual" = df.test$Ratings )
conf.mat <- confusionMatrix(pred, df.test$Ratings)

conf.mat
conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']




