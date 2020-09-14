# Tweet Ratings (Single)
# Reads the .csv file and rate each tweet as Positive Negative using "get_sentiment", which rated each row from -5 to +5  
# Rated tweets as "Positive" if rating was greater than 0, and "Negative" if it was less than 0  
# Caution! Takes time to rate if there is a large number of tweets. 


# Include Libraries

library(SnowballC)
library(tm)
library(syuzhet)
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

# Parallel Processing Library

library(doMC)

registerDoMC(cores=detectCores())

name <<- "Chicfila"

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  FirstSearch <- read.csv("~/Desktop/", name ,"-Tweets.csv", stringsAsFactors = FALSE)
  
} else {
  
  FirstSearch <- read.csv("~\\Desktop\\", name ,"-Tweets.csv", stringsAsFactors = FALSE)
  
}

glimpse(FirstSearch)
FirstSearchClean <- data.frame(FirstSearch$text)
FirstSearchClean <- gsub("http.*","",FirstSearch$text)
FirstSearchClean <- gsub("https.*","",FirstSearchClean)
FirstSearchClean <- gsub("#.*","",FirstSearchClean)
FirstSearchClean <- gsub("@.*","",FirstSearchClean)
FirstSearchClean <- gsub("\U.*","",FirstSearchClean)
head(FirstSearchClean)

FirstSearchVector <- as.vector(FirstSearchClean)
FirstSearch.Sentiment <- get_nrc_sentiment(FirstSearchVector)
Search_With_Sentiment <- cbind(FirstSearchClean, FirstSearch.Sentiment) 
head(Search_With_Sentiment)

sent.value <- get_sentiment(FirstSearchVector)

most.positive <- FirstSearchVector[sent.value == max(sent.value)]
most.positive

most.negative <- FirstSearchVector[sent.value <= min(sent.value)] 
most.negative 

Category_Sent <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive"))

Category_SentRated <- cbind(FirstSearch$text,Category_Sent)
head(Category_SentRated)

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  write.csv(Category_SentRated, file ="~/Desktop/", name ,"-Tweet-Ratings.csv",row.names = F)
  
} else {
  
  write.csv(Category_SentRated, file ="~\\Desktop\\", name ,"-Tweet-Ratings.csv",row.names = F)
  
}






