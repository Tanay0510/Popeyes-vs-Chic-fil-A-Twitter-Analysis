# Single Company Word Cloud & Dendrogram R-Script

# Include Libraries

library(rtweet)
library(syuzhet)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud2)
library(RWeka)
library(dplyr)
library(tidytext)
library(ape) 

# Twitter API Authentication

app.name <- "############"
consumer.key <- "#################"
consumer.secret <- "#####################"
token <- create_token (app = app.name, consumer_key = consumer.key, consumer_secret = consumer.secret)

# Declare Name

name <<- "Chicfila"

# Declare Tweet Query - Not Case Sensitive

# Options

# Option 1 - Include Retweets => include_rts (TRUE / FALSE)

# Option 2 - Tweet Type => type (recent / mixed / popular)

# Option 3 - Language => lang 

## ISO Language Codes (https://www.iso.org/iso-639-language-codes.html)

FirstSearch <- search_tweets(q = "chicfila OR chickfila", n = 1000, include_rts = FALSE, token = token, lang="en", type = "mixed", retryonratelimit = FALSE)

# https://www.rdocumentation.org/packages/rtweet/versions/0.6.9/topics/search_tweets

# Save Tweets as CSV File to Desktop

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(FirstSearch, paste0("~/Desktop/", name ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  save_as_csv(FirstSearch, paste0("~\\Desktop\\", name ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}

# Get Sentiment Data

FirstSearch.sentiment <- get_nrc_sentiment(FirstSearch$text)  
FirstSearch.sentiment

FirstSearch.sentimentVisualization <-cbind(FirstSearch[,c("created_at", "retweet_count", "favorite_count", "text")],FirstSearch.sentiment)
# Create Corpus

FirstSearch.corpus <- Corpus(VectorSource(FirstSearch$text))
FirstSearch.corpus <- tm_map(FirstSearch.corpus, content_transformer(tolower))
FirstSearch.corpus <- tm_map(FirstSearch.corpus, removeWords, stopwords("english"))
FirstSearch.corpus <- tm_map(FirstSearch.corpus, removeWords, c("nk"))
FirstSearch.corpus <- tm_map(FirstSearch.corpus, removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x))
FirstSearch.corpus <- tm_map(FirstSearch.corpus, content_transformer(removePunctuation))
FirstSearch.corpus <- tm_map(FirstSearch.corpus, stripWhitespace)
FirstSearch.corpus <- tm_map(FirstSearch.corpus, removeNumbers)
FirstSearch.corpus <- tm_map(FirstSearch.corpus, stemDocument)

# Word Cloud

FirstSearch.tdm <- TermDocumentMatrix(FirstSearch.corpus, control = list(wordLengths=c(1, Inf)))
dimnames(FirstSearch.tdm)$Terms

FirstSearch.tdm <- as.matrix(TermDocumentMatrix(FirstSearch.corpus, control = list(wordLengths=c(1, Inf))))
FreqMat <- data.frame(ST = rownames(FirstSearch.tdm), 
                      Freq = rowSums(FirstSearch.tdm), 
                      row.names = NULL)
head(FreqMat,10)
v <- sort(rowSums(FirstSearch.tdm), decreasing = TRUE)
TopWords <- data.frame(word = names(v), freq = v)

head(TopWords,10)
row.names(TopWords) <- NULL
head(TopWords,10)

wordcloud2(TopWords, size = 3.0, main = "Word Cloud")

wordcloud2(TopWords, size = 3.0)

# Frequent Terms Histogram

barplot(TopWords[1:10,]$freq, las = 2, names.arg = TopWords[1:10,]$word, col ="lightblue", main =paste(name, "Most Frequent Words"), ylab = "Word Frequencies")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/", name ,"-Word-Freq-Histo ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\", name ,"-Word-Freq-Histo ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

barplot(TopWords[1:10,]$freq, las = 2, names.arg = TopWords[1:10,]$word, col ="lightblue", main =paste(name, "Most Frequent Words"), ylab = "Word Frequencies")

# Word Association

FirstSearch.tdm <- TermDocumentMatrix(FirstSearch.corpus, control = list(wordLengths=c(1, Inf)))

knife <- findAssocs(FirstSearch.tdm, "knife", 0.25)
print(knife)
stab <- findAssocs(FirstSearch.tdm, "stab", 0.25)
print(stab)
fight <- findAssocs(FirstSearch.tdm, "fight", 0.25)
print(fight)
brawl <- findAssocs(FirstSearch.tdm, "brawl", 0.25)
print(brawl)
gun <- findAssocs(FirstSearch.tdm, "gun", 0.25)
print(gun)
shoot <- findAssocs(FirstSearch.tdm, "shoot", 0.25)
print(shoot)
slam <- findAssocs(FirstSearch.tdm, "slam", 0.25)
print(slam)
kill <-findAssocs(FirstSearch.tdm, "kill", 0.25)
print(kill)
murder <-findAssocs(FirstSearch.tdm, "murder", 0.25)
print(murder)
sunday <-findAssocs(FirstSearch.tdm, "sunday", 0.25)
print(sunday)
mcdonalds <- findAssocs(FirstSearch.tdm, "mcdonalds", 0.25)
print(mcdonalds)
taco <- findAssocs(FirstSearch.tdm, "taco", 0.25)
print(taco)
raw <- findAssocs(FirstSearch.tdm, "raw", 0.25)
print(raw)

# Automatically create a table with all the associated words  

wordagg <- rbind(knife, stab, fight, brawl, gun, shoot, slam, kill, murder, sunday, mcdonalds, taco, raw)

colnames(wordagg) <- c('Word and Score')

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  write.csv(wordagg, paste0("~/Desktop/", name ,"-Word-Assoc ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  write.csv(wordagg, paste0("~\\Desktop\\", name ,"-Word-Assoc ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}


