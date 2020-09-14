# Single Company Word Cloud & Dendrogram R-Script
# Library not needed to be loaded everytime if you are running the all the different scripts one after another  

# Include Libraries

library(rtweet)
library(syuzhet)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(dplyr)
library(tidytext)
library(ape) 

# Twitter API Authentication

app.name <- "###########"
consumer.key <- "##############"
consumer.secret <- "#################"
token <- create_token (app = app.name, consumer_key = consumer.key, consumer_secret = consumer.secret)

# Search for Tweets (Not Case Sensitive)

# Option 1 - Include Retweets => include_rts (TRUE / FALSE)

# Option 2 - Tweet Type => type (recent / mixed / popular)

# Option 3 - Retry on Rate Limit => retryonratelimit (TRUE / FALSE)s

FirstSearch <- search_tweets("chicfila OR chickfila", n = 1000, include_rts = FALSE, token = token, lang="en", type= "mixed", retryonratelimit = FALSE)

#  Save Tweets as CSV File to Desktop

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(FirstSearch, paste0("~/Desktop/Chicfila-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  save_as_csv(FirstSearch, paste0("~\\Desktop\\Chicfila-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}

# Get Sentiment Data

FirstSearch.sentiment <- get_nrc_sentiment(FirstSearch$text)  


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

# Create Word Cloud

wordcloud(FirstSearch.corpus,  max.words = 100, random.order = FALSE, colors = brewer.pal(8, 'Set1') )

FirstSearch.tdm <- TermDocumentMatrix(FirstSearch.corpus, control = list(wordLengths=c(1, Inf)))
dimnames(FirstSearch.tdm)$Terms
findFreqTerms(FirstSearch.tdm, lowfreq = 20)

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/Chicfila-WordCloud ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\Chicfila-WordCloud ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

# Insert Terms

findAssocs(FirstSearch.tdm, "knife", 0.25)
findAssocs(FirstSearch.tdm, "stab", 0.25)
findAssocs(FirstSearch.tdm, "fight", 0.25)
findAssocs(FirstSearch.tdm, "brawl", 0.25)
findAssocs(FirstSearch.tdm, "gun", 0.25)
findAssocs(FirstSearch.tdm, "shoot", 0.25)
findAssocs(FirstSearch.tdm, "slam", 0.25)
findAssocs(FirstSearch.tdm, "kill", 0.25)
findAssocs(FirstSearch.tdm, "murder", 0.25)
findAssocs(FirstSearch.tdm, "sunday", 0.25)
findAssocs(FirstSearch.tdm, "homophobic", 0.25)

# Remove Sparse Terms

RemoveSparse <- removeSparseTerms(FirstSearch.tdm, sparse = 0.95)
RemoveSparse.matrix <- as.matrix(RemoveSparse)

# Hierarchical Clustering

distMatrix <- dist(scale(RemoveSparse.matrix))
FirstSearch.fit <-hclust(distMatrix, method ="ward.D")

# Plot Dendrogram

plot(FirstSearch.fit, cex=0.9, hang=1, main="Chicfila Word Cluster Dendrogram")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/Chicfila-Dendrogram ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\Chicfila-Dendrogram ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

# Cut Tree

rect.hclust(FirstSearch.fit, k=5)

# Other Histograms

plot(FirstSearch.fit, cex=0.9, hang=1,main="Chicfila Word Cluster Dendrogram")
plot(FirstSearch.fit, type = c("rectangle", "triangle"), horiz = FALSE)
nk.fit2 <- as.dendrogram(FirstSearch.fit)
plot(nk.fit2, type = "rectangle", ylab = "Height")
plot(nk.fit2, type = "triangle", ylab = "Height")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/Chicfila-Cluster-Dendrogram ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\Chicfila-Cluster-Dendrogram ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

# Customized Plot Remove Labels 

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
plot(nk.fit2, ylab = "Height", nodePar = nodePar, leaflab = "none")

# Colorful Plot 

plot(nk.fit2, xlim = c(1, 20), ylim = c(1,8))
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
plot(nk.fit2,  xlab = "Height", nodePar = nodePar, edgePar = list(col = 2:3, lwd = 2:1))

# Unrooted

plot(as.phylo(FirstSearch.fit), type = "unrooted", cex = 0.6, no.margin = TRUE)

# Fan

plot(as.phylo(FirstSearch.fit), type = "fan")


Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/Chicfila-Fan ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\Chicfila-Fan ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

# Radial

plot(as.phylo(FirstSearch.fit), type = "radial")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/Chicfila-Radial ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\Chicfila-Radial ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

# Clandogram 

plot(as.phylo(FirstSearch.fit), type = "cladogram", cex = 0.6, edge.color = "steelblue", edge.width = 2, edge.lty = 2, tip.color = "steelblue")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  dev.print(png, file = paste0("~/Desktop/Chicfila-Clandogram ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
} else {
  
  dev.print(png, file = paste0("~\\Desktop\\Chicfila-Clandogram ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), width = 1024, height = 1024, res = 200)
  
  dev.off()
  
}

