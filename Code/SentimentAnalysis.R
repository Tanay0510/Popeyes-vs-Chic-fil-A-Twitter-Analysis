# Include Libraries
# Library not needed to be loaded everytime if you are running the all the different scripts one after another  
# Caution! When doing sentiment analysis of more than 100000 tweets, R will give error or take a lot of time depending on the system configuration  
# Recommended, if less than 8gb RAM, do sentiment analysis of around 50000 tweets to not run into any problem  
# Caution! Do sentiment analysis of tweets one at a time of you are doing it for multiple companies so that you do not get "Error: memory exhausted (limit reached?)" 
# If facing the above issue, refer to this https://stat.ethz.ch/R-manual/R-devel/library/base/html/Memory-limits.html
# Sentiment Analysis takes time, so be patient. Do not "STOP" the code if it is taking too much time, otherwise you may end up hanging your machine.


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

app.name <- "################"
consumer.key <- "###################"
consumer.secret <- "#######################3"
token <- create_token (app = app.name, consumer_key = consumer.key, consumer_secret = consumer.secret)

# Declare Name (1)

name1 <<- "Chicfila"

# Declare Name (2)

name2 <<- "Popeyes"

# Option 1 - Include Retweets => include_rts (TRUE / FALSE)

# Option 2 - Tweet Type => type (recent / mixed / popular)

# Option 3 - Retry on Rate Limit => retryonratelimit (TRUE / FALSE)

# Code Begin

FirstSearch <- search_tweets(q = "chicfila OR chickfila", n = 1000, include_rts = FALSE, token = token, lang="en", type = "mixed", retryonratelimit = FALSE)

SecondSearch <- search_tweets(q = "popeye OR popeyes", n = 1000, include_rts = FALSE, token = token, lang="en", type = "mixed", retryonratelimit = FALSE)

# Save Tweets as CSV File to Desktop

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(FirstSearch, paste0("~/Desktop/", name1 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  save_as_csv(FirstSearch, paste0("~/Desktop/", name2 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  save_as_csv(FirstSearch, paste0("~\\Desktop\\", name1 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  save_as_csv(FirstSearch, paste0("~\\Desktop\\", name2 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}

# Begin Graphs

# Add Theme

theme_ica17 <- function () { 
  theme_bw(base_size=10, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="gray96", colour=NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

# Create Query (Search) Variable

FirstSearch$query <- paste(name1)

SecondSearch$query <- paste(name2)

# Row Bind into Single Data Frame

df <- rbind(SecondSearch, FirstSearch)

# Create Plain Tweets

df$text_plain <- plain_tweets(df$text)

# Run Sentiment Analysis

sa <- syuzhet::get_nrc_sentiment(df$text_plain)

# Get Sentiment Data

FirstSearch.sentiment <- get_nrc_sentiment(FirstSearch$text)  
FirstSearch.sentiment

# recreate sentiment to create visualization
FirstSearch.sentimentVisualization <-cbind(FirstSearch[,c("created_at", "retweet_count", "favorite_count", "text")],FirstSearch.sentiment)


# Sentiment Visualization

SentimentVisualization <- reshape2::melt(FirstSearch.sentimentVisualization,
                     variable.name = "emotion",
                     value.name = "sentiment",
                     id.vars = c("created_at", "favorite_count", "retweet_count","text"))

SentimentPlot <- ggplot(SentimentVisualization, aes(x = emotion, y = sentiment,
                       fill = emotion)) + theme_minimal() +
  coord_cartesian(ylim = c(0, 10)) +
  geom_jitter(color = "#ffffff", shape = 21,
              size = 2, alpha = .7, stroke = .15) +
  coord_flip() + labs(y = "", x = "", title = paste(name1, "NRC Sentiment")) +
  theme(legend.position = "none",
        text = element_text("Georgia", size = 18)
        # ,axis.text.x = element_blank()
  )

SentimentPlot

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(FirstSearch.sentiment, paste0("~/Desktop/", name1 ,"-Sentiment-Data ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  ggsave(paste0("~/Desktop/", name1 ,"-Sentiment ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".jpg"))
  
} else {
  
  save_as_csv(FirstSearch.sentiment, paste0("~\\Desktop\\", name1 ,"-Sentiment-Data ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  ggsave(paste0("~\\Desktop\\", name1 ,"-Sentiment ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".jpg"))
  
}

# Get Sentiment Data

SecondSearch.sentiment <- get_nrc_sentiment(SecondSearch$text)  
SecondSearch.sentiment

#recreate sentiment to create visualization
SecondSearch.sentimentVisualization <-cbind(SecondSearch[,c("created_at", "retweet_count", "favorite_count", "text")],SecondSearch.sentiment)
SecondSearch.sentimentVisualization

# Sentiment Visualization

SentimentVisualization_2 <- reshape2::melt(SecondSearch.sentimentVisualization,
                     variable.name = "emotion",
                     value.name = "sentiment",
                     id.vars = c("created_at", "favorite_count", "retweet_count","text"))

SentimentPLot_2 <- ggplot(SentimentVisualization_2, aes(x = emotion, y = sentiment,
                       fill = emotion)) + theme_minimal() +
  coord_cartesian(ylim = c(0, 10)) +
  geom_jitter(color = "#ffffff", shape = 21,
              size = 2, alpha = .7, stroke = .15) +
  coord_flip() + labs(y = "", x = "", title = paste(name2, "NRC Sentiment")) +
  theme(legend.position = "none",
        text = element_text("Georgia", size = 18)
        # ,axis.text.x = element_blank()
  )

SentimentPLot_2

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(FirstSearch.sentiment, paste0("~/Desktop/", name2 ,"-Sentiment-Data ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  ggsave(paste0("~/Desktop/", name2 ,"-Sentiment ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".jpg"))
  
} else {
  
  save_as_csv(FirstSearch.sentiment, paste0("~\\Desktop\\", name2 ,"-Sentiment-Data ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  ggsave(paste0("~\\Desktop\\", name2 ,"-Sentiment ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".jpg"))
  
}

# view output
tibble::as_tibble(sa)

# bind columns
df <- cbind(df, sa)

# load dplyr
suppressPackageStartupMessages(library(dplyr))

# create function for aggregating date-time vectors
round_time <- function(x, interval = 60) {
  # round off to lowest value
  rounded <- floor(as.numeric(x) / interval) * interval
  # center so value is interval mid-point
  rounded <- rounded + round(interval * .5, 0)
  # return to date-time
  as.POSIXct(rounded, origin = "1970-01-01")
}

# use pipe (%>%) operator for linear syntax

long_emotion_ts <- df %>%
  # select variables (columns) of interest
  dplyr::select(created_at, query, anger:positive) %>%
  # convert created_at variable to desired interval
  # here I chose 6 hour intervals (3 * 60 seconds * 60 mins = 3 hours)
  mutate(created_at = round_time(created_at, 3 * 60 * 60)) %>%
  # transform data to long form
  tidyr::gather(sentiment, score, -created_at, -query) %>%
  # group by time, query, and sentiment
  group_by(created_at, query, sentiment) %>%
  # get mean for each grouping
  summarize(score = mean(score, na.rm = TRUE),
            n = n()) %>%
  ungroup()

# View Data

long_emotion_ts

# Sentiment over Time Graphs

# date_breaks = hours or hour?

# date_labels = day(%d),  %Y-%m-%d_%H-%M

long_emotion_ts %>%
  ggplot(aes(x = created_at, y = score, color = query)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~ sentiment, scale = "free_y", nrow = 2) +
  theme_bw() +
  theme(text = element_text(family = "Georgia"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 9),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Twitter Sentiment Analysis over Time",
       subtitle = paste("Tweets Aggregated for", name1, "and", name2)) +
  scale_x_datetime(date_breaks = "48 hours", date_labels = "%d")


Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  ggsave(paste0("~/Desktop/", name1 ,"-", name2 ,"-Sentiment ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".jpg"))  
  
} else {
  
  ggsave(paste0("~\\Desktop\\", name1 ,"-", name2 ,"-Sentiment ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".jpg")) 
  
}    

