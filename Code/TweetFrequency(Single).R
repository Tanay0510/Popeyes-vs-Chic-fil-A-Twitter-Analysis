# Tweet Frequency (Single)
# Library not needed to be loaded everytime if you are running the all the different scripts one after another  

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
library(TSstudio)


# Twitter API Authentication

app.name <- "##########"
consumer.key <- "#############"
consumer.secret <- "################"
token <- create_token (app = app.name, consumer_key = consumer.key, consumer_secret = consumer.secret)

# Declare Name

name <<- "Chicfila"

# Declare Tweet Query - Not Case Sensitive

# Options

# Option 1 - Include Retweets => include_rts (TRUE / FALSE)

# Option 2 - Tweet Type => type (recent / mixed / popular)

# Option 3 - Language => lang 

## ISO Language Codes (https://www.iso.org/iso-639-language-codes.html)

nk <- search_tweets(q = "chicfila OR chickfila", n = 1000, include_rts = FALSE, token = token, lang="en", type = "mixed", retryonratelimit = FALSE)

# https://www.rdocumentation.org/packages/rtweet/versions/0.6.9/topics/search_tweets

# Save Tweets as CSV File to Desktop

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(nk, paste0("~/Desktop/", name ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  save_as_csv(nk, paste0("~\\Desktop\\", name ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}

# Frequency of Tweets

# Option 1 - secs, mins, hours, days, weeks, months, or years

agg1 <- ts_data(nk, "days")

agg1

ts_plot(agg1, title = "Time Series of Tweets", Xtitle = "Date and Time", Ytitle = "Frequency of Tweets", slider = TRUE, line.mode =  "lines+markers")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  htmlwidgets::saveWidget(ts_plot(agg1), paste0("~/Desktop/", name ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
} else {
  
  htmlwidgets::saveWidget(ts_plot(agg1), paste0("~\\Desktop\\", name ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
}


