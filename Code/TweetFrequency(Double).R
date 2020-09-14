# Tweet Frequency (Multiple)
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
library(doMC)
library(TSstudio)

registerDoMC(cores=detectCores())

# Twitter API Authentication

app.name <- "###########"
consumer.key <- "##################"
consumer.secret <- "##################"
token <- create_token (app = app.name, consumer_key = consumer.key, consumer_secret = consumer.secret)

# Declare Name (1)

name1 <<- "Chicfila"

# Declare Name (2)

name2 <<- "Popeyes"

# Declare Tweet Query - Not Case Sensitive

# Options

# Option 1 - Include Retweets => include_rts (TRUE / FALSE)

# Option 2 - Tweet Type => type (recent / mixed / popular)

# Option 3 - Language => lang 

## ISO Language Codes (https://www.iso.org/iso-639-language-codes.html)

nk <- search_tweets(q = "chicfila OR chickfila", n = 1000, include_rts = FALSE, token = token, lang="en", type = "mixed", retryonratelimit = FALSE)

cbo <- search_tweets(q = "popeye OR popeyes", n = 1000, include_rts = FALSE, token = token, lang="en", type = "mixed", retryonratelimit = FALSE)

# https://www.rdocumentation.org/packages/rtweet/versions/0.6.9/topics/search_tweets

# Save Tweets as CSV File to Desktop

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(nk, paste0("~/Desktop/", name1 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  save_as_csv(nk, paste0("~/Desktop/", name2 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  save_as_csv(nk, paste0("~\\Desktop\\", name1 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  save_as_csv(nk, paste0("~\\Desktop\\", name2 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}

# Frequency of Tweets (1)

# Option 1 - secs, mins, hours, days, weeks, months, or years

agg1 <- ts_data(nk, "days")

agg1

ts_plot(agg1, title = "Time Series of Tweets (Interval: Days)", Xtitle = "Date and Time", Ytitle = "Frequency of Tweets", slider = TRUE, line.mode =  "lines+markers")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  htmlwidgets::saveWidget(ts_plot(agg1), paste0("~/Desktop/", name1 ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
} else {
  
  htmlwidgets::saveWidget(ts_plot(agg1), paste0("~\\Desktop\\", name1 ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
}

agg2 <- ts_data(cbo, "days")

agg2

# Frequency of Tweets (2)

ts_plot(agg1, title = "Time Series of Tweets (Interval: Days)", Xtitle = "Date and Time", Ytitle = "Frequency of Tweets", slider = TRUE, line.mode =  "lines+markers")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  htmlwidgets::saveWidget(ts_plot(agg1), paste0("~/Desktop/", name2 ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
} else {
  
  htmlwidgets::saveWidget(ts_plot(agg1), paste0("~\\Desktop\\", name2 ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
}

# Frequency of Tweets (1 & 2) 

dfagg <- cbind(agg1, agg2)

colnames(dfagg) <- c('Time','", name1 ,"','Time','", name2 ,"')

ts_plot(dfagg, title = "Time Series of Combined Tweets (24 hr Intervals)", Xtitle = "Date and Time", Ytitle = "Frequency of Tweets", slider = TRUE, line.mode =  "lines+markers")

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  htmlwidgets::saveWidget(ts_plot(dfagg), paste0("~/Desktop/", name1 ,"-", name2 ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
} else {
  
  htmlwidgets::saveWidget(ts_plot(dfagg), paste0("~\\Desktop\\", name1 ,"-", name2 ,"-Tweet-Freq ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".html"))
  
}


