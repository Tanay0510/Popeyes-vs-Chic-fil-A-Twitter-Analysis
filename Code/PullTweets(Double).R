# Include Libraries

library(rtweet)

# Twitter API Authentication

app.name <- "#########"
consumer.key <- "################"
consumer.secret <- "###################"
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
# Check for the operating system (Windows or Mac)

Sys.info()["sysname"]

if(Sys.info()["sysname"]  == "Darwin") {
  
  save_as_csv(nk, paste0("~/Desktop/", name1 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  save_as_csv(nk, paste0("~/Desktop/", name2 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
} else {
  
  save_as_csv(nk, paste0("~\\Desktop\\", name1 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
  save_as_csv(nk, paste0("~\\Desktop\\", name2 ,"-Tweets ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"))
  
}
