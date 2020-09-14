
# Step 1 – add the required libraries and the data unless you have it already loaded
# Install and load the required packages. 

pacman::p_load(tidyr, dplyr, stringr, data.table, sentimentr, ggplot2, text2vec, tm, ggrepel)


SearchWord = read.csv(file.choose(), stringsAsFactors = F)


# create a rowid for the reviews
review_df <- SearchWord %>% mutate(id = row_number())

# examine the structure 
str(SearchWord)


nrow(lexicon::hash_sentiment_jockers_rinker) 

# words to replace – Add word as you require  

replace_in_lexicon <- tribble(
  ~x, ~y,
  "word1", 0,       
  "word2", 0,      
  "word3", 0,          
  "word4", 0, 
)

# create a new lexicon with modified sentiment
review_lexicon <- lexicon::hash_sentiment_jockers_rinker %>%
  filter(!x %in% replace_in_lexicon$x) %>%
  bind_rows(replace_in_lexicon) %>%
  setDT() %>%
  setkey("x")

sent_df <- review_df %>%
  get_sentences() %>%
  sentiment_by(by = c('text','id'),  polarity_dt = review_lexicon)


tokens <- space_tokenizer(SearchWord$text %>% tolower() %>% removePunctuation())

# Create vocabulary. Terms will be unigrams (simple words). 

it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)


# prune (remove) words that appear less than 3 times
vocab <- prune_vocabulary(vocab, term_count_min = 3L)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)

# use skip gram window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)


# fit the model. It can take several minutes based on how much data you have
glove = GloVe$new(rank = 100, x_max = 5)
glove$fit_transform(tcm, n_iter = 60)

# if the above code gives you an error, try replacing it with commented lines # below instead
# glove = GloVe$new(rank = 100, x_max = 5)
# glove$fit_transform(tcm, n_iter = 20)

# get the processed word vector
word_vectors = glove$components     

# Step 5 – check which words have contextual similarity, expand to 20 if there are too 
# many irrelevant words 
# check for nintendo first

berniefirst <- word_vectors["wordToCheckFor", drop = F] 

# Step 6 – implement a quick t-SNE to visualize reviews by similarity of words 
# load packages

pacman::p_load(tm, Rtsne, tibble, tidytext, scales)

#create vector of words to keep, before applying tsne (remove stop words)
keep_words <- setdiff(colnames(word_vectors), stopwords())

# keep words in vector
word_vec <- word_vectors[, keep_words]

# prepare data frame to train
train_df <- data.frame(t(word_vec)) %>% rownames_to_column("word")

# train tsne for visualization

# Use the max_iter as required. Check when the error is not changing alot and set the max_iter to a point where you observe a saturation 
tsne <- Rtsne(train_df[,-1], dims = 2, perplexity = 42, verbose=TRUE, max_iter = 1000)

#  t-Distributed Stochastic Neighbor Embedding (t-SNE)
#  t-SNE maps high dimensional data such as word embedding into a lower dimension      
#  in such that the distance between two words roughly describe the similarity.                                          
#  Additionally, t-SNE begins to create naturally forming clusters.

# Interpretation of a t-SNE is straightforward 
# Similar objects (or words) appear nearby each other in the plot and
# dissimilar objects appear far away from each other.


# Step 7 – plot the t-SNE and examine it
# create plot

colors = rainbow(length(unique(train_df$word)))
names(colors) = unique(train_df$word)

plot_df <- data.frame(tsne$Y) %>% mutate(
  word = train_df$word,
  col = colors[train_df$word]
) %>% left_join(vocab, by = c("word" = "term")) %>%
  filter(doc_count >= 100)

ggplot(plot_df, aes(X1, X2)) +
  geom_text(aes(X1, X2, label = word, color = col), size = 3) +
  xlab("") + ylab("") +
  theme(legend.position = "none") 

# Step 8 – calculate word level sentiment and overlay these on the t-SNE
# calculate word-level sentiment

word_sent <- sent_df %>%
  select(id,text, ave_sentiment) %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  summarise(
    count = n(),
    avg_sentiment = mean(ave_sentiment),
    sum_sentiment = sum(ave_sentiment),
    sd_sentiment = sd(ave_sentiment)
  ) %>%
  # remove stop words
  anti_join(stop_words, by = "word") 
# filter to words that appear at least 5 times
pd_sent <- plot_df %>%
  left_join(word_sent, by = "word") %>%
  drop_na() %>%
  filter(count >= 500)

# Step 9 – Plot the results

ggplot(pd_sent, aes(X1, X2)) +
  geom_point(aes(X1, X2, size = count, alpha = .1, color = avg_sentiment)) +
  geom_text(aes(X1, X2, label = word), size = 2) +
  scale_colour_gradient2(low = muted("red"), mid = "white",
                         high = muted("blue"), midpoint = 0) +
  scale_size(range = c(5, 20)) +
  xlab("") + ylab("") +
  ggtitle("2-dimensional t-SNE Mapping of Word Vectors") +
  guides(color = guide_legend(title="Avg. Sentiment"), size = guide_legend(title = "Frequency"), alpha = NULL) +
  scale_alpha(range = c(1, 1), guide = "none")



