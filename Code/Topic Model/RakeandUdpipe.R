
# Install pacman in case you don’t have pacman already installed. 
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")

# Step 1 – add the required libraries and the data unless you have it already loaded
# Install and load the required packages. 
pacman::p_load(dplyr, ggplot2, stringr, udpipe, lattice)

# load the data unless you already have it loaded 
# remember to use the stringsAsFactors = F argument, otherwise errors galore.
# The file being used is “Coronavirus Subreddit March 20 to Mar 31 2020.csv”

SearchWord <- read.csv(file.choose(), stringsAsFactors = F)

head(SearchWord)
names(SearchWord)

# udpipe needs a model file loaded. This file can be downloaded and needs to 
# be placed in the working directory for it to loaded, otherwise full path is 
# needed. This is an important step.  

# MAKE SURE YOUR WORKING DIRECTORY IS SET TO WHERE THIS FILE IS LOCATED. 

udmodel_english <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

# Step 2 – count the number of total headlines by date and plot the results to examine
SearchWord %>% group_by(created_at) %>% count() %>% arrange(desc(n))

SearchWord %>% group_by(created_at) %>% count() %>% ggplot() + geom_line(aes(created_at,n, group = 1))

                                                               
s <- udpipe_annotate(udmodel_english, SearchWord$text)
x <- data.frame(s)

# Step 5 – extract and display frequencies for universal parts of speech (upos) in text
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

# Step 5 –extract and display most occurring nouns in the headlines
## NOUNS – change the number from 25 to lower or higher as applicable.

stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 25), col = "cadetblue", main = "Most occurring nouns", xlab = "Freq")

# Step 6 –extract and display most occurring adjectives in the headlines

## ADJECTIVES - change the number from 25 to lower or higher as applicable.

stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 25), col = "purple", main = "Most occurring adjectives", xlab = "Freq")

# Step 7 –extract and display most occurring verbs in the headlines

## VERBS - change the number from 25 to lower or higher as applicable.

stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 25), col = "gold", main = "Most occurring Verbs", xlab = "Freq")

# Step 8 – Use RAKE (Rapid Automatic Keyword Extraction algorithm) to                            
# determine key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.
## RAKE - Adjust the frequency and the number of results by changing 3 and 25 ## appropriate numbers for your dataset.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", relevant = x$upos %in% c("NOUN", "ADJ"))

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 25), col = "red", main = "Keywords identified by RAKE", xlab = "Rake")

# Step 9 – In English (and in many other languages a phrase can be formed simply with a # noun and a verb (e.g., cat meows) This may be useful for understanding context of a       
# sentence or a review or headlines especially if they are clickbait like. This step is to just # extract top phrases that are keyword-topics.
## display by plot a sequence of POS tags (noun phrases / verb phrases)
## Adjust the frequency and the number of results by changing 3 and 25 
## to appropriate numbers for your dataset. 
## You can also change the ngram levels to higher than 2 (like 3 or 4) 
## to get lengths of 3 word or 4 word combinations.

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), pattern = "(A|N)*N(P+D*(A|N)*N)*", is_regex = TRUE, detailed = FALSE)

stats <- subset(stats, ngram > 1 & freq > 3)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ freq, data = head(stats, 25), col = "magenta", main = "Keywords - simple noun phrases", xlab = "Frequency")

# Step 10 –it would be helpful to explore the words that appear next to each other. We can 
# do this with just nouns and adjectives to explore # the patterns to get focus topic areas. # Adjust the ngram max levels if needed. It is set to 4 to indicate that we want 
# co-occurrences within 3 words of each other.

## Collocation identification – basically words following one another)
stats <- keywords_collocation(x = x, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), ngram_max = 4)

## How frequently do words occur in the same sentence (nouns and adjectives)
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"))
## Co-occurrences: How frequent do words follow one another even if we would ## skip 2 words in between. You can adjust this if you need to.

stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)

head(stats)

# Step 11 – it may be helpful to explore this visually instead of inspecting it in a table. To 
# do this you will need the igraph library and the ggraph library. Load these with pacman.
# adjust the number you would like displayed. It is now set to 25.

pacman::p_load(igraph, ggraph)


wordnetwork <- head(stats, 40)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") + geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "red") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Co-occurrences within 3 words distance", subtitle = "Nouns & Adjectives")

