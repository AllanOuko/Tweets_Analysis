##Load the required packages

library(rtweet) # Twitter API medium
library(ggplot2) # for plotting
library(dplyr) # for piping operator and handling tibbles
library(tidytext) # text mining libraries
library(textdata)

#Search for the last 10000 tweets
rt = search_tweets("Ukraine+Russia,lang:en",n = 10000, include_rts = F)


#clear all links
rt$updated_text = gsub("https.*","",rt$text)
rt$updated_text = gsub("http.*","",rt$updated_text)


#convert all texts to lowercase and remove punctuation 
rt2 <- rt %>%
  dplyr::select(updated_text) %>%
  unnest_tokens(word, updated_text)


#removing stop words
data("stop_words")


#nrow(rt2)

rt2 = anti_join(rt2,stop_words)


# attach each word to its sentiment using the dictionary "bing"
rt3 = rt2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()


# plot the negatives and positives
rt3 %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing Ukraine and Russia", y = NULL, x = NULL) +
  coord_flip() +
  theme_classic()


##Analyze the sentiments of US and Russia's President

biden_tweet = get_timeline("POTUS", n = 10000, include_rts = F)
putin_tweet = get_timeline("KremlinRussia_E", n = 10000, include_rts = F)


#clear all links
biden_tweet$updated_text1 = gsub("https.*","",biden_tweet$text)
biden_tweet$updated_text1 = gsub("http.*","",biden_tweet$updated_text1)


putin_tweet$updated_text2 = gsub("https.*","",putin_tweet$text)
putin_tweet$updated_text2 = gsub("http.*","",putin_tweet$updated_text2)

#convert all texts to lowercase and remove punctuations 
biden_tweet2 <- biden_tweet %>%
  dplyr::select(updated_text1) %>%
  unnest_tokens(word, updated_text1)

putin_tweet2 <- putin_tweet%>%
  dplyr::select(updated_text2) %>%
  unnest_tokens(word, updated_text2)


#removing stop words
data("stop_words")

biden_tweet2 = anti_join(biden_tweet2,stop_words)
putin_tweet2 = anti_join(putin_tweet2,stop_words)

# attach each word to its sentiment using the dictionary "bing"
biden_tweet3 = biden_tweet2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

putin_tweet3 = putin_tweet2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()


# plot the negatives and positives
biden_tweet3 %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Biden's Tweet Sentiments", y = NULL, x = NULL) +
  coord_flip() +
  theme_classic()


putin_tweet3 %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Putin's Tweet Sentiments", y = NULL, x = NULL) +
  coord_flip() +
  theme_classic()


