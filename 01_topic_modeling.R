library(feather)
library(tidytext)
library(textdata)
library(tidyverse)

comments <- read_feather("data/comments_recent_tidy.feather")

comments_unnested <- comments_recent %>%
  select(-c(by, parent, time, type)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

afinn <- comments_unnested %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(lexicon = "afinn")

bing <- comments_unnested %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(value = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(id) %>%
  summarize(value = sum(value)) %>%
  mutate(lexicon = "bing")

nrc <- comments_unnested %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(
    value = str_replace(sentiment, "anticipation|trust|joy|surprise", "positive"),
    value = str_replace(sentiment, "sadness|fear|anger|disgust", "negative"),
    value = ifelse(sentiment == "positive", 1, -1)
  ) %>%
  group_by(id) %>%
  summarize(value = sum(value)) %>%
  mutate(lexicon = "nrc")

sentiments <- bind_rows(afinn, bing, nrc)

# Make implicit missing values explicit
sentiments <- tidyr::complete(sentiments, id, lexicon) %>%
  mutate(value = replace_na(value, 0))

sentiments %>%
  ggplot(aes(x = id, y = value, fill = lexicon)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~lexicon, ncol = 1) +
  coord_cartesian(ylim = c(-25, 25))


get_sentiments("nrc") %>%
  mutate(
    sentiment = str_replace(sentiment, "anticipation|trust|joy|surprise", "positive"),
    sentiment = str_replace(sentiment, "sadness|fear|anger|disgust", "negative")
  ) %>%
  count(sentiment)

# Each row represents one review
# Each column represents one word
# Each value represents the # of times the word appears in the review
document_term_matrix <- comments_unnested %>%
  group_by(id) %>%
  count(word) %>%
  cast_dtm(id, word, n)
document_term_matrix

library(topicmodels)

comments_lda <- LDA(document_term_matrix, k = 6, control = list(seed = 2020))
comments_lda

