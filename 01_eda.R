library(tidyverse)
library(feather)
library(tidytext)
library(topicmodels)
library(wordcloud)





comments <- read_feather("data/comments.feather")

comments_tidy <- comments %>%
  select(by, text) %>%
  group_by(by) %>%
  summarize(text = str_c(text, collapse = " ")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Save
comments_tidy %>%
  write_feather("data/comments_tidy.feather")





articles <- read_feather("data/articles.feather")

articles_tidy <- articles %>%
  select(by, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)

# Save
articles_tidy %>%
  write_feather("data/articles_tidy.feather")

articles_tidy %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))



