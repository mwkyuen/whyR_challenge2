library(tidyverse)
library(feather)
library(tidytext)


user_word_groups <- read_feather("data/user_word_groups.feather")

# Sentiment Analysis by User Group
afinn <- user_word_groups %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(topic) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(lexicon = "afinn")

bing <- user_word_groups %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(value = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(topic) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(lexicon = "bing")

nrc <- user_word_groups %>%
  inner_join(get_sentiments("nrc")) %>%
  mutate(
    value = str_replace(sentiment, "anticipation|trust|joy|surprise", "positive"),
    value = str_replace(sentiment, "sadness|fear|anger|disgust", "negative"),
    value = ifelse(sentiment == "positive", 1, -1)
  ) %>%
  group_by(topic) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(lexicon = "nrc")

# Sentiment Analysis Plot
bind_rows(afinn, bing, nrc) %>%
  ggplot(aes(topic, value, fill = lexicon)) +
  geom_col(show.legend = F) +
  facet_wrap(~ lexicon, ncol = 1, scales = 'free_y') +
  labs(
    x = 'Time',
    y = 'Sentiment',
    title = 'Sentiment Analysis by User Group'
  )

ggsave("plot/sentiment_analysis_by_group.png", width = 12, height = 8)
