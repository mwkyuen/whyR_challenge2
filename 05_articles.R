library(tidyverse)
library(feather)
library(tidytext)


articles <- read_feather("data/articles.feather")

user_groups <- read_feather("data/user_groups.feather")

comments <- read_feather("data/comments.feather")

user_word_groups <- read_feather("data/user_word_groups.feather")

# Top words in title by topic
title_words <- comments %>%
  inner_join(user_groups, by = "by") %>%
  select(-c(id, gamma)) %>%
  rename(id = parent) %>%
  inner_join(articles, by = "id") %>%
  select(topic, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)

title_words %>%
  group_by(topic, word) %>%
  count() %>%
  ungroup() %>%
  group_by(topic) %>%
  arrange(desc(n)) %>%
  slice(1:25) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, topic)) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 25 Title Keywords in Each Group",
       x = NULL, y = expression(gamma)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

ggsave("plot/articles_top_title_keywords_by_group.png", width = 12, height = 8)

# Top words in comments by topic
user_word_groups %>%
  group_by(topic, word) %>%
  count() %>%
  ungroup() %>%
  group_by(topic) %>%
  arrange(desc(n)) %>%
  slice(1:25) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, topic)) %>%
  group_by(topic, word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 25 Comment Keywords in Each Group",
       x = NULL, y = expression(gamma)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

ggsave("plot/articles_top_comment_keywords_by_group.png", width = 12, height = 8)
