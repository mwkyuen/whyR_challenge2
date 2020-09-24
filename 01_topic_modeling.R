library(feather)
library(tidytext)
library(textdata)
library(tidyverse)
library(topicmodels)
library(purrr)
library(wordcloud)
library(jsonlite)

comments <- read_feather("data/comments_recent_tidy.feather")

comments_unnested <- comments %>%
  select(-c(id, parent, time, type)) %>%
  group_by(by) %>%
  summarize(text = str_c(text, collapse = " ")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Drop numeric values
comments_unnested <- comments_unnested %>%
  mutate(as_num_val = as.numeric(word)) %>%
  filter(is.na(as_num_val)) %>%
  select(-as_num_val)

# # Each row represents one review
# # Each column represents one word
# # Each value represents the # of times the word appears in the review
# document_term_matrix <- comments_unnested %>%
#   group_by(by) %>%
#   count(word) %>%
#   cast_dtm(by, word, n)
# document_term_matrix
#
# get_alpha <- function(x) {
#   comments_lda <- LDA(document_term_matrix, k = x, control = list(seed = 2020))
#   write_rds(comments_lda, paste0("model/comments_lda_", x))
#   alpha <- comments_lda@alpha
#   return(alpha)
# }
# k <- 2:10
# alpha <- lapply(k, get_alpha) %>% unlist()

# Select best model based on the elbow method
tibble(
  index = 2:8,
  alpha = map(2:8, ~ read_rds(paste0("model/comments_lda_", .))@alpha) %>%
    unlist()
) %>%
  ggplot(aes(x = index, y = alpha)) +
  geom_line(aes(color = "color"), show.legend = FALSE) +
  labs(
    title = "How alpha Changes as k Increases",
    x = "k"
  )
# alpha - parameter of the Dirichlet distribution for topics over documents.
ggsave("plot/user_groups_elbow_plot.png", width = 7, height = 3.5)

comments_lda <- read_rds("model/comments_lda_7")

# Word - Topic
comments_beta <- tidy(comments_lda, matrix = "beta")

top_beta <- comments_beta %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_beta %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  xlab('Word')

ggsave("plot/important_words_by_group.png", width = 10, height = 7.5)

# User - Topic
comments_gamma <- tidy(comments_lda, matrix = "gamma")

top_gamma <- comments_gamma %>%
  group_by(topic) %>%
  top_n(15, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

top_gamma %>%
  mutate(document = reorder(document, gamma)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  xlab('Word')

# User Groups
group_users <- comments_gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  rename(by = document)

comments_groups <- comments_unnested %>%
  inner_join(group_users, by = "by") %>%
  select(-gamma)

# Sentiment Analysis by User Group
afinn <- comments_groups %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(topic) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(lexicon = "afinn")

bing <- comments_groups %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(value = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(topic) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(lexicon = "bing")

nrc <- comments_groups %>%
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

ggsave("plot/sentiment_analysis_by_group.png", width = 7, height = 3.5)

# Grouped comments
comments_groups <- comments %>%
  inner_join(group_users, by = "by") %>%
  select(by, parent, text, topic)

# Proportion of comments by topic
comments_groups %>%
  group_by(topic) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  gridExtra::grid.table(row = NULL)

# Proportion of users by topic
group_users %>%
  group_by(topic) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  gridExtra::grid.table(row = NULL)

# Proportion of words by topic
comments_unnested_group <- comments_unnested %>%
  inner_join(group_users, by = "by") %>%
  select(by, word, topic)
comments_unnested_group %>%
  count(topic) %>%
  mutate(proportion = n / nrow(comments_unnested_group)) %>%
  gridExtra::grid.table(row = NULL)


articles <- fromJSON(readLines('data/articles.json')) %>%
  as_tibble() %>%
  unnest(c(by, id, title)) %>%
  rename(parent = id) %>%
  select(by, parent, title)

# Top words in title by topic
title_words <- comments_groups %>%
  inner_join(
    articles %>%
      select(-by),
    by = "parent"
  ) %>%
  select(topic, parent, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  filter(word != "hn")

# Drop numeric values
title_words <- title_words %>%
  mutate(as_num_val = as.numeric(word)) %>%
  filter(is.na(as_num_val)) %>%
  select(-as_num_val)

title_words %>%
  group_by(topic, word) %>%
  count() %>%
  ungroup() %>%
  group_by(topic) %>%
  arrange(desc(n)) %>%
  slice(1:15) %>%
  mutate(word = reorder(word, n)) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  xlab('Word')

ggsave("plot/top_title_words_by_group.png", width = 10, height = 7.5)

title_words %>%
  filter(topic == 1) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

title_words %>%
  filter(topic == 2) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

title_words %>%
  filter(topic == 3) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

title_words %>%
  filter(topic == 4) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

title_words %>%
  filter(topic == 5) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

title_words %>%
  filter(topic == 6) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

title_words %>%
  filter(topic == 7) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Top words in comments by topic
comments_unnested_group %>%
  group_by(topic, word) %>%
  count() %>%
  ungroup() %>%
  group_by(topic) %>%
  arrange(desc(n)) %>%
  slice(1:15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  xlab('Word')

ggsave("plot/top_comment_words_by_group.png", width = 10, height = 7.5)

dev.off()
comments_unnested_group %>%
  filter(topic == 1) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

dev.off()
comments_unnested_group %>%
  filter(topic == 2) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

dev.off()
comments_unnested_group %>%
  filter(topic == 3) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

dev.off()
comments_unnested_group %>%
  filter(topic == 4) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

dev.off()
comments_unnested_group %>%
  filter(topic == 5) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

dev.off()
comments_unnested_group %>%
  filter(topic == 6) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

dev.off()
comments_unnested_group %>%
  filter(topic == 7) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

