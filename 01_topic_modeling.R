library(feather)
library(tidytext)
library(textdata)
library(tidyverse)
library(topicmodels)
library(purrr)

comments <- read_feather("data/comments_recent_tidy.feather")

comments_unnested <- comments %>%
  select(-c(id, parent, time, type)) %>%
  group_by(by) %>%
  summarize(text = str_c(text, collapse = " ")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

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

tibble(
  index = 2:8,
  alpha = map(2:8, ~ read_rds(paste0("model/comments_lda_", .))@alpha) %>%
    unlist()
) %>%
  ggplot(aes(x = index, y = alpha)) +
  geom_line()

comments_lda <- read_rds("model/comments_lda_7")

comments_topic <- tidy(comments_lda, matrix = "beta")

top_terms <- comments_topic %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  xlab('Word')





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
