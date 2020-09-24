library(tidyverse)
library(feather)
library(tidytext)
library(topicmodels)

comments <- read_feather("data/comments.feather")

comments_tidy <- read_feather("data/comments_tidy.feather")

# Fit LDA models
# document_term_matrix <- comments_tidy %>%
#   group_by(by) %>%
#   count(word) %>%
#   cast_dtm(by, word, n)
# document_term_matrix
# fit_models <- function(x) {
#   LDA(document_term_matrix, k = x, control = list(seed = 2020)) %>%
#     write_rds(paste0("model/comments_lda_", x))
# }
# lapply(2:10, fit_models)

# Best model
comments_lda <- read_rds("model/comments_lda_8")

# Word - Topic
comments_beta <- tidy(comments_lda, matrix = "beta")

top_beta <- comments_beta %>%
  group_by(topic) %>%
  top_n(25, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_beta %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

ggsave("plot/topic_modeling_comments_by_group.png", width = 12, height = 8)

# User - Topic
comments_gamma <- tidy(comments_lda, matrix = "gamma")

top_gamma <- comments_gamma %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

top_gamma %>%
  mutate(document = reorder_within(document, gamma, topic)) %>%
  group_by(topic, document) %>%
  arrange(desc(gamma)) %>%
  ungroup() %>%
  ggplot(aes(document, gamma, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 documents in each LDA topic",
       x = NULL, y = expression(gamma)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

ggsave("plot/topic_modeling_users_by_group.png", width = 12, height = 8)

# User Groups
user_groups <- comments_gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  rename(by = document)

user_groups %>%
  write_feather("data/user_groups.feather")

# User Word Groups
user_word_groups <- comments_tidy %>%
  inner_join(user_groups, by = "by") %>%
  select(-gamma)

user_word_groups %>%
  write_feather("data/user_word_groups.feather")











