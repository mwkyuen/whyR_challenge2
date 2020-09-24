library(tidyverse)
library(feather)
library(gridExtra)


comments <- read_feather("data/comments.feather")

comments_tidy <- read_feather("data/comments_tidy.feather")

user_groups <- read_feather("data/user_groups.feather")

# Grouped comments
comments_groups <- comments %>%
  inner_join(user_groups, by = "by") %>%
  select(by, parent, text, topic)

# Proportion of comments by topic
dev.off()
comments_groups %>%
  group_by(topic) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n), 5)) %>%
  grid.table(row = NULL)

# Proportion of users by topic
dev.off()
user_groups %>%
  group_by(topic) %>%
  summarize(n = n()) %>%
  mutate(prop = round(n / sum(n), 5)) %>%
  grid.table(row = NULL)

# Proportion of words by topic
comments_tidy_group <- comments_tidy %>%
  inner_join(user_groups, by = "by") %>%
  select(by, word, topic)

dev.off()
comments_tidy_group %>%
  count(topic) %>%
  mutate(proportion = round(n / nrow(comments_tidy_group), 5)) %>%
  grid.table(row = NULL)




