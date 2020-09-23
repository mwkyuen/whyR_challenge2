library(tidyverse)
library(jsonlite)
library(textutils)
library(feather)

comments_recent <- readLines("data/comments_recent.json") %>%
  fromJSON() %>%
  as_tibble()

comments_recent_tidy <- comments_recent %>%
  mutate(text = HTMLdecode(text)) %>%
  drop_na(text)

comments_recent_tidy %>%
  drop_na(text) %>%
  write_feather("data/comments_recent_tidy.feather")


