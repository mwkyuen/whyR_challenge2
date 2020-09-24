library(tidyverse)
library(jsonlite)
library(textutils)
library(feather)

# Comments

## Read
comments <- fromJSON(readLines("data/comments_recent.json")) %>%
  as_tibble()

## Decode
comments <- comments %>%
  mutate(text = HTMLdecode(text)) %>%
  drop_na(text)

comments %>%
  select(-type) %>%
  write_feather("data/comments_decoded.feather")

## Load decoded
comments <- read_feather("data/comments_decoded.feather")

## Clean
comments <- comments %>%
  mutate(
    ### Remove digits
    text = str_replace_all(text, "[0-9]", " "),
    ### Remove html tags
    text = str_replace_all(text, "<.*</.*>", " "),
    text = str_replace_all(text, "<.*>", " "),
    ### Remove URLs
    text = str_replace_all(text, "(s?)(f|ht)tp(s?)://\\S+\\b", " ")
  )

## Save
write_feather(comments, "data/comments.feather")


# Articles
articles <- fromJSON(readLines("data/articles.json")) %>%
  as_tibble() %>%
  select(by, id, time, title) %>%
  unnest(cols = c(by, id, time, title))

## Decode
articles <- articles %>%
  mutate(title = HTMLdecode(title)) %>%
  drop_na(title)

articles %>%
  write_feather("data/articles_decoded.feather")

## Load decoded
articles <- read_feather("data/articles_decoded.feather")

## Clean
articles <- articles %>%
  mutate(
    ### Remove digits
    title = str_replace_all(title, "[0-9]", " "),
    ### Remove html tags
    title = str_replace_all(title, "<.*</.*>", " "),
    title = str_replace_all(title, "<.*>", " "),
    ### Remove URLs
    title = str_replace_all(title, "(s?)(f|ht)tp(s?)://\\S+\\b", " "),
    ### Remove "HN:"
    title = str_replace_all(title, "HN:", " ")
  )

## Save
articles %>%
  write_feather("data/articles.feather")


