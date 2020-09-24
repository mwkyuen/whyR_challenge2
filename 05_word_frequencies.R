library(tidyverse)
library(feather)
library(tidytext)
library(scales)


user_word_groups <- read_feather("data/user_word_groups.feather")

freq <- user_word_groups %>%
  count(topic, word, sort = T) %>%
  group_by(topic) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = topic, values_from = proportion)

freq %>%
  ggplot(aes(x = `4`, y = `8`)) +
  geom_abline(color = 'gray40', lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = T) +
  scale_x_log10(labels = percent) +
  scale_y_log10(labels = percent) +
  labs(
    x = 'Group 4',
    y = 'Group 8',
    title = 'Word Frequencies of Group 4 and Group 8 Comments'
  )

ggsave("plot/word_frequencies_group.png", width = 12, height = 8)
