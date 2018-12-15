
library(tidyverse)
library(tidytext)

his <- read_csv("hisTweets.csv") %>%
  select(screen_name, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "his")

his %>% count(word1,word2) %>%
  arrange(desc(n)) %>%
  unite(bigram,word1,word2, sep = " ")
