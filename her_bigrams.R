library(tidyverse)
library(tidytext)
library(cldr)

her <- read_csv("herTweets.csv") %>%
  select(screen_name, text) %>% 
  mutate(language = detectLanguage(text)$detectedLanguage) %>%
  filter(language=="ENGLISH")
  
her <- her %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "her")

her %>% count(word1,word2) %>%
  arrange(desc(n)) %>%
  unite(bigram,word1,word2, sep = " ")
