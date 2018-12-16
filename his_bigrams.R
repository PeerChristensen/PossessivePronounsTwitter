
library(tidyverse)
library(tidytext)
library(udpipe)
library(wordcloud)

his <- read_csv("hisTweets.csv") %>%
  select(screen_name, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "his")

udmodel <- udpipe_download_model(language = "english")

include <- udpipe(x = his$word2,
                  object = udmodel)

include <- include %>%
  select(token,upos) %>%
  filter(upos =="NOUN") %>%
  select(token) 

his <- his %>%
  filter(word2 %in% include$token)

hisCount <- his %>%
  count(word1,word2) %>%
  arrange(desc(n)) %>%
  select(word2,n)
  #unite(bigram,word1,word2, sep = " ")

wordcloud(words = hisCount$word2, freq = hisCount$n, min.freq = 50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

