#Hashtags


library(tidyverse)
library(tidytext)
library(udpipe)
library(wordcloud)

df <- read_csv("mwTweets.csv") %>%
  select(X1,screen_name,text)  %>% 
  mutate(text = tolower(text))

remove_reg <- "&amp;|&lt;|&gt;"

df <- df %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(hashtag, text, token = "tweets") %>%
  filter(!hashtag %in% stop_words$word,
         !hashtag %in% str_remove_all(stop_words$word, "'")) %>%
  filter(str_detect(hashtag, "^#")) %>%
  mutate(hashtag = str_remove(hashtag,"#")) %>%
  filter(str_detect(hashtag,"men|women")) %>%
  filter(hashtag != "men", hashtag != "women",hashtag != "mens", hashtag != "womens", !str_detect(hashtag,"ment"))

tags <- df %>%
  group_by(hashtag) %>%
  count() %>%
  arrange(desc(n))

tags <- tags %>%
  mutate(gender = case_when(str_detect(hashtag,"women") ~ "f",
                            !str_detect(hashtag,"women") ~ "m"))

menTags <- tags %>%
  filter(gender == "m")

womenTags <- tags %>%
  filter(gender == "f")

wordcloud(words = menTags$hashtag, freq = menTags$n, min.freq = 3, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

wordcloud(words = womenTags$hashtag, freq = womenTags$n, min.freq = 3, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Reds")[4:9])
