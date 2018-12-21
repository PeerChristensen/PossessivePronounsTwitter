# Tri- and Quadrograms

library(tidyverse)
library(tidytext)
library(udpipe)
library(wordcloud)

heTweets <- read_csv("heTweets.csv") 

# TRIGRAMS

he <- heTweets                %>%
  select(screen_name, text) %>% 
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace(text, "he's", "he is")) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(word1 == "he", word2 == "is")

#udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model("english-ud-2.0-170801.udpipe")

include <- udpipe(x = he$word3,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN" | upos == "ADJ") %>%
  select(token) 

he <- he %>%
  filter(word3 %in% include$token)

heCount <- he         %>%
  count(word1,word2,word3)    %>%
  arrange(desc(n))      %>%
  select(word3,n)       %>%
  mutate(row = rev(row_number()))
#unite(bigram,word1,word2, sep = " ")

wordcloud(words = heCount$word3, freq = heCount$n, min.freq = 15, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

heCount      %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = heCount$row,
    labels = heCount$word3,
    expand = c(0,0)) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Blues")[2],high=brewer.pal(9,"Blues")[9])

ggsave("heIsCount.png")

### she

sheTweets <- read_csv("sheTweets.csv") 

she <- sheTweets               %>%
  select(screen_name, text)    %>% 
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace(text, "she's", "she is")) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(word1 == "she", word2 == "is")

#udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model("english-ud-2.0-170801.udpipe")

include <- udpipe(x = she$word3,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN" | upos == "ADJ") %>%
  select(token) 

she <- she %>%
  filter(word3 %in% include$token)

sheCount <- she            %>%
  count(word1,word2,word3) %>%
  arrange(desc(n))         %>%
  select(word3,n)          %>%
  mutate(row = rev(row_number()))
#unite(bigram,word1,word2, sep = " ")

wordcloud(words = sheCount$word3, freq = sheCount$n, min.freq = 15, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Reds")[4:9])

sheCount      %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = sheCount$row,
    labels = sheCount$word3,
    expand = c(0,0)) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Reds")[2],high=brewer.pal(9,"Reds")[9])

ggsave("sheIsCount.png")

# QUADROGRAMS

he <- heTweets                %>%
  select(screen_name, text) %>% 
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace(text, "he's", "he is")) %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n = 4) %>%
  separate(quadrogram, c("word1", "word2","word3", "word4"), sep = " ") %>%
  filter(word1 == "he", word2 == "is", word3 == "a" | word3 == "an")

#udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model("english-ud-2.0-170801.udpipe")

include <- udpipe(x = he$word4,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN") %>%
  select(token) 

he <- he %>%
  filter(word4 %in% include$token)

heCount <- he         %>%
  count(word1,word2,word3,word4) %>%
  arrange(desc(n))      %>%
  select(word4,n)       %>%
  mutate(row = rev(row_number()))
#unite(bigram,word1,word2, sep = " ")

wordcloud(words = heCount$word4, freq = heCount$n, min.freq = 5, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

heCount      %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = heCount$row,
    labels = heCount$word4,
    expand = c(0,0)) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Blues")[2],high=brewer.pal(9,"Blues")[9])

ggsave("heIsACount.png")

### she

she <- sheTweets               %>%
  select(screen_name, text)    %>% 
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace(text, "she's", "she is")) %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n = 4) %>%
  separate(quadrogram, c("word1", "word2","word3", "word4"), sep = " ") %>%
  filter(word1 == "she", word2 == "is", word3 == "a" | word3 == "an")

#udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model("english-ud-2.0-170801.udpipe")

include <- udpipe(x = she$word4,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN") %>%
  select(token) 

she <- she %>%
  filter(word4 %in% include$token)

sheCount <- she         %>%
  count(word1,word2,word3,word4)    %>%
  arrange(desc(n))      %>%
  select(word4,n)       %>%
  mutate(row = rev(row_number()))
#unite(bigram,word1,word2, sep = " ")

wordcloud(words = sheCount$word4, freq = sheCount$n, min.freq = 5, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Reds")[4:9])

sheCount      %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = sheCount$row,
    labels = sheCount$word4) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Reds")[2],high=brewer.pal(9,"Reds")[9])

ggsave("sheIsACount.png")

