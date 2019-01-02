
library(tidyverse)
library(tidytext)
library(udpipe)
library(wordcloud)

his <- read_csv("hisTweets.csv") %>%
  select(text)                   %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")     %>%
  filter(word1 == "his")

#udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model("english-ud-2.0-170801.udpipe")

include <- udpipe(x = his$word2,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN") %>%
  select(token) 

his <- his %>%
  filter(word2 %in% include$token)

hisCount <- his         %>%
  count(word1,word2)    %>%
  arrange(desc(n))      %>%
  select(word2,n)       %>%
  mutate(row = rev(row_number()))
  #unite(bigram,word1,word2, sep = " ")

wordcloud(words = hisCount$word2, freq = hisCount$n, min.freq = 50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

hisCount      %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = hisCount$row,
    labels = hisCount$word2) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Blues")[2],high=brewer.pal(9,"Blues")[9])


