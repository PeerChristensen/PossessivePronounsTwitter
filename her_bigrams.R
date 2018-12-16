library(tidyverse)
library(tidytext)
library(cldr)
library(udpipe)

her <- read_csv("herTweets.csv") %>%
  select(screen_name, text) %>% 
  mutate(language = detectLanguage(text)$detectedLanguage) %>%
  filter(language=="ENGLISH")
  
her <- her %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "her")

#udmodel <- udpipe_download_model(language = "english")

include <- udpipe(x = her$word2,
             object = udmodel)

include <- include %>%
  select(token,upos) %>%
  filter(upos =="NOUN") %>%
  select(token) 

her <- her %>%
  filter(word2 %in% include$token)

herCount <- her %>%
  count(word1,word2) %>%
  arrange(desc(n)) %>%
  select(word2,n) %>%
  mutate(row = rev(row_number()))

#unite(bigram,word1,word2, sep = " ")

wordcloud(words = herCount$word2, freq = herCount$n, min.freq = 50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Reds")[4:9])

herCount %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width=.9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = herCount$row,
    labels = herCount$word2) +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Reds")[2],high=brewer.pal(9,"Reds")[9])


  
