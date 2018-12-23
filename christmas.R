# christmas tweets
# date range:   "2018-12-14 13:40:43 UTC" "2018-12-22 03:32:27 UTC"
# n:             71,714
# n about trump: 1,713 (2.38 %)

library(tidyverse)
library(tidytext)
library(udpipe)
library(wordcloud)

xmas <- read_csv("xmasTweets.csv") %>%
  select(X1,screen_name,text)         %>% 
  mutate(text = tolower(text))     %>%
  mutate(text = str_remove(text,"all i want for christmas is"))

# NOUNS AFTER

xmas <- xmas                %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",]) %>%
  filter(!word %in% c("t.co","amp","https","christmas"))

#udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model("english-ud-2.0-170801.udpipe")

include <- udpipe(x = xmas$text,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos =="NOUN") %>%
  select(token) 

xmas <- xmas %>%
  filter(word %in% include$token)

xmasCount <- xmas          %>%
  count(word)              %>%
  arrange(desc(n))         %>%
  mutate(row = rev(row_number()))
#unite(bigram,word1,word2, sep = " ")

wordcloud(words = xmasCount$word, freq = xmasCount$n, min.freq = 500, random.order=FALSE, rot.per=0.35, 
          colors=c(brewer.pal(9,"Greens")[5:9],brewer.pal(9,"Reds")[6:8]))

xmasCount     %>%
  top_n(20,n) %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = xmasCount$row,
    labels = xmasCount$word,
    expand = c(0,0)) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low = brewer.pal(9,"Greens")[5], high = brewer.pal(9,"Reds")[9])

# HASHTAGS

remove_reg <- "&amp;|&lt;|&gt;"

tags <- xmas %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(hashtag, text, token = "tweets") %>%
  filter(!hashtag %in% stop_words$word,
         !hashtag %in% str_remove_all(stop_words$word, "'")) %>%
  filter(str_detect(hashtag, "^#")) %>%
  mutate(hashtag = str_remove(hashtag,"#"))

tags <- tags %>%
  group_by(hashtag) %>%
  count() %>%
  arrange(desc(n))

wordcloud(words = tags$hashtag, freq = tags$n, min.freq = 10, random.order=FALSE, rot.per=0.35, 
          colors=c(brewer.pal(9,"Greens")[5:9],brewer.pal(9,"Reds")[6:8]))

# TRUMP

xmas %>% 
  filter(str_detect(text,"trump"))
         
  group_by(text) %>% 
    
    summarise(n=n()) %>% top_n(20)