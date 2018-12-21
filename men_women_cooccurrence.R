# MEN / WOMEN WORD CO-OCCURRENCE

library(tidyverse)
library(tidytext)
library(gridExtra)
library(widyr)
library(ggraph)
library(igraph)
library(tm)
library(wordcloud)

########## 1. Prepare data ################################

df <- read_csv("mwTweets.csv")

df %<>%
  select(screen_name, text)    %>%
  mutate(text = tolower(text)) %>%
  mutate(gender = case_when(str_detect(text,"women") &
                            str_detect(text," men")   ~ "both",
                            str_detect(text,"women")  ~ "women",
                            str_detect(text,"men")    ~ "men")) %>%
  unnest_tokens(word, text)    %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",]) %>%
  mutate(word = removeWords(word,c(stopwords(),"t.co","https","amp","'s","’s"))) %>%
  add_count(word)              %>%
  filter(n > 1,word != "")     %>%
  select(-n)

########## 1. CO-OCCURRENCE ########################################

word_pairs_men <- df      %>%
  filter(gender == "men") %>%
  pairwise_count(word, screen_name, sort = TRUE) %>%
  filter(item1 == "men")  %>% 
  top_n(20)

word_pairs_women <- df      %>%
  filter(gender == "women") %>%
  pairwise_count(word, screen_name, sort = TRUE)  %>%
  filter(item1 == "women")  %>% 
  top_n(20)

word_pairs_both <- df      %>%
  filter(gender == "both") %>%
  pairwise_count(word, screen_name, sort = TRUE) %>%
  filter(item1 == "men", item2 != "men" & item2 != "women")  %>% 
  top_n(20) %>%
  mutate(item1 = "both")
 
word_pairs <- rbind(word_pairs_men, word_pairs_women,word_pairs_both) %>%
  mutate(order = rev(row_number()), item1 = factor(item1, levels = c("men", "women","both")))

word_pairs %>% 
  ggplot(aes(x = order, y = n, fill = item1)) + 
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = word_pairs$order, 
                     labels = word_pairs$item2, 
                     expand = c(0,0)) + 
  facet_wrap(~item1, scales = "free") +
  scale_fill_manual(values = c("steelblue", "indianred","darkgreen")) + coord_flip() + labs(x = "words") +
  theme_minimal() +
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size=24, face="bold"),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("mw_cooccurrence.png")

# set.seed(611)
# 
# pairs_plot_men <- word_pairs_men %>%
#   filter(n > 200)                %>%
#   graph_from_data_frame()        %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#00B67A",show.legend=F) +
#   geom_node_point(size = 4) +
#   geom_node_text(aes(label = name), repel = TRUE,
#                  point.padding = unit(0.2, "lines")) +
#   theme_void()
# 
# pairs_plot_women <- word_pairs_women %>%
#   filter(n >= 200)                 %>%
#   graph_from_data_frame()        %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#FF3722",show.legend=F) +
#   geom_node_point(size = 4) +
#   geom_node_text(aes(label = name), repel = TRUE,
#                  point.padding = unit(0.2, "lines")) +
#   theme_void()
# 
# grid.arrange(pairs_plot_men, pairs_plot_women, ncol = 2)

########## 1. CORRELATION ########################################

cor_men <- df               %>%
  filter(gender == "men")   %>%
  group_by(word) %>%
  filter(n() >= 100)       %>%
  pairwise_cor(word, screen_name, sort = TRUE) %>% 
  filter(item1 == "men")    %>% 
  top_n(20)

cor_women <- df             %>% 
  filter(gender == "women") %>%
  group_by(word)            %>% 
  filter(n() >= 100)        %>%
  pairwise_cor(word,screen_name, sort = TRUE) %>% 
  filter(item1 == "women")  %>% 
  top_n(20)

cor_words <- rbind(cor_men, cor_women) %>%
  mutate(order = rev(row_number()), item1 = factor(item1, levels = c("men", "women")))

cor_words %>% 
  ggplot(aes(x = order, y = correlation, fill = item1)) + 
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = cor_words$order, 
                     labels = cor_words$item2, 
                     expand = c(0,0)) + 
  facet_wrap(~item1, scales = "free") +
  scale_fill_manual(values = c("steelblue", "indianred")) + coord_flip() + labs(x = "words") +
  theme_minimal() +
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size=24, face="bold"),
        #axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("mw_correlation.png")

########## 1. BIGRAMS MEN ########################################

men <- read_csv("mwTweets.csv") %>%
  select(screen_name, text)     %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")     %>%
  filter(word2 == "men")

menCount <- men      %>%
  count(word1,word2) %>%
  select(word1,n)    %>%
  arrange(desc(n))   %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",],by = c("word1" = "word")) 
  
wordcloud(words = menCount$word1, freq = menCount$n, min.freq = 30, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

menCountTop <- menCount            %>%
  filter(word1!="amp",word1!="ii") %>%
  mutate(row = rev(row_number()))  %>%
  top_n(20,n)

menCountTop %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = menCountTop$row,
    labels = menCountTop$word1,
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

ggsave("menCount.png")

########## 1. BIGRAMS WOMEN ########################################

women <- read_csv("mwTweets.csv") %>%
  select(screen_name, text)       %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")     %>%
  filter(word2 == "women")

womenCount <- women  %>%
  count(word1,word2) %>%
  select(word1,n)    %>%
  arrange(desc(n))   %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",],by = c("word1" = "word")) 

wordcloud(words = womenCount$word1, freq = womenCount$n, min.freq = 30, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Reds")[4:9])

womenCountTop <- womenCount        %>%
  filter(word1!="amp",word1!="ii") %>%
  mutate(row = rev(row_number()))  %>%
  top_n(20,n)

womenCountTop %>%
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE,width = .9) +
  coord_flip() +
  scale_x_continuous( 
    breaks = womenCountTop$row,
    labels = womenCountTop$word1,
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


