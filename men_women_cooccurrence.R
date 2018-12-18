# MEN / WOMEN WORD CO-OCCURRENCE

library(tidyverse)
library(tidytext)
library(gridExtra)
library(widyr)
library(ggraph)
library(igraph)
library(tm)

########## 1. Prepare data ################################

df <- read_csv("mwTweets.csv")

df %<>%
  select(screen_name, text) %>%
  mutate(text = tolower(text)) %>%
  mutate(gender = ifelse(str_detect(text,"women"),"women","men")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words[stop_words$lexicon=="SMART",])     %>%
  mutate(word = removeWords(word,c(stopwords(),"t.co","https","amp","'s","’s"))) %>%
  add_count(word)           %>%
  filter(n > 1,word != "")  %>%
  select(-n)

########## 1. CO-OCCURRENCE ########################################

word_pairs_men <- df %>%
  filter(gender == "men") %>%
  pairwise_count(word, screen_name, sort = TRUE) %>%
  filter(item1 == "men")

word_pairs_women <- df %>%
  filter(gender == "women") %>%
  pairwise_count(word, screen_name, sort = TRUE)  %>%
  filter(item1 == "women")

set.seed(611)

pairs_plot_men <- word_pairs_men %>%
  filter(n > 200)                %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#00B67A",show.legend=F) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

pairs_plot_women <- word_pairs_women %>%
  filter(n >= 200)                 %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#FF3722",show.legend=F) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

grid.arrange(pairs_plot_pos, pairs_plot_neg, ncol = 2)


########## 1. CORRELATION ########################################

cor_men <- df    %>%
  group_by(word) %>%
  #filter(n() >= 100) %>%
  pairwise_cor(word, screen_name, sort = TRUE) %>% 
  filter(item1 == "men") %>% 
  top_n(25)

cor_women <- df %>% 
  group_by(word) %>% 
  #filter(n() >= 100) %>%
  pairwise_cor(word,screen_name, sort = TRUE) %>% 
  filter(item1 == "women") %>% 
  top_n(25)

cor_words <- rbind(cor_men, cor_women) %>%
  mutate(order = rev(row_number()), item1 = factor(item1, levels = c("men", "women")))

cor_words %>% 
  ggplot(aes(x = order, y = correlation, fill = item1)) + 
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = cor_words$order, 
                     labels = cor_words$item2, 
                     expand = c(0,0)) + 
  facet_wrap(~item1, scales = "free") +
  scale_fill_manual(values = c("steelblue", "indianred")) + coord_flip() + labs(x = "words") + theme_minimal()


