
library(tidytext)
library(tm)
library(stm)
library(furrr)

########## 1. Prepare data ################################

df <- read_csv("herTweets.csv") %>%
  select(screen_name, text)

#udmodel <- udpipe_download_model(language = "danish")

df <- df %>% 
  mutate(text = removeWords(text, tm::stopwords("english"))) %>%
  unnest_tokens(word,text) %>%
  rename(id = screen_name) %>%
  add_count(word) %>%
  filter(n > 1,word != "") %>%
  select(-n)

dfSparse <- df             %>%
  count(id, word)          %>%
  cast_sparse(id, word, n)

plan("default")
start_time_stm <- Sys.time()

nTopics <- seq(5,50,5)

many_models_stm <- data_frame(K = nTopics) %>%
  mutate(topic_model = future_map(K, ~stm(dfSparse, K = ., verbose = TRUE)))

end_time_stm <- Sys.time() # Time difference of 3.680019 hours

heldout <- make.heldout(dfSparse)

k_result <- many_models_stm %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfSparse),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, dfSparse),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound))) %>%
  mutate(mean_semantic_coherence = map(semantic_coherence,mean) %>% unlist(),
         mean_exclusivity = map(exclusivity,mean) %>% unlist())

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            Exclusivity           = map_dbl(exclusivity, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics")

excl_sem_plot <- k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  #filter(K %in% seq(2,16,2)) %>%
  unnest()                                         %>%
  mutate(K = as.factor(K))                         %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity") +
  scale_color_viridis_d()

excl_sem_plot

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = 8, fps = 0.5)

k_result %>% 
  ggplot(aes(x=mean_semantic_coherence, y = mean_exclusivity,
             label=K)) +
  geom_point(size=3) +
  geom_text_repel(size=5) +
  geom_smooth()
# ---------------------------------

# ---------------------------------
# SELECT STM MODEL

topic_model_stm <- k_result %>% 
  filter(K ==20)            %>% 
  pull(topic_model)         %>% 
  .[[1]]

topic_model_stm

# ---------------------------------
# EXPLORE STM MODEL

td_beta <- tidy(topic_model_stm)

top_terms <- td_beta  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(5, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_stm, matrix = "gamma",
                 document_names = rownames(dfSparse))

gamma_terms <- td_gamma              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot <- gamma_terms %>%
  top_n(15, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = gamma)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = -.1, vjust=0, size = 5, family = "Helvetica") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.2),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 15 STM topics",
       subtitle = "Tweets with 'her'") +
  scale_fill_gradient(low=brewer.pal(9,"Reds")[2],high=brewer.pal(9,"Reds")[9])

stm_plot

ggsave("herTopics.png")

