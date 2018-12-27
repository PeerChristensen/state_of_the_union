#heatmap2

library(tidytext)
library(tidyverse)
library(magrittr)
library(furrr)
library(beepr)
library(stm)
library(ggrepel)
library(RColorBrewer)

df <- read_csv("state_of_the_union.csv")

df                                         %<>%
  unnest_tokens(word, text)                %>%
  anti_join(get_stopwords())               %>%
  filter(!str_detect(word, "[0-9]+ | NA")) %>%
  filter(!word %in% c("united","states","must","may","can")) %>%
  add_count(word)                          %>%
  filter(n > 1)                            %>%
  select(-n)

sparse_df <- df               %>%
  count(president, word) %>%
  cast_sparse(president, word, n)

n_topics = seq(10,60,10) # change to seq

plan("default")
start_time_stm <- Sys.time()

many_models_stm <- data_frame(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(sparse_df, K = ., verbose = FALSE)))

end_time_stm <- Sys.time()
beep()

heldout <- make.heldout(sparse_df)

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

# DIAGNOSTIC PLOTS

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
       y        = NULL)

excl_sem_plot <- k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  #filter(K %in% seq(2,15)) %>%
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
animate(anim_plot, nframes = 14, fps = 0.5)

k_result %>% 
  ggplot(aes(x=mean_semantic_coherence, y = mean_exclusivity,
                        label=K)) +
  geom_point(size=3) +
  geom_text_repel(size=5) 

# ---------------------------------
# SELECT STM MODEL

topic_model_stm <- k_result %>% 
  filter(K ==40)            %>% 
  pull(topic_model)         %>% 
  .[[1]]

topic_model_stm

# ---------------------------------
# EXPLORE STM MODEL

# BETA

td_beta <- tidy(topic_model_stm)

top_terms <- td_beta %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number()))

top_terms %>%
  ggplot(aes(order, beta,fill = rev(factor(topic)))) +
  ggtitle("Positive review topics") +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(
    breaks = top_terms$order,
    labels = top_terms$term,
    expand = c(0,0)) +
  facet_wrap(~ topic,scales="free") +
  coord_flip(ylim=c(0,max(top_terms$beta))) +
  labs(x="",y=expression(beta)) +
  theme(axis.title=element_blank()) + 
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank(),
        strip.text.x = element_text(size=16))

top_terms <- td_beta  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_stm, matrix = "gamma",
                 document_names = rownames(sparse_df))

gamma_terms <- td_gamma              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot <- gamma_terms %>%
  top_n(15, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  #geom_text(hjust = 1.05, vjust=0, size = 3, family = "Helvetica") +
  geom_text(hjust = 0, nudge_y = 0.0100, size = 6,
            family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.6),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma)) +
  theme_minimal() + 
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid   = element_blank()) 

stm_plot

# HEATMAP

posterior <- topic_model_stm$theta  %>% 
  data.frame() %>%
  mutate(id = factor(unique(df$president), levels= unique(df$president)))

heat_df <- posterior %>%
  gather(topic, value, -id)      %>%
  as_tibble() %>%
  mutate(topic = as.factor(as.numeric(str_replace(topic,"X",""))))

heat_df$president <- factor(heat_df$president, levels = unique(df$president))

heat_df %>% 
  ggplot(aes(topic, fct_rev(id))) + 
  geom_tile(aes(fill = log(value)), colour = "snow") + 
  #scale_fill_stellenbosch(discrete = F, "wine") +
  scale_fill_gradient(low = "snow", high = "darkred",guide=F) +
  theme_minimal() +
  theme(axis.text.y  = element_text(size = 18),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_blank())
