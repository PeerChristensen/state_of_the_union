# SOTU - Topic modelling
# june 2019
# Peer Christensen

# ---------------------------------
# LOAD PACKAGES

library(tidytext)
library(tidyverse)
library(stm)
library(furrr)
library(beepr)
library(future)
library(topicmodels)
library(glue)
library(gganimate)
library(ggthemes)
library(scales)
library(gridExtra)
library(tictoc)

theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 20,
                                            margin=margin(20,0,20,0)),
                  axis.title.x = element_text(size = 14,margin=margin(10,0,10,0)),
                  axis.title.y = element_text(size = 14,margin=margin(0,10,0,10)),
                  axis.text = element_text(size = 11,margin=margin(5,0,5,0)),
                  plot.margin = margin(10, 40, 10, 10)))

# ---------------------------------
# LOAD & PREPARE DATA

df <- read_csv("data/sotu_parsed.csv")

sotu_stop_words <- c("united","states","government","congress","citizen",
                     "house","representatives","na","country","year")

df <- df %>%
  unnest_tokens(word,lemma) %>%
  filter(upos=="PROPN" | upos == "NOUN") %>%
  filter(!word %in% sotu_stop_words) %>%
  add_count(word) %>%
  filter(n > 3) %>%
  select(doc_id,word)

# ---------------------------------
# BUILD STM MODELS

df_sparse <- df       %>%
  count(doc_id, word) %>%
  cast_sparse(doc_id, word, n)

plan("sequential")

tic()

n_topics = seq(2,20,2)

models <- tibble(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(df_sparse, K = ., verbose = T)))

toc()
beep()

save(models,file="data/sotu_topic_models.Rdata")

load("data/sotu_topic_models.Rdata")

# ---------------------------------
# EVALUATE STM MODELS

heldout <- make.heldout(df_sparse)

k_result <- models %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, df_sparse),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, df_sparse),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics")

ggsave("plots/sotu_stm_eval.png")

# ---------------------------------
# EXCLUSIVITY 

#BOXPLOT

k_result$exclusivity                   %>%
  unlist()                             %>% 
  enframe()                            %>% 
  mutate(K=rep(k_result$K,k_result$K)) %>%
  group_by(K)                          %>%
  ggplot(aes(x = factor(K), y = value)) +
  geom_boxplot(fill=NA,colour = "darkorange") +
  geom_jitter(width=.2, alpha = .7,colour = "steelblue") +
  labs(x = "Number of topics",
       y = "Exclusivity",
       title = "Exclusivity by number of topics")

ggsave("plots/sotu_stm_exclusivity_boxplot.png")

# LINE PLOT

k_result$exclusivity %>%
  unlist() %>% 
  enframe() %>% 
  mutate(K=rep(k_result$K,k_result$K)) %>%
  group_by(K) %>%
  summarise(m = mean(value)) %>%
  ggplot(aes(x = K, y = m)) +
  geom_line() +
  labs(x = "Number of topics",
       y = "Exclusivity",
       title = "Exclusivity by number of topics")

ggsave("plots/sotu_stm_exclusivity_lineplot.png")

# ---------------------------------
# COMPARE EXCLUSIVITY AND SEMANTIC COHERENCE

k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  filter(K %in% seq(4,10,2)) %>%
  unnest()                                         %>%
  mutate(K = as.factor(K))                         %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity") +
  scale_color_viridis_d()

ggsave("plots/sotu_stm_excl_sem_plot.png")

# ---------------------------------
# ANIMATE EXCLUSIVITY AND SEM. COHERENCE

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = 10, fps = 0.5)

# ---------------------------------
# SELECT STM MODELS

topic_model_stm_small <- k_result %>% 
  filter(K ==6)             %>% 
  pull(topic_model)         %>% 
  .[[1]]

save(topic_model_stm_small, file="data/topic_model_stm_small.Rdata")

topic_model_stm_big <- k_result %>% 
  filter(K == 10)             %>% 
  pull(topic_model)         %>% 
  .[[1]]

save(topic_model_stm_big, file="data/topic_model_stm_big.Rdata")

load("data/topic_model_stm_big.Rdata") 

# ---------------------------------
# EXPLORE STM MODELS

# small model
td_beta_small <- tidy(topic_model_stm_small)

top_terms <- td_beta_small  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma_small <- tidy(topic_model_stm_small, matrix = "gamma",
                 document_names = rownames(df_sparse))

gamma_terms_small <- td_gamma_small  %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot_small <- gamma_terms_small %>%
  top_n(10, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,size=0) +
  geom_text(hjust = -.05, vjust=0, size = 5, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(gamma_terms_small$gamma)+.3),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot_small

ggsave("plots/sotu_stm_plot_small.png", width=10)

# big model

td_beta_big <- tidy(topic_model_stm_big)

top_terms <- td_beta_big  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma_big <- tidy(topic_model_stm_big, matrix = "gamma",
                       document_names = rownames(df_sparse))

gamma_terms_big <- td_gamma_big  %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot_big <- gamma_terms_big %>%
  top_n(10, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,size=0) +
  geom_text(hjust = -.05, vjust=0, size = 5, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(gamma_terms_big$gamma)+.3),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot_big

ggsave("plots/sotu_stm_plot_big.png", width=10)

# heatmap

df2 <- read_csv("data/state_of_the_union.csv") %>%
  distinct(president,document)

posterior <- topic_model_stm_big$theta  %>% 
  data.frame() %>%
  add_column(president =factor(df2$president))

heat_df <- posterior %>%
  gather(topic, value, -president)      %>%
  as_tibble() %>%
  mutate(topic = as.factor(as.numeric(str_replace(topic,"X",""))),
         row = row_number())

export_posterior <- posterior %>%
  mutate(document = 1:nrow(posterior)) %>%
  select(president,document,everything())

write_csv(export_posterior,"data/sotu_topic_posterior_vals.csv")

# inspect in wide format
heat_df %>% spread(topic,value)

# heatmap of mean vals by president
heat_df %>% 
  select(-row) %>%
  group_by(president,topic) %>%
  mutate(mean = mean(value)) %>%
  select(-value) %>%
  distinct() %>%
  ungroup() %>%
  mutate(row = row_number()) %>%
  ggplot(aes(topic, reorder(president,rev(row)))) + 
  geom_tile(aes(fill = log(mean)), colour = "snow") + 
  scale_fill_gradient(low = "snow", high = "darkorange",guide=F) +
  labs(x="Presidents", y = "Topic",
       title = "Topic probabilities for each president")

ggsave("plots/sotu_heatmap_topics.png")

# heatmap of unaggregated vals by president
heat_df %>% 
  ggplot(aes(topic, reorder(president,rev(row)))) + 
  geom_tile(aes(fill = log(value)), colour = "snow") + 
  scale_fill_gradient(low = "snow", high = "darkorange",guide=F) +
  labs(x="Presidents", y = "Topic",
       title = "Topic probabilities for each president")

ggsave("plots/sotu_heatmap_topics2.png")

# add party and test effect

df <- read_csv("data/sotu_parsed.csv")

df2 <- read_csv("data/state_of_the_union.csv") %>%
  select(party,document) %>% # for adding party as variable
  filter(party == "Republican" | party == "Democrat") %>%
  distinct()

sotu_stop_words <- c("united","states","government","congress","citizen",
                     "house","representatives","na","country","year")

df <- df %>%
  unnest_tokens(word,lemma) %>%
  filter(upos=="PROPN" | upos == "NOUN") %>%
  filter(!word %in% sotu_stop_words) %>%
  add_count(word) %>%
  filter(n > 3) %>%
  select(doc_id,word) %>%
  inner_join(df2, by = c("doc_id" = "document"))

# ---------------------------------
# BUILD STM MODEL

df_sparse <- df       %>%
  count(doc_id, word) %>%
  cast_sparse(doc_id, word, n)

meta <- df %>%
  distinct(party,doc_id)

model <- stm(df_sparse, K = 10, verbose = F,
             prevalence = ~party + s(doc_id),data=meta)

# effect of party on each topic
est <- estimateEffect(~party, model,metadata = meta,
                      uncertainty="None")
summary(est)

# sort coefficients and find top three topics per party
coefficient <- c()

for (i in topic) {
  coefficient[i] = est$parameters[[i]][[1]]$est[2]
}

coef_df <- tibble(topic = 1:10,coefficient) %>%
  arrange(coefficient)

# top 3 Democratic topics
labelTopics(model, topics=coef_df$topic[1])
labelTopics(model, topics=coef_df$topic[2])
labelTopics(model, topics=coef_df$topic[3])

# top 3 Republican topics

labelTopics(model, topics=coef_df$topic[10])
labelTopics(model, topics=coef_df$topic[9])
labelTopics(model, topics=coef_df$topic[8])


