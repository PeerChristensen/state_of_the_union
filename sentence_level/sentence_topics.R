# sotu sentences: topics

library(tidytext)
library(stm)
library(tidyverse)
library(future)
library(furrr)
library(scales)

df <- read_csv("data/sotu_w_style_measures_sentences.csv")

sotu_stop_words <- c("united","states","government","congress","citizen",
                     "house","representatives","na","country","year")

pos_keep <- parts_of_speech %>% filter(pos == "Noun" | pos == "Plural")
  
df <- df %>%
  unnest_tokens(word,sentence) %>%
  filter(!word %in% sotu_stop_words) %>%
  anti_join(stop_words) %>%
  anti_join(pos_keep) %>%
  add_count(word) %>%
  filter(n > 3) %>%
  select(sentence_id,word)

df_sparse <- df       %>%
  count(sentence_id, word) %>%
  cast_sparse(sentence_id, word, n)

plan("sequential")

tic()

n_topics = seq(2,12,2)

models <- tibble(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(df_sparse, K = ., verbose = T)))

toc()
beep() # 4.2 hours

save(models,file="data/sotu_topic_models_sentences.Rdata")

load("data/sotu_topic_models_sentences.Rdata")

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

save(k_result,file="data/sotu_topic_models_k_results_sentences.Rdata")

load("data/sotu_topic_models_k_results_sentences.Rdata")

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

# select model
topic_model_sentence <- k_result %>% 
  filter(K ==8)             %>% 
  pull(topic_model)         %>% 
  .[[1]]

# explore model
td_beta <- tidy(topic_model_sentence)

top_terms <- td_beta  %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_sentence, matrix = "gamma",
                       document_names = rownames(df_sparse))

gamma_terms <- td_gamma  %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot <- gamma_terms %>%
  top_n(10, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE,size=0) +
  geom_text(hjust = -.05, vjust=0, size = 5, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(gamma_terms$gamma)+.3),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot

# join topic posteriors and sentence_id
posteriors <- tibble(sentence_id = df_sparse@Dimnames[[1]],
                     theta = topic_model_sentence$theta)
                     
df <- read_csv("data/sotu_w_style_measures_sentences.csv") %>%
  mutate(sentence_id=as.character(sentence_id)) %>%
  inner_join(posteriors)

write_csv(df,"data/sotu_w_style_measures_& topics_sentences.csv")

