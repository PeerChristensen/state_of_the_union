# SCRIPT FOR EVALUATING STM AND LDA TOPIC MODELS
# PEER CHRISTENSEN
# SEPTEMBER 2018

# ---------------------------------
# SET TIMER

start_time <- Sys.time()

# ---------------------------------
# LOAD PACKAGES

library(tidytext)
library(tidyverse)
library(magrittr)
library(stm)
library(furrr)
library(beepr)
library(topicmodels)
library(gmailr)
library(glue)
library(gganimate)
library(ggthemes)
library(scales)
library(gridExtra)
library(udpipe)

# ---------------------------------
# LOAD & PREPARE DATA

#model    <- udpipe_download_model(language = "english")
ud_english <- udpipe_load_model(model)

df_1 <- read_csv("state_of_the_union.csv")

df_2 <- udpipe(df_1$text, object = ud_english,doc_id=df_1$document)

sotu_stop_words <- c("united","states","government","congress","citizen",
                     "house","representatives","na","country","year")

df_2 <- df_2 %>%
  unnest_tokens(word,lemma) %>%
  filter(upos=="PROPN" | upos == "NOUN") %>%
  filter(!word %in% sotu_stop_words) %>%
  add_count(word) %>%
  filter(n > 3) %>%
  select(-n)
  
n_topics = seq(2,20,2)

# ---------------------------------
# BUILD STM MODELS

df_sparse <- df_2     %>%
  count(doc_id, word) %>%
  cast_sparse(doc_id, word, n)

plan("default")
start_time_stm <- Sys.time()

many_models_stm <- data_frame(K = n_topics) %>%
  mutate(topic_model = future_map(K, ~stm(df_sparse, K = ., verbose = FALSE)))

end_time_stm <- Sys.time()
beep()

# ---------------------------------
# EVALUATE STM MODELS

heldout <- make.heldout(df_sparse)

k_result <- many_models_stm %>%
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
       title    = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 15")

ggsave("stm_evaluation.png")

# ---------------------------------
# EXCLUSIVITY 

#BOXPLOT

k_result$exclusivity                   %>%
  unlist()                             %>% 
  as_tibble()                          %>% 
  mutate(K=rep(k_result$K,k_result$K)) %>%
  group_by(K)                          %>%
  ggplot(aes(x = factor(K), y = value)) +
  geom_boxplot(fill=NA) +
  geom_jitter(width=.2, alpha = .7)

ggsave("exclusivity_boxplot.png")

# LINE PLOT

k_result$exclusivity %>%
  unlist() %>% 
  as_tibble() %>% 
  mutate(K=rep(k_result$K,k_result$K)) %>%
  group_by(K) %>%
  summarise(m = mean(value)) %>%
  ggplot(aes(x = K, y = m)) +
  geom_line()

ggsave("exclusivity_lineplot.png")

# ---------------------------------
# COMPARE EXCLUSIVITY AND SEMANTIC COHERENCE

k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  filter(K %in% seq(2,20,2)) %>%
  unnest()                                         %>%
  mutate(K = as.factor(K))                         %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity") +
  scale_color_viridis_d()

ggsave("excl_sem_plot.png")

# ---------------------------------
# ANIMATE EXCLUSIVITY AND SEM. COHERENCE

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = 10, fps = 0.5)

# ---------------------------------
# SELECT STM MODEL

topic_model_stm <- k_result %>% 
  filter(K ==10)            %>% 
  pull(topic_model)         %>% 
  .[[1]]

topic_model_stm

# ---------------------------------
# EXPLORE STM MODEL

td_beta <- tidy(topic_model_stm)

top_terms <- td_beta  %>%
  mutate(term = str_replace(term,"government","govt")) %>%
  arrange(beta)       %>%
  group_by(topic)     %>%
  top_n(6, beta)      %>%
  arrange(-beta)      %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma <- tidy(topic_model_stm, matrix = "gamma",
                 document_names = rownames(df_sparse))

gamma_terms <- td_gamma              %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_plot <- gamma_terms %>%
  top_n(10, gamma)      %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = -.05, vjust=0, size = 5, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.4),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "STM: Top 10 topics by prevalence in the SOU",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

stm_plot

ggsave("stm_plot.png", width=10)

# ---------------------------------
# BUILD LDA MODELS

df_dtm <- df_2        %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

start_time_lda_gibbs <- Sys.time()

burnin <- 1000
iter   <- 1000
keep   <- 50

many_models_lda <- n_topics %>%
  map(LDA, x = df_dtm, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep))

end_time_lda_gibbs   <- Sys.time()
beep()

# ---------------------------------
# EVALUATE LDA MODELS

# HARMONIC MEAN OF LOG-LIKELIHOOD

# extract logliks from each topic
logLiks_many <- many_models_lda %>% 
  map(function(x) x@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
#hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

hm_many <- logLiks_many %>%
  map_dbl(function(x) 1/mean(1/x))

ggplot(data.frame(n_topics, hm_many), aes(x=n_topics, y=hm_many)) + geom_path(lwd=1.5) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  ggplot2::annotate("text", x = 25, y = max(hm_many), label = paste("The optimal number of topics is", n_topics[which.max(hm_many)])) 

ggsave("likelihood_plot.png", width=10)

# PERPLEXITY !!! REQUIRES VEM SAMPLING

start_time_lda_vem <- Sys.time()

many_models_lda_vem <- n_topics %>%
  map(LDA, x = df_dtm, method = "VEM")

end_time_lda_vem <- Sys.time()
beep()

data_frame(k = n_topics,
           perplex = map_dbl(many_models_lda_vem, perplexity)) %>%
  ggplot(aes(k, perplex)) +
  geom_point() +
  geom_line() +
  labs(title    = "Evaluating LDA topic models",
       subtitle = "Optimal number of topics (smaller is better)",
       x        = "Number of topics",
       y        = "Perplexity")

ggsave("perplexity_plot.png", width=10)

# ---------------------------------
# SELECT LDA MODEL

topic_model_lda <- many_models_lda %>% 
  .[[5]]

topic_model_lda

# ---------------------------------
# EXPLORE LDA MODEL

td_beta2 <- tidy(topic_model_lda)

top_terms2 <- td_beta2          %>%
  mutate(term = recode(term, government = "govt")) %>%
  arrange(beta)                 %>%
  group_by(topic)               %>%
  top_n(6, beta)                %>%
  arrange(-beta)                %>%
  select(topic, term)           %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

td_gamma2 <- tidy(topic_model_lda, matrix = "gamma",
                  document_names = rownames(df_dtm))

gamma_terms2 <- td_gamma2            %>%
  group_by(topic)                    %>%
  summarise(gamma = mean(gamma))     %>%
  arrange(desc(gamma))               %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

lda_plot <- gamma_terms2 %>%
  top_n(10, gamma)       %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = -.05, size = 5, family = "Helvetica") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.48),
                     labels = percent_format()) +
  labs(x        = NULL, 
       y        = expression(gamma),
       title    = "LDA: Top 10 topics by prevalence in the SOU",
       subtitle = "With the top words that contribute to each topic") +
  scale_fill_viridis_d(begin=.3)

lda_plot 

ggsave("lda_plot.png", width=10)

# ---------------------------------
# COMPARE STM AND LDA

grid.arrange(stm_plot,lda_plot,ncol=2,widths=c(1,2))

compare_stm_lda <- arrangeGrob(stm_plot,lda_plot,ncol=2)
ggsave("compare_stm_lda.png", plot = compare_stm_lda, width=16)

# ---------------------------------
# COMPARE STM AND LDA: CLUSTER

# --------------------------------- 
# EXECUTION TIMES

end_time             <- Sys.time()
total_time_stm       <- end_time_stm - start_time_stm 
total_time_lda_gibbs <- end_time_lda_gibbs - start_time_lda_gibbs
total_time_lda_vem   <- end_time_lda_vem - start_time_lda_vem
total_eval_time      <- end_time - start_time
total_eval_time

beep("mario")

# --------------------------------- 
# SEND EMAIL WHEN DONE

my_email <- "hr.pchristensen@gmail.com"

mime()              %>%
  from(my_email)    %>%
  to(my_email)      %>%
  subject("From R") %>%
  text_body(glue("DONE\n
                 Elapsed time: {total_eval_time},\n
                 STM: {total_time_stm},\n
                 LDA_gibbs: {total_time_lda_gibbs}\n
                 LDA_vem: {total_time_lda_vem}")) %>%
  send_message()
