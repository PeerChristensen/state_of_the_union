library(tidytext)
library(tidyverse)
library(magrittr)

df <- read_csv("state_of_the_union.csv")

df                                         %<>%
  unnest_tokens(word, text)                %>%
  anti_join(get_stopwords())               %>%
  filter(!str_detect(word, "[0-9]+ | NA")) %>%
  filter(!word %in% c("united","states","must","may","can")) %>%
  add_count(word)                          %>%
  filter(n > 10)                           %>%
  select(-n)

df_dtm_pres <- df               %>%
  dplyr::count(president, word) %>%
  cast_dtm(president, word, n)

model <- LDA(df_dtm_pres,k=2, method = "VEM")

#Determine the posterior probabilities of the topics for each document and of the terms for each topic for a fitted topic model.
d=topicmodels::posterior(model, df_dtm_pres)[["topics"]]

heat_df <- d   %>% 
  data.frame() %>%
  mutate(president = rownames(d)) %>%
  mutate(president = unique(df$president))

heat_df                                 %<>% 
  gather(topic, value, -president)      %>%
  mutate(topic = as.factor(as.numeric(str_replace(topic,"X",""))),
         president = factor(president)) %>%
  as_tibble()

heat_df$president <- factor(heat_df$president, levels = unique(df$president))

heat_df %>% ggplot(aes(topic,fct_rev(president))) + 
  geom_tile(aes(fill = value), colour = "snow") + 
  #scale_fill_stellenbosch(discrete = F, "wine") +
  scale_fill_gradient(low = "snow", high = "darkred") +
  labs(title = "Topics associated with presidents",
       y     = "presidents") +
  theme_minimal()

ggsave("heatmap.png")
  