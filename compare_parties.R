# SOTU - Comparing parties and presidents
# june 2019
# Peer Christensen

library(tidyverse)
library(tidytext)
library(udpipe)

df <- read_csv("state_of_the_union.csv")

d <- 

####
df <- read_csv("sotu_clean.csv")

df_2 <- df_2 %>%
  unnest_tokens(word,lemma) %>%
  filter(upos=="PROPN" | upos == "NOUN") %>%
  filter(!word %in% sotu_stop_words) %>%
  add_count(word) %>%
  filter(n > 3) %>%
  select(-n)
