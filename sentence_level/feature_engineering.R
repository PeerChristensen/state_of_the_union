# Create features for sentence level classicfication
# PEER CHRISTENSEN
# AUGUST 2019

library(tidyverse)
library(readability)
library(tidytext)
library(happyorsad)
library(sentimentr)

df <- read_csv("data/state_of_the_union_sentences.csv") %>%
  mutate(sentence_id = as.character(sentence_id))

# add readability
readability <- readability(df$sentence,
                           order.by.readability = F,
                           grouping.var = df$sentence_id)

df <- df %>%
  left_join(readability,by = "sentence_id")

# add formality
formality_data <- read_csv("data/sotu_PoS_sentences.csv") %>%
  filter(party == "Democrat" | party == "Republican") %>%
  mutate(sentence_id = as.character(sentence_id))

pos_keep <- c("NOUN","ADJ","ADP","DET","PRON","VERB","AUX","ADV")

df <- formality_data %>% 
  count(sentence_id,upos) %>% 
  filter(upos %in% pos_keep) %>%
  mutate(F_class = ifelse(upos %in% pos_keep[1:4],"f","c")) %>%
  group_by(sentence_id,F_class) %>%
  summarise(total = sum(n)) %>%
  spread(F_class, total) %>%
  mutate(total_all=f+c) %>%
  summarise(F_measure = 50*(((f-c)/(total_all))+1)) %>%
  left_join(df, by = "sentence_id")

# add sentence length
df <- df %>% 
  mutate(sentence_length = lengths(str_split(sentence," ")))

# add mean word length
mean_word_length <- df$sentence %>% 
  str_split(" ") %>% 
  map(nchar) %>% 
  map(mean) %>%
  unlist()

df$mean_word_length <- mean_word_length

# add sentiment score
df$sentiment_score <- df$sentence %>% 
  map(happyorsad,language="en") %>%
  unlist()

sentiment_score2 <- df$sentence %>%
  get_sentences() %>%
  sentiment() %>%
  mutate(element_id = as.character(element_id))

df <- df %>% 
  left_join(sentiment_score2, 
            by = c("sentence_id" = "element_id")) %>%
  rename(sentiment_score2 = sentiment) %>%
  select(-sentence_id.y) %>%
  drop_na()

df <- df %>%
  distinct(sentence_id, .keep_all = T)

### write csv

write_csv(df,"data/sotu_w_style_measures_sentences.csv")
