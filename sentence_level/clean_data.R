# Create data set for sentence level classicfication
# PEER CHRISTENSEN
# AUGUST 2019

library(tidyverse)
library(tidytext)
library(udpipe)

# documents
df <- read_csv("data/state_of_the_union.csv")

df1 <- df %>%
  group_by(document) %>%
  mutate(text = paste0(text, collapse = " ")) %>%
  distinct() %>%
  ungroup()

write_csv(df1, "data/state_of_the_union_documents.csv")

# sentences

my_stop_chars <- c("NA|Applause|[0-9]|_")

df2 <- df1 %>%
  mutate(text = str_remove_all(text, my_stop_chars)) %>%
  unnest_tokens(output = sentence, 
                input  = text, 
                token  = "sentences") %>%
  filter(party == "Democrat" | party == "Republican") %>%
  mutate(sentence_id = row_number()) %>%
  group_by(document) %>%
  filter(sentence_id != min(sentence_id)) %>% # remove 1st sentences
  ungroup() 
    
write_csv(df2, "data/state_of_the_union_sentences.csv")

### PoS tagged data

ud_english <- udpipe_load_model("data/english-ewt-ud-2.3-181115.udpipe")

df_1 <- read_csv("data/state_of_the_union_sentences.csv")

df_2 <- udpipe(df_1$sentence, object = ud_english,doc_id=df_1$sentence_id)

df_1 <- df_1 %>%
  select(-document, -president, -date) %>%
  distinct() %>%
  mutate(sentence_id = as.character(sentence_id))

df_2 <- df_2 %>% 
  as_tibble() %>%
  select(doc_id, token, lemma, upos, start, end) %>%
  mutate(sentence_id = doc_id) %>%
  select(-doc_id)

df <- inner_join(df_2,df_1, by = "sentence_id")

write_csv(df, "data/sotu_PoS_sentences.csv")
