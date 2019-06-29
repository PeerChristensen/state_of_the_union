# Clean data


ud_english <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")

df_1 <- read_csv("state_of_the_union.csv")

df_2 <- udpipe(df_1$text, object = ud_english,doc_id=df_1$document)

df_1 <- df_1 %>%
  mutate(doc_id = document) %>%
  select(-text, -document) %>%
  distinct()

df_2 <- df_2 %>% 
  as_tibble() %>%
  select(doc_id,paragraph_id,sentence_id, token, lemma, upos, start, end)

df <- inner_join(df_2,df_1, by = "doc_id")

write_csv(df, "sotu_clean.csv")
