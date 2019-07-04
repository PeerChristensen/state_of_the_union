# SOTU - Joinining and preparing data for classification
# june 2019
# Peer Christensen

library(tidyverse)
library(purrr)

# stylistic features
df1 <- read_csv("data/sotu_w_style_measures.csv") %>%
  rename(document=doc_id)

# sentiment
df2 <- read_csv("data/sotu_positive_sentiment_prop.csv")
  
# topics
df3 <- read_csv("data/sotu_topic_posterior_vals.csv")
  
# sparse matrix - (df_sparse)
load("sparse_matrix_1000.Rdata")

df_sparse <- df_sparse %>%
  select(-party) 

df_sparse <- df_sparse$. %>%
  data.frame() %>%
  add_column(document = df_sparse$document) %>%
  select(document, everything())

df <- list(df1, df2, df3) %>% 
  reduce(left_join, by = "document")

df <- df %>%
  select(-document,-date,-party.y,-president.x,-president.y,
         -Gunning_Fog_Index,-Coleman_Liau,-SMOG,-Automated_Readability_Index,
        -Average_Grade_Level) %>%
  rename(party = party.x) %>%
  filter(party == "Democrat" | party == "Republican") %>%
  bind_cols(df_sparse) %>%
  select(-document)

write_csv(df,"data/sotu_df_classification.csv")

