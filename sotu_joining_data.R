# SOTU - Joinining and preparing data for classification
# june 2019
# Peer Christensen

library(tidyverse)
library(purrr)
library(gbm)

# stylistic features
df1 <- read_csv("sotu_w_style_measures.csv") %>%
  rename(document=doc_id)

# sentiment
df2 <- read_csv("sotu_positive_sentiment_prop.csv")
  
# topics
df3 <- read_csv("sotu_topic_posterior_vals.csv")
  
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

write_csv(df,"sotu_df_classification.csv")

df <- df %>%
  mutate_if(is.numeric, scale,center=T,scale=T) %>%
  mutate(party = factor(party))

## gbm

train_ind <- createDataPartition(df$party,p=.75,list=F)

train1 <- df[train_ind,]
test1 <- df[-train_ind,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  repeats = 5)

tic()
gbmFit1 <- train(party ~ ., data = train1, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
toc()

test_pred1 <- predict(gbmFit1, newdata = test1)

cm1 <- confusionMatrix(data = test_pred1, reference = test1$party)

varImp(gbmFit1, scale = FALSE)

