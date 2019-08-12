# SCRIPT FOR CLASSIFYING sentences (DEM vs. REP)
# PEER CHRISTENSEN
# SEPTEMBER 2018

library(tidytext)
library(tidyverse)
library(caret)
library(tictoc)

# words alone 

df <- read_csv("data/sotu_w_style_measures_& topics_sentences.csv")

df2 <- df %>%
  select(sentence_id, party)

my_stop_chars <- c("NA|[0-9]|_")

df <- df %>%
  unnest_tokens(word, sentence) %>%
  count(party,sentence_id, word) %>%
  #anti_join(stop_words) %>%
  filter(!str_detect(word,my_stop_chars))

df_sparse <- df %>% 
  cast_sparse(sentence_id,word,n)  %>%
  as.matrix() %>%
  data.frame()

df_sparse <- df_sparse[,order(-colSums(df_sparse))]

df_sparse <- df_sparse[,1:1000]

df_sparse_a <- df_sparse %>%
  add_column(party_fct = factor(df2$party),
             sentence_id = df2$sentence_id)

df_sparse_b <- df_sparse_a %>%
  select(-sentence_id)

#df_sparse <- df_sparse %>% as.matrix %>% tibble()

# export matrix 
save(df_sparse_a, file="data/sparse_matrix_1000_sentence.Rdata")

train_ind <- createDataPartition(df_sparse_b$party_fct,p=.7,list=F)

train1 <- df_sparse_b[train_ind,]
test1 <- df_sparse_b[-train_ind,]

fitControl <- trainControl(
  method = "repeatedcv",
  repeats = 5,
  number = 5)

tic()
gbmFit1 <- train(party_fct ~ ., data = train1, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
toc()

gbmFit1

test_pred1 <- predict(gbmFit1, newdata = test1)

cm1 <- confusionMatrix(data = test_pred1, reference = test1$party_fct)

# stylistic predictors

df3a <- read_csv("data/sotu_w_style_measures_sentences.csv") %>%
  select(sentence_id,F_measure,party,Flesch_Kincaid,
         sentence_length,mean_word_length,sentiment_score,sentiment_score2,word_count) %>%
  mutate(party = factor(party))

df3a <-droplevels(df3a)

df3b <- df3a %>% select(-sentence_id)

train2 <- df3b[train_ind,]
test2 <- df3b[-train_ind,]

tic()
gbmFit2 <- train(party ~ ., data = train2, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = T)
toc()

gbmFit2

test_pred2 <- predict(gbmFit2, newdata = test2)

cm2 <- confusionMatrix(data = test_pred2, reference = test2$party)

# stylistic predictors combined with words

df4 <- inner_join(df3a,df_sparse_a, by = "sentence_id") %>%
  mutate(party_fct = party.x) %>%
  select(-party.y, -party.x,-sentence_id)

df4 <-droplevels(df4)

train3 <- df4[train_ind,]
test3 <- df4[-train_ind,]

tic()
gbmFit3 <- train(party_fct ~ ., data = train3, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
toc()

gbmFit3

test_pred3 <- predict(gbmFit3, newdata = test3)

cm3 <- confusionMatrix(data = test_pred3, reference = test3$party_fct)

cm1$overall[1]
cm2$overall[1]
cm3$overall[1]


