# SCRIPT FOR CLASSIFYING DOCUMENTS (DEM vs. REP)
# PEER CHRISTENSEN
# SEPTEMBER 2018

library(tidytext)
library(tidyverse)
library(caret)

# words alone 

df <- read_csv("state_of_the_union.csv")

df2 <- df %>%
  select(document, party) %>%
  filter(party == "Democrat" | party == "Republican") %>%
  distinct()
  
my_stop_chars <- c("NA|[0-9]|_")

df <- df %>%
  unnest_tokens(word, text) %>%
  filter(party == "Democrat" | party == "Republican") %>%
  count(party,document, word) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word,my_stop_chars),
         n > 1)

df_sparse <- df %>% 
  cast_sparse(document,word,n) %>%
  as.matrix() %>%
  tibble() %>%
  add_column(party = factor(df2$party))

train_ind <- createDataPartition(df_sparse$party,p=.8,list=F)

train <- df_sparse[train_ind,]
test <- df_sparse[-train_ind,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

gbmFit1 <- train(party ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

test_pred <- predict(gbmFit1, newdata = test)

confusionMatrix(data = test_pred, reference = test$party)

# stylistic predictors

df3 <- read_csv("sotu_w_style_measures.csv") %>%
  select(party,Flesch_Kincaid,n_words,mean_sent_length) %>%
  mutate(party = factor(party)) %>%
  filter(party == "Democrat" | party == "Republican")

df3 <-droplevels(df3)

train_ind <- createDataPartition(df3$party,p=.8,list=F)

train <- df3[train_ind,]
test <- df3[-train_ind,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

gbmFit1 <- train(party ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

test_pred <- predict(gbmFit1, newdata = test)

confusionMatrix(data = test_pred, reference = test$party)
  
# stylistic predictors combined with words

df4 <- inner_join(df3,df_sparse)

df4 <-droplevels(df4)

train_ind <- createDataPartition(df4$party,p=.8,list=F)

train <- df4[train_ind,]
test <- df4[-train_ind,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated ten times
  repeats = 5)

gbmFit1 <- train(party ~ ., data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

test_pred <- predict(gbmFit1, newdata = test)

confusionMatrix(data = test_pred, reference = test$party)

