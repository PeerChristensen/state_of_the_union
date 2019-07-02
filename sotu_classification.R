# SCRIPT FOR CLASSIFYING DOCUMENTS (DEM vs. REP)
# PEER CHRISTENSEN
# SEPTEMBER 2018

library(tidytext)
library(tidyverse)
library(caret)
library(tictoc)

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
  filter(!str_detect(word,my_stop_chars))

df_sparse <- df %>% 
  cast_sparse(document,word,n) 

df_sparse <- df_sparse[,order((colSums(-df_sparse)))]

df_sparse <- df_sparse[,1:1000] %>%
  as.matrix() %>%
  tibble() %>%
  add_column(party = factor(df2$party),
             document = df2$document)

# export matrix 
save(df_sparse, file="sparse_matrix_1000.Rdata")

train_ind <- createDataPartition(df_sparse$party,p=.75,list=F)

train1 <- df_sparse[train_ind,]
test1 <- df_sparse[-train_ind,]

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

gbmFit1

test_pred1 <- predict(gbmFit1, newdata = test1)

cm1 <- confusionMatrix(data = test_pred1, reference = test1$party)

# stylistic predictors

df3 <- read_csv("sotu_w_style_measures.csv") %>%
  select(document = doc_id,party,Flesch_Kincaid,n_words,mean_sent_length) %>%
  mutate(party = factor(party)) %>%
  filter(party == "Democrat" | party == "Republican")

df3 <-droplevels(df3)

train2 <- df3[train_ind,]
test2 <- df3[-train_ind,]

gbmFit2 <- train(party ~ ., data = train2, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit2

test_pred2 <- predict(gbmFit2, newdata = test2)

cm2 <- confusionMatrix(data = test_pred2, reference = test2$party)
  
# stylistic predictors combined with words

df4 <- inner_join(df3,df_sparse, by = "document") %>%
  mutate(party = party.x) %>%
  select(-party.y, -party.x,-document)

df4 <-droplevels(df4)

train3 <- df4[train_ind,]
test3 <- df4[-train_ind,]

tic()
gbmFit3 <- train(party ~ ., data = train3, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
toc()

gbmFit3

test_pred3 <- predict(gbmFit3, newdata = test3)

cm3 <- confusionMatrix(data = test_pred3, reference = test3$party)

cm1$overall[1]
cm2$overall[1]
cm3$overall[1]

#### H20 autoML


