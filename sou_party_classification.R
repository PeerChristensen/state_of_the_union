
library(tidyverse)
library(tidytext)
library(magrittr)
library(caret)
library(glue)


df <- read_csv("state_of_the_union.csv")

df                                         %<>%
  filter(party == "Republican" | party == "Democrat") %>%
  unnest_tokens(word, text)                %>%
  anti_join(get_stopwords())               %>%
  filter(!str_detect(word, "[0-9]+ | NA")) %>%
  filter(!word %in% c("united","states","must","may","can")) %>%
  add_count(word)                          %>%
  filter(n > 10)                           %>%
  select(-n, -president, -date)            %>%
  mutate(party = as.factor(ifelse(party == "Republican",1,0)))
  
  #group_by(document) %>%
  #mutate(text = collapse(word,sep=" ")) %>%
  #ungroup() %>%
  #select(-word) %>%
  #distinct()

set.seed(42)

idx <- createDataPartition(df$party, 
                           p = 0.8, 
                           list = FALSE, times = 1)

df_train <- df[ idx,]
df_test  <- df[-idx,]

train_dtm <- df_train       %>%
  dplyr::count(party, word) %>%
  cast_dtm(party, word, n)

test_dtm <- df_test         %>%
  dplyr::count(party, word) %>%
  cast_dtm(party, word, n)

df_train = as.matrix(df_train)

train <- as.matrix(train_dtm)
train <- cbind(train, c(0, 1))

colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)




xgb_model <- xgb.train(list(max_depth = 7, 
                            eta = 0.1, 
                            objective = "binary:logistic",
                            eval_metric = "error", nthread = 1),
                            xgb.DMatrix(train_dtm, 
                                   label = df_train$party),
                            nrounds = 50)


RTextTools
                           

