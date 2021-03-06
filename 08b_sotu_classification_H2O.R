# SOTU - classification with H2O
# june 2019
# Peer Christensen

library(tidyverse)
library(h2o)
library(autoMLviz)
library(caret)

df <- read_csv("sotu_df_classification.csv") %>%
  mutate(party = factor(party))

# split
set.seed(42)
index <- createDataPartition(df$party, p = 0.7, list = FALSE)

train_data <- df[index, ]
test_data  <- df[-index, ]

h2o.init(nthreads = -1) 
# if not working, use command line
#cd Downloads/h2o-3.24.0.5  #navigate to wherever you have the h2o installation
#java -jar h2o.jar

h2o.init(startH2O = F) 

h2o.no_progress()

train_hf <- as.h2o(train_data)
test_hf <- as.h2o(test_data)

response <- "party"
features <- setdiff(colnames(train_hf), response)

summary(train_hf$party, exact_quantiles = TRUE)
summary(test_hf$party, exact_quantiles = TRUE)

# train with autoML
aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  leaderboard_frame = test_hf,
                  balance_classes = TRUE,
                  max_runtime_secs = 60)

# View the AutoML Leaderboard
aml@leaderboard

best_model <- aml@leader

perf <- h2o.performance(best_model, test_hf)

pred <- h2o.predict(best_model, test_hf[, -1])

h2o.mean_per_class_error(best_model,xval = TRUE)
h2o.mean_per_class_error(perf)

h2o.auc(perf)
plot(perf)

h2o.confusionMatrix(perf) # or..
perf@metrics$cm$table


devtools::install_github("PeerChristensen/autoMLviz")
library(autoMLviz)

autoMLviz::auc_bars(aml,test_data=test_hf)
autoMLviz::varImp_ggplot(aml)
autoMLviz::lift4gains2(aml)


