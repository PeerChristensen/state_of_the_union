# SOTU - Clustering presidents and parties
# june 2019
# Peer Christensen

library(tidyverse)
library(factoextra)
library(ggcorrplot)

# data
df <- read_csv("sotu_w_style_measures.csv")

df <- df %>% 
  select(party,president,F_measure,Flesch_Kincaid,n_words,mean_sent_length)

# correlations among features
sign_test <- df %>% 
  select(F_measure,Flesch_Kincaid,n_words,mean_sent_length) %>%
  cor_pmat()

df  %>% 
  select(F_measure,Flesch_Kincaid,n_words,mean_sent_length) %>%
  cor() %>%
  ggcorrplot(type   = "lower", 
             p.mat  = sign_test,
             colors = c("steelblue", "snow", "darkorange")) +
  ggtitle("Correlations between features",
          subtitle = "Non-significant correlations marked with X")

# distance matrix

df_scaled <- df %>%
  select(-party) %>%
  group_by(president) %>%
  summarise_all(funs(if(is.numeric(.)) mean(., na.rm = TRUE))) %>%
  mutate_if(is.numeric, scale,center=T,scale=T) %>%
  data.frame()

row.names(df_scaled) <- df_scaled$president

df_scaled <- df_scaled %>%
  select(-president)
  

df_dist <- get_dist(df_scaled, stand = TRUE)

fviz_dist(df_dist,gradient = list(low = "steelblue", mid = "white", high = "darkorange")) +
  theme_minimal() +
  ggtitle("Distance matrix",
          subtitle  = "Similarity between presidents based on all features") +
  theme(axis.text.x = element_text(hjust = 1,angle = 45),
        axis.title = element_blank())

# k means

fviz_nbclust(df_scaled, kmeans, method = "wss") +
  ggtitle("Optimal Number of Clusters: K-means Clustering")

set.seed(3243789)
km.res1 <- kmeans(df_scaled, 4, nstart = 25)

fviz_cluster(km.res1, data = df_scaled,
             ellipse.type = "convex",
             repel = T,
             # palette = inferno(10)[c(4,6,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering") 

km.res2 <- kmeans(df_scaled, 4, nstart = 25)

fviz_cluster(km.res2, data = df_scaled,
             ellipse.type = "convex",
             repel = T,
            # palette = inferno(10)[c(4,6,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering")
