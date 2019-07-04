# SOTU - Clustering presidents and parties
# june 2019
# Peer Christensen

library(tidyverse)
library(factoextra)
library(ggcorrplot)

theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 20,
                                            margin=margin(20,0,20,0)),
                  axis.title.x = element_text(size = 14,margin=margin(10,0,10,0)),
                  axis.title.y = element_text(size = 14,margin=margin(0,10,0,10)),
                  axis.text = element_text(size = 11,margin=margin(5,0,5,0)),
                  plot.margin = margin(10, 40, 10, 10)))

# data
df <- read_csv("data/sotu_w_style_measures.csv")

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

ggsave("plots/style_feature_correlation.png")

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
  ggtitle("Similarity between presidents") +
  labs(x=NULL,y=NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20,
                                  margin=margin(20,0,20,0)),
        axis.text.x = element_text(hjust = 1,angle = 45,size=11),
        axis.text.y = element_text(size=11),
        plot.margin = margin(10, 40, 10, 10))

ggsave("plots/distance_matrix_presidents_style.png")

# k means

fviz_nbclust(df_scaled, kmeans, method = "wss") +
  ggtitle("Optimal Number of Clusters: K-means Clustering")

set.seed(3243789)
km.res1 <- kmeans(df_scaled, 2, nstart = 25)

fviz_cluster(km.res1, data = df_scaled,
             ellipse.type = "convex",
             repel = T,
             # palette = inferno(10)[c(4,6,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering") 

ggsave("plots/k_means_2_clust.png")

df_scaled2 <- df_scaled

party <- df %>%
  distinct(party,president) %>%
  pull(party)

rownames(df_scaled2) <- paste(party,1:40)

fviz_cluster(km.res1, data = df_scaled2,
             ellipse.type = "convex",
             repel = T,
             # palette = inferno(10)[c(4,6,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering")

ggsave("plots/k_means_2_clust_party_labs.png")

km.res2 <- kmeans(df_scaled, 4, nstart = 25)

fviz_cluster(km.res2, data = df_scaled,
             ellipse.type = "convex",
             repel = T,
            # palette = inferno(10)[c(4,6,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering")

ggsave("plots/k_means_4_clust.png")
