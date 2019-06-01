


library(udpipe)
library(tidyverse)
library(tidytext)
library(magrittr)
library(tm)
library(wordcloud)
library(happyorsad)
library(ggrepel)
library(stm)
library(furrr)
library(beepr)

########## 1. Prepare data ################################

df <- read_csv("vitaepro_data2.csv")

udmodel <- udpipe_download_model(language = "danish")


my_stopwords <- c("så","vitaepro","pro","vita","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

df %>%
  filter(rating >= 4) 

%>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "") %>%                
  select(-n, -sentiment,-time,-company,-rating)


x <- udpipe(x = df$review,
            object = udmodel)
