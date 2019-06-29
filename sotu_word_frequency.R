# SOTU - Comparing word frequency for democrats and republicans
# june 2019
# Peer Christensen

library(tidyverse)
library(tidytext)
library(udpipe)
library(readability)

theme_set(theme_minimal())

# ------------ READABILITY ----------------------

df <- read_csv("state_of_the_union.csv") 

my_stop_chars <- c("NA|[0-9]|_")

dem_rep_ratios <- df %>%
  filter(party == "Republican" | party == "Democrat") %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word,my_stop_chars)) %>%
  count(party,word) %>%
  spread(party, n, fill = 0) %>%
  filter(Democrat > 25,Republican > 25) %>%
  ungroup() %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(Republican / Democrat)) %>%
  arrange(desc(logratio))

# Plot the log odds ratio for each word by device
dem_rep_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip()  + 
  scale_fill_manual(name = "Party", labels = c("Republican","Democrat"),
                      values = c("steelblue","darkorange"))

# using tf_idf
dem_rep_tf_idf <- df %>%
  filter(party == "Republican" | party == "Democrat") %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word,my_stop_chars)) %>%
  count(party,word) %>%
  bind_tf_idf(word,party,n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(party) %>%
  top_n(15,tf_idf) %>%
  arrange(party) %>%
  ungroup() %>%
  mutate(order = rev(row_number()), 
         party = factor(party, levels = c("Republican", "Democrat")))

dem_rep_tf_idf %>%
  ggplot(aes(order,tf_idf, fill = party)) +
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = dem_rep_tf_idf$order, 
                     labels = dem_rep_tf_idf$word, 
                     expand = c(0,0)) + 
  facet_wrap(~party, scales = "free") +
  coord_flip() +
  scale_fill_manual(values=c("steelblue","darkorange"))

                    