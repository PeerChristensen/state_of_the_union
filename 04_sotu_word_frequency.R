# SOTU - Comparing word frequency for democrats and republicans
# june 2019
# Peer Christensen

library(tidyverse)
library(tidytext)

theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 20,
                                            margin=margin(20,0,20,0)),
                  axis.title.x = element_text(size = 14,margin=margin(10,0,10,0)),
                  axis.title.y = element_text(size = 14,margin=margin(0,10,0,10)),
                  axis.text = element_text(size = 11,margin=margin(5,0,5,0)),
                  plot.margin = margin(10, 40, 10, 10)))

df <- read_csv("data/state_of_the_union.csv") 

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

# Plot the log odds ratios
dem_rep_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(size = 0) +
  coord_flip()  + 
  scale_fill_manual(name = "Party", labels = c("Republican","Democrat"),
                      values = c("steelblue","darkorange")) +
  labs(title = "Words associated with Democrats and Republicans",
       x = "Words", y = "Log ratio")

ggsave("party_words.png")


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
  geom_col(show.legend = FALSE,size=0) + 
  scale_x_continuous(breaks = dem_rep_tf_idf$order, 
                     labels = dem_rep_tf_idf$word, 
                     expand = c(0,0)) + 
  facet_wrap(~party, scales = "free") +
  coord_flip() +
  scale_fill_manual(values=c("steelblue","darkorange")) +
  labs(title = "Words associated with Democrats and Republicans (TF-IDF)",
     x = "Words", y = "TF-IDF")

ggsave("party__words_tf_idf.png")
                    