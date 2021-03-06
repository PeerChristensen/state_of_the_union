# SOTU - Comparing sentiments
# june 2019
# Peer Christensen

library(tidyverse)
library(tidytext)
library(sentimentr)

df <- read_csv("data/state_of_the_union.csv")

theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 20,
                                            margin=margin(20,0,20,0)),
                  axis.title.x = element_text(size = 14,margin=margin(10,0,10,0)),
                  axis.title.y = element_text(size = 14,margin=margin(0,10,0,10)),
                  axis.text = element_text(size = 11,margin=margin(5,0,5,0)),
                  plot.margin = margin(10, 40, 10, 10)))

# sentiment by president - nrc minus pos,neg

df1 <- df %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(president,sentiment) %>%
  filter(!str_detect(sentiment, "pos|neg")) %>%
  group_by(president) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(proportion = n/sum_n) %>%
  ungroup()

df1 %>% 
  ggplot(aes(sentiment,proportion,fill=sentiment)) +
  geom_col() +
  facet_wrap(~president) +
  theme(axis.text.x  =element_blank(),
        axis.ticks.x =element_blank())

ggsave("plots/sentiments_nrc_presidents.png")

# sentiment by president - nrc: pos,neg
df2 <- df %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(president,sentiment) %>%
  filter(str_detect(sentiment, "pos|neg")) %>%
  group_by(president) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(proportion = n/sum_n) %>%
  ungroup()

df2 %>% 
  ggplot(aes(sentiment,proportion,fill=sentiment)) +
  geom_col() +
  facet_wrap(~president) +
  theme(axis.text.y  =element_blank(),
        axis.ticks.y =element_blank())

ggsave("plots/sentiments_pos_neg_presidents.png")

# sentiment score by president ordered by date - nrc: pos,neg

dfx <- df %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(president,date,document,sentiment) %>%
  filter(str_detect(sentiment, "pos|neg")) %>%
  group_by(document) %>%
  mutate(sum_n = sum(n)) %>%
  filter(sentiment == "positive") %>%
  mutate(prop_positive = n/sum_n) %>%
  ungroup()

dfx %>%
  arrange(date) %>%
  group_by(president) %>% 
  mutate(meanProp = mean(prop_positive)) %>%
  ggplot(aes(reorder(president,rev(date)),prop_positive)) +
  geom_hline(yintercept = mean(dfx$prop_positive), 
             color="darkgrey",linetype = "dashed") +
  geom_point(alpha= .7, size = 2, colour = "steelblue") +
  geom_point(aes(reorder(president,rev(date)),meanProp), 
             colour = "darkorange", size = 3, alpha = .3) +
  coord_flip() +
  labs(title = "Proportion of positive sentiments for each president",
       x = "Presidents", y = "Prop positive")

ggsave("plots/positivity_presidents.png")

# sentiment score by party - nrc: pos,neg

dfx2 <- df %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(party,document,sentiment) %>%
  filter(str_detect(sentiment, "pos|neg")) %>%
  group_by(document) %>%
  mutate(sum_n = sum(n)) %>%
  filter(sentiment == "positive") %>%
  mutate(prop_positive = n/sum_n) %>%
  ungroup()

# write file with feature for classification
export_prop_pos <- dfx2 %>%
  select(-sentiment,-n,-sum_n)

write_csv(export_prop_pos,"data/sotu_positive_sentiment_prop.csv")

# proportion positive ~ party

m <- lm(prop_positive~party,data=dfx2)
summary(m) # Republicans significantly more positive than Dems

dfx2 %>%
  ggplot(aes(party,prop_positive)) +
  geom_jitter(alpha= .7, size = 2, colour = "steelblue",width=.2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  labs(title = "Proportion of positive sentiments for each party",
       x = "Party", y = "Prop positive")

ggsave("plots/positivity_party.png")

# sentiment by party - nrc minus pos,neg

df3 <- df %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(party,sentiment) %>%
  filter(!str_detect(sentiment, "pos|neg")) %>%
  group_by(party) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(proportion = n/sum_n) %>%
  ungroup()

df3 %>% 
  ggplot(aes(sentiment,proportion,fill=sentiment)) +
  geom_col() +
  facet_wrap(~party) +
  theme(axis.text.x  =element_blank(),
        axis.ticks.x =element_blank()) 

ggsave("plots/sentiments_nrc_party.png")

# sentiment by party - nrc: pos,neg

df4 <- df %>% 
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(party,sentiment) %>%
  filter(str_detect(sentiment, "pos|neg")) %>%
  group_by(party) %>%
  mutate(sum_n = sum(n)) %>%
  mutate(proportion = n/sum_n) %>%
  ungroup()

df4 %>% 
  ggplot(aes(sentiment,proportion,fill=sentiment)) +
  geom_col() +
  facet_wrap(~party) +
  theme(axis.text.x  =element_blank(),
        axis.ticks.x =element_blank()) 

ggsave("plots/sentiments_pos_neg_party.png")

# sentiment words by party

df5 <- df %>%
  filter(party == "Republican" | party == "Democrat") %>%
  unnest_tokens(word,text) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(str_detect(sentiment, "pos|neg")) %>%
  filter(!word %in% c("government","united")) %>%
  count(party,sentiment,word) %>%
  spread(party, n, fill = 0) %>%
  filter(Democrat > 5,Republican > 5) %>%
  ungroup() %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(Republican / Democrat)) %>%
  group_by(sentiment, logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  arrange(sentiment, logratio) %>%
  mutate(order = row_number())

df5 %>%
  #mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(order, logratio, fill = logratio < 0)) +
  geom_col(size=0) +
  facet_wrap(~sentiment,scales = "free") +
  coord_flip()   +
  scale_x_continuous(breaks = df5$order, 
                     labels = df5$word, 
                      expand = c(0,0)) +
  scale_fill_manual(name = "Party", labels = c("Republican","Democrat"),
                    values = c("steelblue","darkorange")) +
  labs(title = "Sentiment words associated with Democrats and Republicans",
       x = "Words", y = "Log ratio")

ggsave("plots/party_sentiment_words.png")
