# SOTU - Comparing readability and formality of parties and presidents
# create new data set 
# june 2019
# Peer Christensen

library(tidyverse)
library(tidytext)
library(udpipe)
library(readability)

theme_set(theme_minimal() +
            theme(plot.title = element_text(size = 20,
                                            margin=margin(20,0,20,0)),
                  axis.title.x = element_text(size = 14,margin=margin(10,0,10,0)),
                  axis.title.y = element_text(size = 14,margin=margin(0,10,0,10)),
                  axis.text = element_text(size = 11,margin=margin(5,0,5,0)),
                  plot.margin = margin(10, 40, 10, 10)))

# ------------ READABILITY ----------------------

df <- read_csv("data/state_of_the_union.csv")

read <- readability(df$text,list(df$party,df$president,df$document))

read <- read %>%
   select(-president, -party)

df <- df %>%
 left_join(read,by = "document")

Flesch_Kincaid scores for each president and document

df %>%
  arrange(date) %>%
  select(-text) %>%
  distinct() %>%
  group_by(president) %>% 
  mutate(meanFK = mean(Flesch_Kincaid)) %>%
  ggplot(aes(reorder(president,rev(date)),Flesch_Kincaid)) +
  geom_hline(yintercept = mean(df$Flesch_Kincaid), 
             color="darkgrey",linetype = "dashed") +
  geom_point(alpha= .7, size = 2, colour = "steelblue") +
  geom_point(aes(reorder(president,rev(date)),meanFK), 
             colour = "darkorange", size = 3, alpha = .3) +
  coord_flip() +
  labs(title = "Readability scores for each president", 
       x = "Presidents", y = "Flesch-Kincaid score") 

ggsave("plots/readability_presidents.png")

# Flesch_Kincaid scores by party

df %>%
  arrange(date) %>%
  select(-text) %>%
  distinct() %>%
  group_by(president) %>%  
  ggplot(aes(party,Flesch_Kincaid)) +
  geom_jitter(alpha= .7, size = 2, colour = "steelblue",width=.2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  labs(title = "Readability scores for each party", 
       x = "Party", y = "Flesch-Kincaid score") 

ggsave("plots/readability_party.png")

# are lesch_Kincaid scores significantly different for democrats and republicans?

df_dem_rep <- df %>% 
  filter(party == "Democrat"| party=="Republican") %>%
  mutate(date = as.numeric(date) + 65736,
         document = doc_id)

m_read <-lmer(Flesch_Kincaid ~ party + document + (1|president),data=df_dem_rep)
summary(m_read)

# ------------ FORMALITY ----------------------

df <- df %>%
  select(-text) %>%
  distinct()

form <- read_csv("SOTU_clean.csv")

pos_keep <- c("NOUN","ADJ","ADP","DET","PRON","VERB","AUX","ADV")

df <- form %>% 
  count(doc_id,upos) %>% 
  filter(upos %in% pos_keep) %>%
  mutate(F_class = ifelse(upos %in% pos_keep[1:4],"f","c")) %>%
  group_by(doc_id,F_class) %>%
  summarise(total = sum(n)) %>%
  spread(F_class, total) %>%
  mutate(total_all=f+c) %>%
  summarise(F_measure = 50*(((f-c)/(total_all))+1)) %>%
  left_join(df, by = c("doc_id" = "document"))

# Formality scores for each president and document

df %>%
  arrange(date) %>%
  group_by(president) %>% 
  mutate(meanF = mean(F_measure)) %>%
  ggplot(aes(reorder(president,rev(date)),F_measure)) +
  geom_hline(yintercept = mean(df$F_measure), 
             color="darkgrey",linetype = "dashed") +
  geom_point(alpha= .7, size = 2, colour = "steelblue") +
  geom_point(aes(reorder(president,rev(date)),meanF), 
             colour = "darkorange", size = 3, alpha = .3) +
  coord_flip() +
  labs(title = "Formality scores for each president", 
       x = "Presidents", y = "Formality score") 

ggsave("plots/formality_presidents.png")

# by party boxplot

df %>%
  ggplot(aes(party,F_measure)) +
  geom_jitter(alpha= .7, size = 2, colour = "steelblue",width=.2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  labs(title = "Formality scores for each party", 
       x = "Party", y = "Formality score") 

ggsave("plots/formality_party.png")

# are democrats and republicans significantly different?

m_form <-lmer(F_measure ~ party + doc_id + (1|president),data=df_dem_rep)
summary(m_form)

# ------------ SENTENCE LENGTH ------------------

df2 <- read_csv("data/state_of_the_union.csv")

df2 <- df2 %>% 
  group_by(document) %>% 
  mutate(full_text = paste(text, collapse = " ")) %>%
  select(-text) %>%
  distinct()

df2 <- df2 %>%
  mutate(n_stops = str_count(full_text,"\\.|!"),
         n_words = str_count(full_text,"\\W+")) %>%
  mutate(mean_sent_length_doc = n_words / n_stops)

# by president
df2 %>%
  arrange(date) %>%
  group_by(president) %>% 
  mutate(mean_sent_length_pres = mean(mean_sent_length_doc)) %>%
  ggplot(aes(reorder(president,rev(date)),mean_sent_length_doc)) +
  geom_hline(yintercept = mean(df2$mean_sent_length_doc), 
             color="darkgrey",linetype = "dashed") +
  geom_point(alpha= .7, size = 2, colour = "steelblue") +
  geom_point(aes(reorder(president,rev(date)),mean_sent_length_pres), 
             colour = "darkorange", size = 3, alpha = .3) +
  coord_flip() +
  labs(title = "Mean sentence lengths for each president", 
       x = "Presidents", y = "Mean sentence lengths") 

ggsave("plots/sentence_length_presidents.png")

# by party boxplot

df2 %>%
  ggplot(aes(party,mean_sent_length_doc)) +
  geom_jitter(alpha= .7, size = 2, colour = "steelblue",width=.2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  labs(title = "Mean sentence lengths for each party", 
       x = "Party", y = "Mean sentence lengths") 

ggsave("plots/sentence_length_party.png")

# Are democrats and republicans significantly different?

df_dem_rep2 <- df2 %>% 
  filter(party == "Democrat"| party=="Republican") %>%
  mutate(date = as.numeric(date) + 65736)

m_sent_len <-lmer(mean_sent_length_doc ~ party + document + (1|president),data=df_dem_rep2)
summary(m_sent_len)

# ------------ DOC LENGTH ------------------

df2 %>%
  arrange(date) %>%
  group_by(president) %>% 
  mutate(mean_n_words_pres = mean(n_words)) %>%
  ggplot(aes(reorder(president,rev(date)),n_words)) +
  geom_hline(yintercept = mean(df2$n_words), 
             color="darkgrey",linetype = "dashed") +
  geom_point(alpha= .7, size = 2, colour = "steelblue") +
  geom_point(aes(reorder(president,rev(date)),mean_n_words_pres), 
             colour = "darkorange", size = 3, alpha = .3) +
  coord_flip() +
  labs(title = "Document lengths for each president", 
       x = "President", y = "Document lengths / Word counts") 

ggsave("plots/doc_length_presidents.png")

# by party boxplot

df2 %>%
  ggplot(aes(party,n_words)) +
  geom_jitter(alpha= .7, size = 2, colour = "steelblue",width=.2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  labs(title = "Document lengths for each party", 
       x = "Party", y = "Document lengths / Word counts") 

ggsave("plots/doc_length_party.png")

# Are democrats and republicans significantly different?

m_doc_len <-lmer(n_words ~ party + document + (1|president),data=df_dem_rep2)
summary(m_doc_len)

# ------------ OUTPUT ---------------------------

df <- df %>%
  add_column(n_words = df2$n_words,
             mean_sent_length = df2$mean_sent_length_doc) %>%
  select(doc_id, party, president, date, everything())
  
write_csv(df,"data/sotu_w_style_measures.csv")
