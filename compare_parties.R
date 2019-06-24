# SOTU - Comparing readability and formality of parties and presidents
# june 2019
# Peer Christensen

library(tidyverse)
library(tidytext)
library(udpipe)
library(readability)

theme_set(theme_minimal)

# ------------ READABILITY ----------------------

df <- read_csv("state_of_the_union.csv")

read <- readability(df$text,list(df$party,df$president,df$document))

read <- read %>%
   select(-president, -party)
   
df <- df %>% 
  left_join(read,by = "document") 

# Flesch_Kincaid scores for each president and document

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
  coord_flip()

# Flesch_Kincaid scores by party

df %>%
  ggplot(aes(party,Flesch_Kincaid)) +
  geom_point(colour = "steelblue", alpha = .6, size = 2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  theme_minimal()

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
  theme_minimal()

# by party boxplot

df %>%
  ggplot(aes(party,F_measure)) +
  geom_point(colour = "steelblue", alpha = .6, size = 2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  theme_minimal()

# are democrats and republicans significantly different?

m_form <-lmer(F_measure ~ party + doc_id + (1|president),data=df_dem_rep)
summary(m_form)

# ------------ SENTENCE LENGTH ------------------

df2 <- read_csv("state_of_the_union.csv")

df2 <- df2 %>% 
  group_by(document) %>% 
  mutate(full_text = paste(text, collapse = " ")) %>%
  select(-text) %>%
  distinct()

df2 <- df2 %>%
  mutate(n_stops = str_count(full_text,"\\.|!"),
         n_words = str_count(full_text,"\\W+")) %>%
  mutate(mean_sent_length_doc = n_words / n_stops)

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
  theme_minimal()

# by party boxplot

df2 %>%
  ggplot(aes(party,mean_sent_length_doc)) +
  geom_point(colour = "steelblue", alpha = .6, size = 2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  theme_minimal()

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
  theme_minimal()

# by party boxplot

df2 %>%
  ggplot(aes(party,n_words)) +
  geom_point(colour = "steelblue", alpha = .6, size = 2) +
  geom_boxplot(alpha=0,colour = "darkorange") +
  theme_minimal()

# Are democrats and republicans significantly different?

m_doc_len <-lmer(n_words ~ party + document + (1|president),data=df_dem_rep2)
summary(m_doc_len)

# ------------ OUTPUT ---------------------------

df <- df %>%
  add_column(n_words = df2$n_words,
             mean_sent_length = df2$mean_sent_length_doc) %>%
  select(doc_id, party, president, date, everything())
  



