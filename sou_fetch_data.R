# Download and clean State of the Union addresses
# PEER CHRISTENSEN
# SEPTEMBER 2018

# -----------------------------------------------
# LOAD PACKAGES

library(tidytext)
library(gutenbergr)
library(tidyverse)
library(magrittr)
library(lubridate)

# -----------------------------------------------
# DOWNLOAD DATA

df <- gutenberg_download(5050) %>% 
  select(-gutenberg_id)

# -----------------------------------------------
# CLEAN & ADD VARIABLES

# DOCUMENT NUMBER

df %<>% 
  mutate(document  = cumsum(str_detect(text, regex("\\*\\*\\*"))) -1) %>%
  filter(document != 0, document != max(document))

# -----------------------------------------------
# PRESIDENT

pres_indices  <- grep("State of the Union Address", df$text) + 1
presidents    <- df[pres_indices,]
presidents    <- dplyr::rename(presidents,president = text)

df %<>% left_join(presidents, by = "document")

# -----------------------------------------------
# DATE 

date_indices <- grep("State of the Union Address", df$text) + 2
dates        <- df[date_indices,]
dates        <- dplyr::rename(dates, date = text) %>% mutate(date = mdy(date))

df %<>% left_join(dates, by = c("document","president"))

# -----------------------------------------------
# REMOVE TEXT INTROS

df                         %<>% 
  group_by(document)       %>% 
  filter(row_number() > 6) %>%
  ungroup()

# -----------------------------------------------
# ADD PARTY INFO

party <- read_csv2("presidents_1.csv")

df <- left_join(df,party) %>% 
  filter(!is.na(party))
# -----------------------------------------------
# EXPORT DATA FILE

write_csv(df, "state_of_the_union.csv")






