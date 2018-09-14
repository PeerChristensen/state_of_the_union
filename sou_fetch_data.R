# Download and clean State of the Union addresses

library(tidytext)
library(gutenbergr)
library(tidyverse)
library(magrittr)
library(lubridate)

df <- gutenberg_download(5050) %>% 
  select(-gutenberg_id)

df %<>% 
  mutate(document = cumsum(str_detect(text, regex("\\*\\*\\*"))) -1) %>%
  filter(document != 0, document != max(document))

pres_indices <- grep("State of the Union Address", df$text) + 1
presidents    <- df[pres_indices,]
presidents <- rename(presidents,president = text)

df %<>% left_join(presidents, by = "document")

date_indices <- grep("State of the Union Address", df$text) + 2
dates <- df[date_indices,]
dates <- rename(dates, date = text) %>%
  mutate(date = mdy(date))

df %<>% left_join(dates, by = c("document","president"))


