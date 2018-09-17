

library(tidyverse)
library(rvest)

#df <- read_csv("state_of_the_union.csv")

#url <- read_html("https://en.wikipedia.org/wiki/List_of_Presidents_of_the_United_States")

url <- read_html("http://www.enchantedlearning.com/history/us/pres/list.shtml")

df  <- url %>% html_table(fill=T) %>% .[[9]] %>% select(President,Party)


write.csv2(df,"presidents_2.csv")
