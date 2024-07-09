#' DESCRIPTION: in-text information pulled from Jones et al. (2008).
#' AUTHOR: Colin Carlson
#' DATE: 08 July 2024


library(tidyverse)
jo <- read_csv("./data/recreation/data-from-jones-2008.csv")

table(jo$PathType) %>% prop.table() %>% round(3)

# "1" is zoonotic; "0" is non-zoonotic
table(jo$TranType) %>% prop.table() %>% round(3)

# "2" is wildlife; "1" is non-wildlife and "3" is unspecified I think?
# Wish there had been a legend on the table that's been cited 10,000 times!
jo %>% filter(TranType == 1) %>% pull(ZooType) %>% table() %>% prop.table()
 %>% round(3)

# What if you exclude antimicrobial resistance?
jo %>% pull(DrugRes) %>% table() 
jo %>% pull(DrugRes) %>% table() %>% prop.table()

jo %>% filter(!DrugRes==1,
              !str_detect(`Pathogen responsible for each EID event`, '-res'))
               %>% View()
  pull(TranType) %>% table() # %>% prop.table()

jo %>% filter(!DrugRes==1,
              !str_detect(`Pathogen responsible for each EID event`, '-res')) 
              %>%
  filter(TranType==1) %>% pull(ZooType) %>% table() %>% prop.table()

# What if you limit it to viruses?

jo %>% filter(!DrugRes==1,
              !str_detect(`Pathogen responsible for each EID event`, '-res')) 
              %>% 
  filter(PathType == 'virus') %>%
  pull(TranType) %>% table()

jo %>% filter(!DrugRes==1,
              !str_detect(`Pathogen responsible for each EID event`, '-res'))
               %>% 
  filter(PathType == 'virus') %>%
  pull(ZooType) %>% table()
