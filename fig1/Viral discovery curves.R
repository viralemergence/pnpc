
library(tidyverse)
library(vroom)
 
vir <- vroom("Documents/Github/virion/virion/virion.csv.gz")

vir %>%
  #filter(ICTVRatified == 'TRUE') %>%
  rowwise() %>%
  mutate(year = min(PublicationYear, ReleaseYear, na.rm = TRUE)) -> vir

vir$year[vir$year==Inf] <- NA

vir %>% 
  filter(Host == 'homo sapiens') %>%
  pull(Virus) %>% unique() -> zoos

vir %>% 
  mutate(zoo = as.numeric(Virus %in% zoos)) %>%
  group_by(Virus) %>%
  summarize(zoo = max(zoo), year = min(year)) %>%
  filter(!is.na(year)) -> vir

vir %>%
  group_by(zoo) %>%
  count(year) %>%
  complete(year = 1955:2020, fill = list(n = 0)) %>%
  mutate(tally = cumsum(n)) %>% 
  ggplot(aes(x = year, y = tally, group = zoo, fill = zoo)) + 
  geom_area() + 
  theme_bw() + xlim(1990, 2020)
 