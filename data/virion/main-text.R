
library(tidyverse)
library(vroom)

vir <- vroom("./data/virion/virion-zipped.csv.gz")

# How many pathogens do humans and animals share?

vir %>% filter(Host == 'homo sapiens') %>% pull(Virus) %>% unique() -> humanviruses
vir %>% filter(!(Host == 'homo sapiens'), Virus %in% humanviruses) %>% pull(Virus) %>% unique() -> zoonoses

length(zoonoses)
length(humanviruses)

# How many viruses are in bats?

vir %>% filter(HostOrder == 'chiroptera') %>% pull(Virus) %>% unique() -> batviruses
vir %>% filter(HostClass == 'mammalia') %>% pull(Virus) %>% unique() -> mammalviruses

length(batviruses)/length(mammalviruses)
1400/6400

vir %>% filter(HostOrder == 'rodentia') %>% pull(Virus) %>% unique() -> rodentviruses
length(rodentviruses)/length(mammalviruses)
2276/6400
