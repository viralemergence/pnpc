
library(tidyverse)
library(sf)

clo1 <- read_csv("~/Documents/Github/clover/clover/clover_1.0_allpathogens/CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")
clo2 <- read_csv("~/Documents/Github/clover/clover/clover_1.0_allpathogens/CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")
clo3 <- read_csv("~/Documents/Github/clover/clover/clover_1.0_allpathogens/CLOVER_1.0_Viruses_AssociationsFlatFile.csv")
clo <- bind_rows(clo1, clo2, clo3)

clo %>% filter(Host == 'homo sapiens') %>% pull(Pathogen) %>% unique() -> zoonoses
clo %>%
  filter(HostClass == 'mammalia') %>%
  filter(!(Host == 'homo sapiens')) %>%
  filter(Pathogen %in% zoonoses) %>%
  pull(Host) %>%
  unique() -> reservoirs

iucn <- read_sf("./Data/IUCN/MAMMALS.shp")
iucn %>%
  mutate(binomial = str_to_lower(binomial)) %>%
  filter(binomial %in% reservoirs) -> ranges
