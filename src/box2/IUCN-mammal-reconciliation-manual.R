#' DESCRIPTION: IUCN mammal reconciliation manual
#' AUTHOR: Cole Brookson, & Caroline Cummings
#' Last Update: 23 June 2024

# set up =======================================================================

## clean environment & plots
rm(list=ls()) 
graphics.off()
gc()

#load in packages
library(tidyverse)
library(sf)
library(vroom)
library(fasterize)
library(patchwork)
library(MetBrewer)

#order of operations
#1 load in IUCN data and VIRION to make sure I use the correct IUCN
#2 match VIRION to IUCN 
#3 save a csv of original VIRION names with a column for IUCN  match (empty rn)

#4 match Virion/IUCN to Upham?
#5 match IUCN to Upham?
#6 match Clover to Upham?

#1
#load in IUCN data and VIRION
#source(here::here("./src/global-funs.R"))
#source(here::here("./src/fig1/dep/00_raster_funs.R"))
source(here::here("Desktop/GitHub/pnpc/src/global-funs.R"))
source(here::here("Desktop/GitHub/pnpc/src/fig1/dep/00_raster_funs.R"))

#vir <- vroom::vroom(here::here("./data/virion/virion-zipped.csv.gz"))
vir <- vroom::vroom(here::here("Desktop/GitHub/pnpc/data/virion/virion-zipped.csv.gz"))

# need to make sure we're all using the same IUCN
tryCatch(
  {
    iucn <- sf::read_sf(here::here("./data/IUCN/"))
  },
  warning = function(w) {
    print() # dummy warning function to suppress the output of warnings
  },
  error = function(err) {
    print("Could not read data from current directory, attempting download...")
    tryCatch({
      source(here::here("./src/iucn-download.R"))
    })
    warning <- function(w) {
      print() # dummy warning function to suppress the output of warnings
    }
  }
)

#2
#unique species in iucn dataset & VIRION
n_distinct(iucn$binomial) #5844
n_distinct(vir$Host) #4103

#make some new dataframes that are more manageable
iucn<- iucn %>% select(binomial) %>% arrange(binomial) %>%
  mutate(binomial = tolower(binomial))
vir<- vir %>% select(Host) %>% arrange(Host) %>%
  mutate(Host = tolower(Host)) %>% unique()

#find differences
miss=setdiff(vir$Host,iucn$binomial) %>% as.data.frame %>% unique() #2,534
miss$species=miss$.
miss$.=NULL

#make VIRION match IUCN
#revalue=c("old tip"= "new tip")
vir$iucn=vir$Host
vir$iucn= plyr::revalue(vir$iucn,
                        c("abramis brama"="",
                          "abrothrix hirta"="abrothrix longipilis"))

#progreess
#abramis brama -- not found, try again tmrw


#3
#save csv


#4 
#match IUCN/VIRION to Upham


#5
#match IUCN and Upham
#load Upham phylogeny
#Caroline, get the One drive to work!!!!! or ask cole for an alterative idea
#setwd("~/OneDrive - University of Oklahoma/Becker Lab/Datasets/Upham phylo") #this doesn't work for me still... on my list to try again
setwd("~/Desktop/GitHub/WBC-BRT/phylo")
tree=read.nexus('MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_NDexp_MCC_v2_target.tre')

## load in taxonomy
taxa=read.csv('taxonomy_mamPhy_5911species.csv',header=T)
taxa$tip=taxa$Species_Name

## fix tips on phylogeny and taxa 
tree$tip.label=sapply(strsplit(tree$tip.label,'_'),function(x) paste(x[1],x[2],sep=' '))
taxa$tip=sapply(strsplit(taxa$tip,'_'),function(x) paste(x[1],x[2],sep=' '))


#6
#match Clover and Upham
