#' DESCRIPTION: IUCN mammal reconciliation manual
#' AUTHOR: Cole Brookson, & Caroline Cummings
#' Last Update: 28 Ausgust 2024

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
#4 there is a script that reconciles VIRION to Upham in VERNA github--> find this
#5 match IUCN to Upham
#6 match Clover to Upham

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
#filter to mammals
iucn<-iucn %>% filter(class=="MAMMALIA")
vir<- vir %>% filter(HostClass=="mammalia")

#unique species in iucn dataset & VIRION
n_distinct(iucn$binomial) #5844
n_distinct(vir$Host) #1795

#make some new dataframes that are more manageable (original IUCN is slow)
iucn<- iucn$binomial 
iucn <- iucn %>% as.data.frame() 
iucn$binomial=iucn$.
iucn$.=NULL
iucn<- iucn %>% arrange(binomial) %>%
  mutate(binomial = tolower(binomial))
vir<- vir %>% select(Host) %>% arrange(Host) %>%
  mutate(Host = tolower(Host)) %>% unique()

#find differences
miss=setdiff(vir$Host,iucn$binomial) %>% as.data.frame %>% unique() #233
miss$species=miss$.
miss$.=NULL

#make VIRION match IUCN
#revalue=c("old tip"= "new tip")

#https://www.iucnredlist.org/
#https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi
#https://www.gbif.org/species/search

vir$iucn=vir$Host
vir$iucn= plyr::revalue(vir$iucn,
                        c("abrothrix hirta"="abrothrix longipilis",
                          "aeorestes cinereus"="lasiurus cinereus",
                          "aeorestes egregius"="lasiurus egregius",
                          "afronycteris nana"="neoromicia nana",
                          "alces americanus"="alces alces",
                          "alexandromys oeconomus"="microtus oeconomus",
                          "antrozous dubiaquercus"="bauerus dubiaquercus",
                          "aotus azarai"= "aotus azarae",
                         "apodemus chejuensis"="apodemus agrarius",
                         "apodemus ilex"="apodemus draco",
                         "artibeus cinereus"="dermanura cinerea",
                         "artibeus glaucus"="dermanura glauca",
                         "artibeus phaeotis"="dermanura phaeotis",
                         "artibeus toltecus"="dermanura tolteca",
                         "arvicanthis sp."= "", #recheck synonyms
                         "ateles sp."= "", #recheck synonyms
                         "bandicota sp."="", #recheck synonyms
                         "bolomys amoenus"="necromys amoenus",
                         "bos frontalis"="bos gaurus",
                         "bos grunniens"="bos mutus",
                         "bos indicus"= "", #recheck synonyms
                         "bos indicus x bos taurus"="",#recheck synonyms
                         "bos primigenius"="", #recheck synonyms
                         "bos taurus"="", #recheck synonyms
                         "bos taurus x bison bison"="",
                         "bubalus bubalis"="bubalus arnee",
                         "bubalus carabanensis"="bubalus arnee",
                         "bubalus sp."="", #recheck synonyms
                         "cacajao rubicundus"="cacajao calvus",
                         "callicebus sp."="", #recheck synonyms
                         "callithrix sp."="", #recheck synonyms
                         "calomys sp."="", #recheck synonyms
                         "camelus bactrianus"="camelus ferus",
                         "camelus dromedarius"="camelus ferus", 
                         "canis sp."="", #recheck synonyms
                         "capra hircus"="capra aegagrus",
                         "capra sp."="capra aegagrus", 
                         "capricornis milneedwardsii"="capricornis sumatraensis",
                         "cavia cutleri"="", #recheck synonyms
                         "cavia porcellus"="cavia aperea",
                         "cebus sp."="", #recheck synonyms
                         "cephalophorus rufilatus"="cephalophus rufilatus",
                         "cercopithecus albogularis"="cercopithecus mitis",
                         "cercopithecus doggetti"="cercopithecus mitis",
                         "cercopithecus kandti"="cercopithecus mitis",
                         "cervidae sp."= "", #recheck synonyms
                         "chaerephon leucogaster"="mops leucogaster",
                         "chaerephon pusillus"="mops pusillus",
                         "chiropotes sp."= "", #recheck synonyms
                         "clethrionomys gapperi"= "myodes gapperi",
                         "coendou rothschildi"="coendou quichua",
                         "cricetulus griseus"="cricetulus barabensis",
                         "crocidura dracula"= "crocidura fuliginosa",
                         "cynopterus sp."= "", #recheck synonyms
                         "dasypterus ega"="lasiurus ega",
                         "dasypterus xanthinus"="lasiurus xanthinus",
                         "delphinus capensis"="delphinus delphis",
                         "dipodillus dasyurus"="gerbillus dasyurus",
                         "dobsonia andersoni"="dobsonia anderseni",
                         "dorcopsis veterum"="dorcopsis muelleri",
                         "doryrhina cyclops"= "hipposideros cyclops",
                         "echinosciurus variegatoides"="sciurus variegatoides",
                         "eospalax rufescens"="eospalax smithii",
                         "eothenomys eleusis"="", #recheck synonyms
                         "eothenomys eva"="caryomys eva",
                         "eothenomys inez"="caryomys inez",
                         "epomophorus pusillus"="micropteropus pusillus",
                         "epomops sp."="", #recheck synonyms
                         "eptesicus regulus"="vespadelus regulus",
                         "eptesicus sp."="",#recheck synonyms
                         "eptesicus vulturnus"="vespadelus vulturnus",
                         "equus asinus"="equus africanus",
                         "equus asinus x caballus"="equus africanus",
                         "equus burchellii"="equus quagga",
                         "equus caballus"="equus ferus",
                         "equus caballus x equus asinus"="equus ferus",
                         "equus przewalskii"="equus ferus",
                         "equus sp."="",#recheck synonyms
                         "erinaceus sp."= "", #recheck synonyms
                         "felis catus"="felis silvestris",
                         "felis sp."="", #recheck synonyms
                         "galerella pulverulenta"="herpestes pulverulentus",
                         "galerella sanguinea"="herpestes sanguineus",
                         "gerbilliscus sp."= "", #recheck synonyms
                         "giraffa giraffa"="giraffa camelopardalis",
                         "giraffa reticulata"="giraffa camelopardalis",
                         "grammomys sp."= "", #recheck synonyms
                         "grammomys surdaster"="grammomys dolichurus",
                         "hesperosciurus griseus"="sciurus griseus",
                         "hexaprotodon liberiensis"="choeropsis liberiensis",
                         "hipposideros cf. ruber"="hipposideros ruber",
                         "hipposideros terasensis"="hipposideros armiger",
                         "homo sapiens"= "", #what should i do for humans?
                         "hsunycteris thomasi"="lonchophylla thomasi",
                         "hylobates sp."="",  #recheck synonyms
                         "hylomyscus simus"="hylomyscus alleni",
                         "inia boliviensis"="inia geoffrensis",
                         "laephotis capensis"="neoromicia capensis",
                         "lagothrix cana"="lagothrix lagothricha",
                         "lagothrix lagotricha"="lagothrix lagothricha",
                         "lagothrix lugens"="lagothrix lagothricha",
                         "lagothrix poeppigii"="lagothrix lagothricha",
                         "lama glama"="lama guanicoe",
                         "lasiopodomys gregalis"="microtus gregalis",
                         "lasiurus sp. (in: bats)"= "", #recheck synonyms
                         "liomys adspersus"="heteromys adspersus",
                         "liomys pictus"="heteromys pictus",
                         "liomys salvini"="heteromys salvini",
                         "lophuromys aquilus"="lophuromys flavopunctatus",
                         "lophuromys dudui"="lophuromys flavopunctatus",
                         "lophuromys laticeps"="lophuromys flavopunctatus",
                         "macaca balantak"="macaca tonkeana",
                         "macaca balantak x tonkeana"="macaca tonkeana",
                         "macaca leucogenys"="macaca sinica"
                        ))

#check
miss=setdiff(vir$iucn,iucn$binomial) #120


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
