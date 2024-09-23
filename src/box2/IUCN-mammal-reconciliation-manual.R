#' DESCRIPTION: IUCN mammal reconciliation manual
#' AUTHOR: Cole Brookson, & Caroline Cummings
#' Last Update: 23 September 2024

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
                         "arvicanthis sp."= "", 
                         "ateles sp."= "", 
                         "bandicota sp."="", 
                         "bolomys amoenus"="necromys amoenus",
                         "bos frontalis"="bos gaurus",
                         "bos grunniens"="bos mutus",
                         "bos indicus"= "", #recheck synonyms
                         "bos indicus x bos taurus"="",#recheck synonyms
                         "bos primigenius"="", #recheck synonyms
                         "bos sp."="",
                         "bos taurus"="", #recheck synonyms
                         "bos taurus x bison bison"="",
                         "bubalus bubalis"="bubalus arnee",
                         "bubalus carabanensis"="bubalus arnee",
                         "bubalus sp."="", 
                         "cacajao rubicundus"="cacajao calvus",
                         "callicebus sp."="", 
                         "callithrix sp."="", 
                         "calomys sp."="", 
                         "camelus bactrianus"="camelus ferus",
                         "camelus dromedarius"="camelus ferus", 
                         "canis sp."="", 
                         "capra hircus"="capra aegagrus",
                         "capra sp."="capra aegagrus", 
                         "capricornis milneedwardsii"="capricornis sumatraensis",
                         "cavia cutleri"="", #recheck synonyms
                         "cavia porcellus"="cavia aperea",
                         "cebus sp."="", 
                         "cephalophorus rufilatus"="cephalophus rufilatus",
                         "cercopithecus albogularis"="cercopithecus mitis",
                         "cercopithecus doggetti"="cercopithecus mitis",
                         "cercopithecus kandti"="cercopithecus mitis",
                         "cervidae sp."= "", 
                         "chaerephon leucogaster"="mops leucogaster",
                         "chaerephon pusillus"="mops pusillus",
                         "chiropotes sp."= "", 
                         "clethrionomys gapperi"= "myodes gapperi",
                         "coendou rothschildi"="coendou quichua",
                         "cricetulus griseus"="cricetulus barabensis",
                         "crocidura dracula"= "crocidura fuliginosa",
                         "cynopterus sp."= "", 
                         "dasypterus ega"="lasiurus ega",
                          "dasypterus intermedius"="lasiurus intermedius",
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
                         "epomops sp."="", 
                         "eptesicus regulus"="vespadelus regulus",
                         "eptesicus sp."="",
                         "eptesicus vulturnus"="vespadelus vulturnus",
                         "equus asinus"="equus africanus",
                         "equus asinus x equus caballus"="equus africanus",
                         "equus asinus x caballus"="equus africanus",
                         "equus burchellii"="equus quagga",
                         "equus caballus"="equus ferus",
                         "equus caballus x equus asinus"="equus ferus",
                         "equus przewalskii"="equus ferus",
                         "equus sp."="",
                         "erinaceus sp."= "", 
                         "felis catus"="felis silvestris",
                         "felis sp."="", 
                         "galerella pulverulenta"="herpestes pulverulentus",
                         "galerella sanguinea"="herpestes sanguineus",
                         "gerbilliscus sp."= "", 
                         "giraffa giraffa"="giraffa camelopardalis",
                         "giraffa reticulata"="giraffa camelopardalis",
                         "grammomys sp."= "", 
                         "grammomys surdaster"="grammomys dolichurus",
                         "hesperosciurus griseus"="sciurus griseus",
                         "hexaprotodon liberiensis"="choeropsis liberiensis",
                         "hipposideros cf. ruber"="hipposideros ruber",
                         "hipposideros terasensis"="hipposideros armiger",
                         "homo sapiens"= "", #what should i do for humans?
                         "hsunycteris thomasi"="lonchophylla thomasi",
                         "hylobates sp."="",  
                         "hylomyscus simus"="hylomyscus alleni",
                         "inia boliviensis"="inia geoffrensis",
                         "laephotis capensis"="neoromicia capensis",
                         "lagothrix cana"="lagothrix lagothricha",
                         "lagothrix lagotricha"="lagothrix lagothricha",
                         "lagothrix lugens"="lagothrix lagothricha",
                         "lagothrix poeppigii"="lagothrix lagothricha",
                         "lama glama"="lama guanicoe",
                         "lasiopodomys gregalis"="microtus gregalis",
                         "lasiurus sp. (in: bats)"= "", 
                         "liomys adspersus"="heteromys adspersus",
                         "liomys pictus"="heteromys pictus",
                         "liomys salvini"="heteromys salvini",
                         "lophuromys aquilus"="lophuromys flavopunctatus",
                         "lophuromys dudui"="lophuromys flavopunctatus",
                         "lophuromys laticeps"="lophuromys flavopunctatus",
                         "macaca balantak"="macaca tonkeana",
                         "macaca balantak x tonkeana"="macaca tonkeana",
                         "macaca leucogenys"="macaca sinica",
                         "macaca sp."= "", 
                         "macaca speciosa"="macaca arctoides",
                         "macronycteris vittata"="macronycteris vittatus",
                         "macropus sp."="", 
                         "marmosets"= "", 
                         "mastomys sp."= "", 
                         "mazama gouazoupira"="mazama gouazoubira",
                         "megaderma lyra"="lyroderma lyra",
                         "microtus obscurus"="microtus arvalis",
                        "microtus rossiaemeridionalis"="microtus levis",
                        "miniopterus fuliginosus"="miniopterus schreibersii",
                        "miniopterus mossambicus"= "miniopterus minor",
                        "miniopterus orianae"= "miniopterus schreibersii",
                        "molossus ater"="molossus rufus",
                        "mops pumilus"="chaerephon pumilus",
                        "mus sp."= "", 
                        "mustela eversmannii"="mustela eversmanii",
                        "mustela sp."= "",
                        "myotis cf. californicus/ciliolabrum bl-2021"= "myotis californicus", #Colin preference? which has more traits known
                        "myotis crypticus"="myotis nattereri",
                        "myotis flavus"="myotis formosus",
                        "myotis myotis/blythii"="myotis myotis", #Colin preference? which has more traits known
                        "myotis oxygnathus"="myotis blythii",
                        "myotis ricketti"= "myotis pilosus",
                        "myotis sp."="", 
                        "myotomys unisulcatus"="otomys unisulcatus",
                        "nannospalax galili"="nannospalax ehrenbergi",
                        "neodon clarkei"="microtus clarkei",
                        "neodon fuscus"="lasiopodomys fuscus",
                        "neodon leucurus"="phaiomys leucurus",
                        "neogale vison"="neovison vison",
                        "neoromicia brunneus"="neoromicia brunnea",
                        "neoromicia somalicus"="neoromicia malagasyensis",
                        "nesogale dobsoni"="microgale dobsoni",
                        "niumbaha superba"="glauconycteris superba",
                        "niviventer huang"="niviventer fulvescens",
                        "niviventer lotipes"="niviventer confucianus",
                        "niviventer sp."="", 
                        "notamacropus agilis"="macropus agilis",
                        "notamacropus dorsalis"="macropus dorsalis",
                        "notamacropus eugenii"="macropus eugenii",
                        "notamacropus parma"="macropus parma",
                        "notamacropus parryi"="macropus parryi",
                        "notamacropus rufogriseus"="macropus rufogriseus",
                        "nyctalus velutinus"="nyctalus noctula",
                        "ochotona sp."= "", 
                        "oligoryzomys costaricensis"="oligoryzomys fulvescens",
                        "oligoryzomys mattogrossae"="oligoryzomys microtis",
                        "oligoryzomys sp."="", 
                        "oligoryzomys sp. rt-2012"="", 
                        "oligoryzomys utiaritensis"="oligoryzomys eliurus",
                        "orientallactaga sibirica"="allactaga sibirica",
                        "oryzomys sp."="", 
                        "oryzomys texensis"="oryzomys palustris",
                        "osphranter robustus"="macropus robustus",
                        "osphranter rufus"="macropus rufus",
                        "ovis aries"="", #drop domesticated animals
                        "ovis orientalis"="ovis gmelini",
                        "oxymycterus judex"="oxymycterus quaestor",
                        "pantherina griselda"="blarinella griselda",
                        "papio sp."="",
                        "parahypsugo crassulus"="pipistrellus crassulus",
                        "pekania pennanti"="martes pennanti",
                        "peromyscus sp."="",
                        "phoca fasciata"="histriophoca fasciata",
                        "phoca groenlandica"="pagophilus groenlandicus",
                        "physeter catodon"="physeter macrocephalus",
                        "plecotus gaisleri"="plecotus teneriffae",
                        "prionailurus iriomotensis"="prionailurus bengalensis",
                        "proechimys sp."="",
                        "ptenochirus jagorii"="ptenochirus jagori",
                        "pteronotus alitonus"= "pteronotus rubiginosus",
                        "pteropus sp."="",
                        "puma yagouaroundi"="herpailurus yagouaroundi",
                        "rattus flavipectus"="rattus tanezumi",
                        "rattus sp."="",
                        "rhinolophus blythi"="rhinolophus pusillus",
                        "rhinolophus cornutus"="rhinolophus pusillus",
                        "rhinolophus hildebrandti"="rhinolophus hildebrandtii",
                        "rhinolophus lobatus"="rhinolophus landeri",
                        "rhinolophus monoceros"= "rhinolophus pusillus",
                        "rhinolophus rhodesiae"= "rhinolophus swinnyi",
                        "rhinolophus sp. cn 2016"="",
                        "rodentia sp."="",
                        "rousettus sp."="",
                        "saccostomus sp."="",
                        "saimiri sp."="",
                        "sapajus sp."="",
                        "scarturus elater"="allactaga elater",
                        "sorex monticolus"="sorex monticola",
                        "steatomys sp."="",
                         "sus sp."="",
                         "sylvilagus sp."="",
                         "syntheosciurus granatensis"="sciurus granatensis",
                         "taeromys dominator"="paruromys dominator",
                          "talpa aquitania"= "talpa occidentalis",
                          "tamias amoenus"="neotamias amoenus",
                          "tamias minimus"="neotamias minimus",
                          "tamias quadrivittatus"="neotamias quadrivittatus",
                          "tamias sibiricus"="eutamias sibiricus",
                          "tamias umbrinus"="neotamias umbrinus",
                           "taphozous sp."="",
                           "terricola subterraneus"="microtus subterraneus",
                           "thryonomys sp."="",
                            "triaenops menamena"="triaenops rufus",
                            "tupaia chinensis"="tupaia belangeri",
                            "undetermined sciuridae 'chipmunks'"="",
                            "uropsilus sp."="",
                            "ursus sp."="",
                            "urva auropunctata"="herpestes auropunctatus",
                            "urva edwardsii"="herpestes edwardsii",
                            "urva javanica"="herpestes javanicus",
                            "vicugna pacos"="lama guanicoe",
                            "xanthonycticebus pygmaeus"="nycticebus pygmaeus",
                            "zygodontomys cherriei"="zygodontomys brevicauda"
                       ))

#check
miss=setdiff(vir$iucn,iucn$binomial) #2!

#3
#save csv
setwd("~/Desktop/GitHub/pnpc/src/box2")
#write_csv(vir,"IUCN reconciliation manual.csv")

#4 
#match IUCN/VIRION to Upham


#5
#match IUCN and Upham
#load Upham phylogeny
#setwd("~/OneDrive - University of Oklahoma/Becker Lab/Datasets/Upham phylo") 
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
