#' DESCRIPTION: figure for the third box
#' AUTHOR: Daniel Becker, Cole Brookson, & Caroline Cummings
#' DATE: 19 June 2024

# set up =======================================================================

library(tidyverse)
library(sf)
library(vroom)
library(fasterize)
library(patchwork)
library(MetBrewer)

source(here::here("./src/global-funs.R"))
source(here::here("./src/fig1/dep/00_raster_funs.R"))

vir <- vroom::vroom(here::here("./data/virion/virion-zipped.csv.gz"))
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

# IUCN data ====================================================================

# match virion to iucn
vir <- vir[!is.na(vir$Host), ]
vir <- vir[vir$HostClass == "mammalia", ]
vir <- vir[!is.na(vir$HostClass), ]
vir$iucn <- vir$Host
iucn$binomial <- stringr::str_to_lower(iucn$binomial)

# trim sp.
vir$trim <- ifelse(grepl("sp\\.", vir$iucn), 1, 0)
vir <- vir[!vir$trim == 1, ]

# trim other
vir$trim <- ifelse(vir$iucn %in% c(
  "marmosets",
  "undetermined sciuridae 'chipmunks'"
), 1, 0)
vir <- vir[!vir$trim == 1, ]

# trim domestics and humans in trim
vir$trim <- ifelse(vir$iucn %in% c(
  "bos frontalis",
  "bos grunniens",
  "bos indicus",
  "bos indicus x bos taurus",
  "bos primigenius",
  "bos taurus",
  "bos taurus x bison bison",
  "bubalus bubalis",
  "bubalus carabanensis",
  "camelus bactrianus",
  "camelus dromedarius",
  "capra hircus",
  "cavia porcellus",
  "equus asinus",
  "equus asinus x caballus",
  "equus asinus x equus caballus",
  "equus caballus",
  "equus caballus x equus asinus",
  "felis catus",
  "homo sapiens",
  "lama glama",
  "ovis aries",
  "vicugna pacos"
), 1, 0)
vir <- vir[!vir$trim == 1, ]
vir$trim <- NULL

# manual match
miss <- setdiff(vir$iucn, iucn$binomial)
vir$iucn <- plyr::revalue(
  vir$iucn,
  c(
    "abrothrix hirta" = "abrothrix longipilis",
    "aeorestes cinereus" = "lasiurus cinereus",
    "aeorestes egregius" = "lasiurus egregius",
    "afronycteris nana" = "neoromicia nana",
    "alces americanus" = "alces alces",
    "alexandromys oeconomus" = "microtus oeconomus",
    "antrozous dubiaquercus" = "bauerus dubiaquercus",
    "aotus azarai" = "aotus azarae",
    "apodemus chejuensis" = "apodemus agrarius",
    "apodemus ilex" = "apodemus draco",
    "artibeus cinereus" = "dermanura cinerea",
    "artibeus glaucus" = "dermanura glauca",
    "artibeus phaeotis" = "dermanura phaeotis",
    "artibeus toltecus" = "dermanura tolteca",
    "bolomys amoenus" = "necromys amoenus",
    "cacajao rubicundus" = "cacajao calvus",
    "capricornis milneedwardsii" = "capricornis sumatraensis",
    "cavia cutleri" = "cavia tschudii",
    "cephalophorus rufilatus" = "cephalophus rufilatus",
    "cercopithecus albogularis" = "cercopithecus mitis",
    "cercopithecus doggetti" = "cercopithecus mitis",
    "cercopithecus kandti" = "cercopithecus mitis",
    "chaerephon leucogaster" = "mops leucogaster",
    "chaerephon pusillus" = "mops pusillus",
    "clethrionomys gapperi" = "myodes gapperi",
    "coendou rothschildi" = "coendou quichua",
    "cricetulus griseus" = "cricetulus barabensis",
    "crocidura dracula" = "crocidura fuliginosa",
    "dasypterus ega" = "lasiurus ega",
    "dasypterus intermedius" = "lasiurus intermedius",
    "dasypterus xanthinus" = "lasiurus xanthinus",
    "delphinus capensis" = "delphinus delphis",
    "dipodillus dasyurus" = "gerbillus dasyurus",
    "dobsonia andersoni" = "dobsonia anderseni",
    "dorcopsis veterum" = "dorcopsis muelleri",
    "doryrhina cyclops" = "hipposideros cyclops",
    "echinosciurus variegatoides" = "sciurus variegatoides",
    "eospalax rufescens" = "eospalax smithii",
    "eothenomys eleusis" = "eothenomys melanogaster",
    "eothenomys eva" = "caryomys eva",
    "eothenomys inez" = "caryomys inez",
    "epomophorus pusillus" = "micropteropus pusillus",
    "eptesicus regulus" = "vespadelus regulus",
    "eptesicus vulturnus" = "vespadelus vulturnus",
    "equus burchellii" = "equus quagga",
    "equus przewalskii" = "equus ferus",
    "galerella pulverulenta" = "herpestes pulverulentus",
    "galerella sanguinea" = "herpestes sanguineus",
    "giraffa giraffa" = "giraffa camelopardalis",
    "giraffa reticulata" = "giraffa camelopardalis",
    "grammomys surdaster" = "grammomys dolichurus",
    "hesperosciurus griseus" = "sciurus griseus",
    "hexaprotodon liberiensis" = "choeropsis liberiensis",
    "hipposideros cf. ruber" = "hipposideros ruber",
    "hipposideros terasensis" = "hipposideros armiger",
    "hsunycteris thomasi" = "lonchophylla thomasi",
    "hylomyscus simus" = "hylomyscus alleni",
    "inia boliviensis" = "inia geoffrensis",
    "laephotis capensis" = "neoromicia capensis",
    "lagothrix cana" = "lagothrix lagothricha",
    "lagothrix lagotricha" = "lagothrix lagothricha",
    "lagothrix lugens" = "lagothrix lagothricha",
    "lagothrix poeppigii" = "lagothrix lagothricha",
    "lasiopodomys gregalis" = "microtus gregalis",
    "liomys adspersus" = "heteromys adspersus",
    "liomys pictus" = "heteromys pictus",
    "liomys salvini" = "heteromys salvini",
    "lophuromys aquilus" = "lophuromys flavopunctatus",
    "lophuromys dudui" = "lophuromys flavopunctatus",
    "lophuromys laticeps" = "lophuromys flavopunctatus",
    "macaca balantak" = "macaca tonkeana",
    "macaca balantak x tonkeana" = "macaca tonkeana",
    "macaca leucogenys" = "macaca sinica",
    "macaca speciosa" = "macaca arctoides",
    "macronycteris vittata" = "macronycteris vittatus",
    "mazama gouazoupira" = "mazama gouazoubira",
    "megaderma lyra" = "lyroderma lyra",
    "microtus obscurus" = "microtus arvalis",
    "microtus rossiaemeridionalis" = "microtus levis",
    "miniopterus fuliginosus" = "miniopterus schreibersii",
    "miniopterus mossambicus" = "miniopterus minor",
    "miniopterus orianae" = "miniopterus schreibersii",
    "molossus ater" = "molossus rufus",
    "mops pumilus" = "chaerephon pumilus",
    "mustela eversmannii" = "mustela eversmanii",
    "myotis cf. californicus/ciliolabrum bl-2021" = "myotis californicus",
    "myotis crypticus" = "myotis nattereri",
    "myotis flavus" = "myotis formosus",
    "myotis myotis/blythii" = "myotis myotis",
    "myotis oxygnathus" = "myotis blythii",
    "myotis ricketti" = "myotis pilosus",
    "myotomys unisulcatus" = "otomys unisulcatus",
    "nannospalax galili" = "nannospalax ehrenbergi",
    "neodon clarkei" = "microtus clarkei",
    "neodon fuscus" = "lasiopodomys fuscus",
    "neodon leucurus" = "phaiomys leucurus",
    "neogale vison" = "neovison vison",
    "neoromicia brunneus" = "neoromicia brunnea",
    "neoromicia somalicus" = "neoromicia malagasyensis",
    "nesogale dobsoni" = "microgale dobsoni",
    "niumbaha superba" = "glauconycteris superba",
    "niviventer huang" = "niviventer fulvescens",
    "niviventer lotipes" = "niviventer confucianus",
    "notamacropus agilis" = "macropus agilis",
    "notamacropus dorsalis" = "macropus dorsalis",
    "notamacropus eugenii" = "macropus eugenii",
    "notamacropus parma" = "macropus parma",
    "notamacropus parryi" = "macropus parryi",
    "notamacropus rufogriseus" = "macropus rufogriseus",
    "nyctalus velutinus" = "nyctalus noctula",
    "oligoryzomys costaricensis" = "oligoryzomys fulvescens",
    "oligoryzomys mattogrossae" = "oligoryzomys microtis",
    "oligoryzomys utiaritensis" = "oligoryzomys eliurus",
    "orientallactaga sibirica" = "allactaga sibirica",
    "oryzomys texensis" = "oryzomys palustris",
    "osphranter robustus" = "macropus robustus",
    "osphranter rufus" = "macropus rufus",
    "ovis orientalis" = "ovis gmelini",
    "oxymycterus judex" = "oxymycterus quaestor",
    "pantherina griselda" = "blarinella griselda",
    "parahypsugo crassulus" = "pipistrellus crassulus",
    "pekania pennanti" = "martes pennanti",
    "phoca fasciata" = "histriophoca fasciata",
    "phoca groenlandica" = "pagophilus groenlandicus",
    "physeter catodon" = "physeter macrocephalus",
    "plecotus gaisleri" = "plecotus teneriffae",
    "prionailurus iriomotensis" = "prionailurus bengalensis",
    "ptenochirus jagorii" = "ptenochirus jagori",
    "pteronotus alitonus" = "pteronotus rubiginosus",
    "puma yagouaroundi" = "herpailurus yagouaroundi",
    "rattus flavipectus" = "rattus tanezumi",
    "rhinolophus blythi" = "rhinolophus pusillus",
    "rhinolophus cornutus" = "rhinolophus pusillus",
    "rhinolophus hildebrandti" = "rhinolophus hildebrandtii",
    "rhinolophus lobatus" = "rhinolophus landeri",
    "rhinolophus monoceros" = "rhinolophus pusillus",
    "rhinolophus rhodesiae" = "rhinolophus swinnyi",
    "scarturus elater" = "allactaga elater",
    "sorex monticolus" = "sorex monticola",
    "syntheosciurus granatensis" = "sciurus granatensis",
    "taeromys dominator" = "paruromys dominator",
    "talpa aquitania" = "talpa occidentalis",
    "tamias amoenus" = "neotamias amoenus",
    "tamias minimus" = "neotamias minimus",
    "tamias quadrivittatus" = "neotamias quadrivittatus",
    "tamias sibiricus" = "eutamias sibiricus",
    "tamias umbrinus" = "neotamias umbrinus",
    "terricola subterraneus" = "microtus subterraneus",
    "triaenops menamena" = "triaenops rufus",
    "tupaia chinensis" = "tupaia belangeri",
    "urva auropunctata" = "herpestes auropunctatus",
    "urva edwardsii" = "herpestes edwardsii",
    "urva javanica" = "herpestes javanicus",
    "xanthonycticebus pygmaeus" = "nycticebus pygmaeus",
    "zygodontomys cherriei" = "zygodontomys brevicauda"
  )
)

# recheck
miss <- setdiff(vir$iucn, iucn$binomial)

# merge
iucn <- iucn %>%
  # dplyr::mutate(binomial = stringr::str_to_lower(binomial)) %>%
  dplyr::mutate(anyViruses = as.numeric(binomial %in% vir$iucn)) ## match to revalue names

mam_raster <- raster_extract(iucn_data = iucn) # this is the full extent we want
mammals <- init_data_ob(iucn, mam_raster) # the data object
plot(mammals)

# Bar plot =====================================================================

orderCounts <- iucn %>%
  as.data.frame() %>%
  dplyr::select(order_, binomial, anyViruses) %>%
  dplyr::distinct() %>%
  dplyr::group_by(order_, anyViruses) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(order_ = str_to_sentence(order_))

top10orders <- orderCounts %>%
  group_by(order_) %>%
  summarize(n = sum(n)) %>%
  top_n(10) %>%
  arrange(-n) %>%
  pull(order_)

## DB VERSION
g1 <- orderCounts %>%
  filter(order_ %in% top10orders) %>%
  mutate(order_ = factor(order_, levels = top10orders)) %>%
  rename(count = n) %>%
  mutate(anyViruses = factor(anyViruses, levels = c("0", "1"))) %>%
  ggplot(aes(fill = anyViruses, y = count, x = `order_`)) +
  geom_bar(position = "stack", stat = "identity", colour = "black") +
  # coord_flip() +
  xlab("Mammal orders (top 10 by descending species richness)") +
  ylab("Number of species") +
  theme_base() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12), # change legend text font size
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(2, 2, 2, 2)
  ) +
  # theme(legend.position = c(0.6, 0.1),
  #      legend.title = element_blank(),
  #      legend.box.background = element_rect(colour = "black")) +
  scale_fill_manual(
    values = c("grey90", "#3facbdee"),
    labels = c("No viruses known", "Viruses recorded")
  )

## Map

iucnno <- iucn %>% dplyr::filter(anyViruses == 0)
iucnyes <- iucn %>% dplyr::filter(anyViruses == 1)

# get the fasterized version of the mammals with no viruses
noraster <- fasterize(iucnno, mam_raster, fun = "count")
plot(noraster)



# now just get the raster with all the mammals
allraster <- fasterize(iucn, mam_raster, fun = "count")
plot(allraster)

# need to fill the NA's that should actually be zero's as zeros
summary(noraster@data@values)
noraster@data@values[which(
  is.na(noraster@data@values) & !is.na(allraster@data@values)
)] <- 0

propno <- (noraster) / (allraster)
plot(propno)
summary(propno@data@values)

scaleddiffdf <- raster::as.data.frame(propno, xy = TRUE)

g22 <- ggplot() +
  tidyterra::stat_spatraster(data = terra::rast(propno)) +
  theme_base() +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = c(0.03, 0.03)) +
  theme(
    legend.position = "top",
    legend.key.size = unit(2, "cm"), # change legend key size
    legend.key.height = unit(1, "cm"), # change legend key height
    legend.title = element_text(size = 12), # change legend title font size
    legend.text = element_text(size = 10), # change legend text font size
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.title.align = 1,
    legend.key.width = unit(2, "cm"),
  ) +
  scale_fill_gradientn(
    colors = rev(MoMAColors::moma.colors("OKeeffe")),
    name = "Proportion of species  \nwith no known viruses  ",
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = c("0.0", "0.25", "0.50", "0.75", "1.00"),
    limits = c(0, 1),
    na.value = "grey80"
  )
ggplot2::ggsave(here::here("./figs/box-3/just-map.png"), g22)
## Assembly

# g1 + g2 + plot_layout(widths = c(1, 3))

## DB VERSION, use width=4,height=4.5
# ggarrange(g1, g2, ncol = 2, heights = c(1, 1))

# cole version
p <- g1 + g22 + plot_layout(widths = c(-1, 2))
p
ggplot2::ggsave(here::here("./figs/box-3/side-by-side.png"), p,
  width = 12, height = 10, dpi = 300
)

# special save for figures & their component parts
ggsave(
  here::here("./figs/box-3/submission-figs/box-3.pdf"),
  p,
  height = 12, width = 11,
  dpi = 600
)
ggsave(
  here::here("./figs/box-3/submission-figs/box-3-histogram.pdf"),
  g1,
  height = 6, width = 7,
  dpi = 600
)
ggsave(
  here::here("./figs/box-3/submission-figs/figure-4-a-image.pdf"),
  g22,
  height = 10, width = 8,
  dpi = 600
)
