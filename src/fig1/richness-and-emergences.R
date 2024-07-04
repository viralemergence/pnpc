#' DESCRIPTION: map of richness of mammal hosts of any zoonotic diseasend
#' AUTHOR: Colin Carlson
#' DATE: 03 July 2024

# set up =======================================================================

library(readr)
library(here)
library(dplyr)
library(sf)
library(raster)
library(fasterize)
library(ggnewscale)

# raster

clo1 <- read_csv("~/Documents/Github/clover/clover/clover_1.0_allpathogens/CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")
clo2 <- read_csv("~/Documents/Github/clover/clover/clover_1.0_allpathogens/CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")
clo3 <- read_csv("~/Documents/Github/clover/clover/clover_1.0_allpathogens/CLOVER_1.0_Viruses_AssociationsFlatFile.csv")
clo <- bind_rows(clo1, clo2, clo3)

clo %>%
  filter(Host == "homo sapiens") %>%
  pull(Pathogen) %>%
  unique() -> zoonoses
clo %>%
  filter(HostClass == "mammalia") %>%
  filter(!(Host == "homo sapiens")) %>%
  filter(Pathogen %in% zoonoses) %>%
  pull(Host) %>%
  unique() -> reservoirs

iucn <- read_sf("./Data/IUCN/MAMMALS.shp")
iucn %>%
  mutate(binomial = str_to_lower(binomial)) %>%
  filter(binomial %in% reservoirs) -> ranges

mraster <- raster(iucn, res = 1 / 6)
zraster <- fasterize(ranges, mraster, fun = "sum")

# points

eidr <- read_csv("./Data/recreation/eidr.csv")

eidr %>%
  filter(!(`Drug Resistance` == "Yes")) %>%
  dplyr::select(Longitude, Latitude, `Pathogen Type`) %>%
  separate_rows(everything(), sep = ",") %>%
  filter(!Latitude == "Not Found") %>%
  filter(!Longitude == "Not Found") %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) -> eidr

eidr$`Pathogen Type`[!(eidr$`Pathogen Type` %in% c("Bacteria", "Virus"))] <- "Other"


# plot

ggplot() +
  tidyterra::stat_spatraster(data = terra::rast(zraster)) +
  theme_classic() +
  scale_fill_gradientn(
    colors = met.brewer("Tiepolo", direction = -1)
  ) +
  new_scale_fill() +
  geom_point(data = eidr, aes(x = Longitude, y = Latitude, shape = `Pathogen Type`, fill = `Pathogen Type`), color = "white", stroke = 0.6) +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_fill_manual(values = c("darkblue", "forestgreen", "darkred")) +
  xlab(NULL) +
  ylab(NULL)
