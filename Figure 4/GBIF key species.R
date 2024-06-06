
library(tidyverse)
library(rgbif)
library(rnaturalearth)
library(MoMAColors)

# Horseshoe bats

#gbif_download <- occ_download(pred("taxonKey", 2432611),format = "SIMPLE_CSV")
res <- occ_download_get(key="0007082-240425142415019", overwrite=TRUE)
rhi <- occ_download_import(res)

ne_sf <- ne_coastline(returnclass = "sf")

ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() + 
  geom_bin2d(data = rhi, aes(x = decimalLongitude, y = decimalLatitude), binwidth = c(5,5)) + 
  scale_fill_moma_c("ustwo", 
                    name = "Occurrences", 
                    trans = "log", 
                    breaks = c(1, 10, 100, 1000, 10000, 100000),
                    direction = -1) + 
  geom_sf() +
  coord_sf() + 
  theme_light() + 
  xlab("") + ylab("") + 
  theme(legend.position = 'left', legend.text.position = 'left')

# Aedes aegypti

#gbif_download <- occ_download(pred("taxonKey", 1651891),format = "SIMPLE_CSV")
res <- occ_download_get(key="0007096-240425142415019", overwrite=TRUE)
aed <- occ_download_import(res)

ne_sf <- ne_coastline(returnclass = "sf")

ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() + 
  geom_bin2d(data = aed, aes(x = decimalLongitude, y = decimalLatitude), binwidth = c(5,5)) + 
  scale_fill_moma_c("ustwo", 
                    name = "Occurrences", 
                    trans = "log", 
                    breaks = c(1, 10, 100, 1000, 10000, 100000),
                    direction = -1) + 
  geom_sf() +
  coord_sf() + 
  theme_light() + 
  xlab("") + ylab("") + 
  theme(legend.position = 'left', legend.text.position = 'left')
