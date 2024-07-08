#' DESCRIPTION: three hockeystick plots of extinction, outbreaks, & temperature
#' AUTHOR: Cole Brookson & Colin Carlson
#' DATE: 03 July 2024

# set up =======================================================================
library(here)
library(readr)
library(ggplot2)
library(rgbif)
library(CoordinateCleaner)
library(rnaturalearth)
library(MoMAColors)

source(here::here("./src/global-funs.R"))


# Horseshoe bats ===============================================================

# gbif_download_bat <- occ_download(pred("taxonKey", 2432611),
#   format = "SIMPLE_CSV"
# )
res_bat <- rgbif::occ_download_get(
  key = "0015768-240626123714530",
  path = here::here("./data/GBIF/"),
  overwrite = TRUE
)
rhi <- rgbif::occ_download_import(res_bat)

ne_sf <- rnaturalearth::ne_coastline(returnclass = "sf")

## data cleaning ===============================================================
#' there's some occurent records in weird places up in north america that seem
#' wrong
names(rhi)
unique(rhi$issue)
clean_rhi <- rhi %>%
  dplyr::filter(occurrenceID %notin% c(
    # looks like it's in the middle of the ocean by tazmania - there's country
    # code mismatches and they're from a museum
    "c2e4ea55-bdb6-4555-a091-27158fc15c73",
    "f2a2d90f-8857-42cc-84c5-48b07f7c2f05",
    # this is in the middle of the ocean but is supposed to be south africa
    "	67cce1c4-f6e8-49be-8cb3-982ba2789fe2"
  )) %>%
  CoordinateCleaner::cc_coun(
    x = .,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    iso3 = "countryCode",
    value = "clean"
  )

horsehoe_bat <- ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() +
  geom_bin2d(
    data = rhi, aes(x = decimalLongitude, y = decimalLatitude),
    binwidth = c(5, 5)
  ) +
  MoMAColors::scale_fill_moma_c("ustwo",
    name = "Occurrences",
    trans = "log",
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    direction = -1
  ) +
  geom_sf() +
  coord_sf() +
  theme_base() +
  xlab("") +
  ylab("") +
  theme(legend.position = "left", legend.text.position = "left")

# Aedes aegypti ================================================================

# gbif_download_aedes <- occ_download(pred("taxonKey", 1651891),
#   format = "SIMPLE_CSV"
# )
aed_download <- rgbif::occ_download_get(
  key = "0015770-240626123714530",
  path = here::here("./data/GBIF/"),
  overwrite = TRUE
)
aed <- occ_download_import(aed_download)

ne_sf <- ne_coastline(returnclass = "sf")

ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() +
  geom_bin2d(
    data = aed, aes(x = decimalLongitude, y = decimalLatitude),
    binwidth = c(5, 5)
  ) +
  scale_fill_moma_c("ustwo",
    name = "Occurrences",
    trans = "log",
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    direction = -1
  ) +
  geom_sf() +
  coord_sf() +
  theme_light() +
  xlab("") +
  ylab("") +
  theme(legend.position = "left", legend.text.position = "left")
