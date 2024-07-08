#' DESCRIPTION: three hockeystick plots of extinction, outbreaks, & temperature
#' AUTHOR: Cole Brookson & Colin Carlson
#' DATE: 03 July 2024

# set up =======================================================================
library(here)
library(readr)
library(ggplot2)
library(rgbif)
library(rnaturalearth)
library(MoMAColors)
library(figpatch)

source(here::here("./src/global-funs.R"))


# Horseshoe bats ===============================================================

# gbif_download_bat <- occ_download(pred("taxonKey", 2432611),
#   format = "SIMPLE_CSV"
# )
res_bat <- rgbif::occ_download_get(
  key = "0015768-240626123714530",
  path = here::here("./data/GBIF/"),
  overwrite = FALSE
)
rhi <- rgbif::occ_download_import(res_bat)

ne_sf <- rnaturalearth::ne_coastline(returnclass = "sf")

## data cleaning ===============================================================
#' there's some occurent records in weird places up in north america that seem
#' wrong -- it was easiest to diagnose this in the web viewer, so we've
#' downloaded all occurences where the country code doesn't match (e.g. there's
#' a bat in the allutian islands apparently but the geolocation says Australia)
#' and we just use that to filter out the bad observations
bad_bat <- rgbif::occ_download_get(
  key = "0017000-240626123714530",
  path = here::here("./data/GBIF/"),
  overwrite = FALSE
)
bad_bat_LA <- rgbif::occ_download_get(
  key = "0017081-240626123714530",
  path = here::here("./data/GBIF/"),
  overwrite = FALSE
)
bad_bat_obs <- rgbif::occ_download_import(bad_bat)
bad_bat_LA_obs <- rgbif::occ_download_import(bad_bat_LA)

clean_rhi <- rhi %>%
  dplyr::filter(occurrenceID %notin% bad_bat_obs$occurrenceID) %>%
  # there's also two observatiosn in the americas that are definitely eroneous
  dplyr::filter(occurrenceID %notin% "MCZ:Mamm:4939") %>%
  # and a bunch in the LA museum
  dplyr::filter(occurrenceID %notin% bad_bat_LA_obs$occurrenceID)

horsehoe_bat <- ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() +
  geom_bin2d(
    data = clean_rhi, aes(x = decimalLongitude, y = decimalLatitude),
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
  overwrite = FALSE
)
aed <- occ_download_import(aed_download)

ne_sf <- ne_coastline(returnclass = "sf")

aedes_map <- ggplot(data = ne_sf) +
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


# put the plots all together ===================================================

aedes_img <- figpatch::fig(here::here("./data/GBIF/aedes-image.png"))
bat_img <- figpatch::fig(here::here("./data/GBIF/bat-image.png"))
