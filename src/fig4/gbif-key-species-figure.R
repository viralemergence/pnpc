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
library(patchwork)

source(here::here("./src/global-funs.R"))
options(scipen = 1000)

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

bat_title <- expression(
  paste(italic("Rhinolophus"), " spp. global occurences")
)
bat_map <- ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() +
  geom_bin2d(
    data = clean_rhi, aes(x = decimalLongitude, y = decimalLatitude),
    binwidth = c(5, 5)
  ) +
  scale_fill_gradient(
    name = "Occurrences (log scale)",
    # colors = rev(MoMAColors::moma.colors("ustwo")),
    low = "#f6dcf1", high = "#750e5f",
    trans = "log",
    breaks = c(1, 100, 20000),
    # labels = scales::cut_short_scale(c(1, 10, 100, 1000, 20000)),
    labels = scales::label_scientific()
    # limits = c(1, 15000)
  ) +
  geom_sf() +
  coord_sf() +
  theme_base() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.15),
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.background = element_rect(fill = alpha("white", 0.7)),
    legend.key = element_rect(fill = NA),
    legend.title = element_text(hjust = 0.5),
    legend.box.just = "bottom",
    legend.text = element_text(size = rel(0.7)),
    legend.key.width = unit(1.4, "cm"),
    plot.title = element_text(size = rel(1.5))
  ) +
  ggtitle(bat_title)

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

aedes_title <- expression(
  paste(italic("Aedes aegypti"), " global occurences")
)
aedes_map <- ggplot(data = ne_sf) +
  geom_sf() +
  coord_sf() +
  geom_bin2d(
    data = aed, aes(x = decimalLongitude, y = decimalLatitude),
    binwidth = c(5, 5)
  ) +
  scale_fill_gradient(
    name = "Occurrences (log scale)",
    # colors = rev(MoMAColors::moma.colors("ustwo")),
    low = "#9bdced", high = "#134d5b",
    trans = "log",
    breaks = c(1, 100, 5000),
    labels = scales::label_scientific()
    # limits = c(1, 15000)
  ) +
  geom_sf() +
  coord_sf() +
  theme_base() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.15),
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.background = element_rect(fill = alpha("white", 0.8)),
    legend.key = element_rect(fill = NA),
    legend.title = element_text(hjust = 0.5),
    legend.box.just = "bottom",
    legend.text = element_text(size = rel(0.7)),
    legend.key.width = unit(1.4, "cm"),
    plot.title = element_text(size = rel(1.5))
  ) +
  ggtitle(aedes_title)


# put the plots all together ===================================================

aedes_img <- figpatch::fig(here::here("./data/GBIF/aedes-image.png"))
bat_img <- figpatch::fig(here::here("./data/GBIF/bat-image.png"))

aedes_together <- patchwork::wrap_plots(aedes_map, aedes_img)
ggsave(
  here::here("./figs/fig-4/aedes-plot.png"),
  aedes_together,
  width = 12, height = 8
)
bats_together <- patchwork::wrap_plots(bat_map, bat_img)
ggsave(
  here::here("./figs/fig-4/bats-plot.png"),
  bats_together,
  width = 20, height = 8
)
fig_4 <- ggpubr::ggarrange(
  aedes_together, bats_together,
  labels = c("A", "B"),
  nrow = 2,
  font.label = list(size = 28),
  widths = 1,
  heights = 1
) +
  ggpubr::bgcolor("white")
ggsave(
  here::here("./figs/fig-4/figure-4.png"),
  fig_4,
  height = 10, width = 14,
  dpi = 300
)
