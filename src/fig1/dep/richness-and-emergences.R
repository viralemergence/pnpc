#' DESCRIPTION: map of richness of mammal hosts of any zoonotic diseasend
#' AUTHOR: Colin Carlson
#' DATE: 03 July 2024

# set up =======================================================================

library(readr)
library(here)
library(dplyr)
library(magrittr)
library(tidyr)
library(sf)
library(tidyterra)
library(raster)
library(fasterize)
library(ggnewscale)
library(ggplot2)


clo_bac <- readr::read_csv(here::here("./data/clover/clover-1.0-bacteria.csv"))
clo_vir <- readr::read_csv(here::here("./data/clover/clover-1.0-viruses.csv"))
clo_other <- readr::read_csv(
  here::here("./data/clover/clover-1.0-hel-proto-fungi.csv")
)
iucn <- sf::st_read(here::here("./data/IUCN/"))
eidr <- readr::read_csv("./data/recreation/data-from-eidr.csv")

# data cleaning ================================================================

# single object to work on
clo <- dplyr::bind_rows(clo_bac, clo_vir, clo_other)

zoonoses <- clo %>%
  dplyr::filter(Host == "homo sapiens") %>%
  dplyr::pull(Pathogen) %>%
  unique()
reservoirs <- clo %>%
  dplyr::filter(
    HostClass == "mammalia" &
      !(Host == "homo sapiens") &
      Pathogen %in% zoonoses
  ) %>%
  dplyr::pull(Host) %>%
  unique()

ranges <- iucn %>%
  dplyr::mutate(binomial = stringr::str_to_lower(binomial)) %>%
  dplyr::filter(binomial %in% reservoirs)

# set up raster data
mraster <- raster::raster(iucn, res = 1 / 6)
zraster <- fasterize::fasterize(ranges, mraster, fun = "sum")

# points
eidr <- eidr %>%
  dplyr::filter(!(`Drug Resistance` == "Yes")) %>%
  dplyr::select(Longitude, Latitude, `Pathogen Type`) %>%
  tidyr::separate_rows(dplyr::everything(), sep = ",") %>%
  dplyr::filter(!Latitude == "Not Found") %>%
  dplyr::filter(!Longitude == "Not Found") %>%
  dplyr::mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

eidr$`Pathogen Type`[!(eidr$`Pathogen Type`
  %in% c("Bacteria", "Virus"))] <- "Other"

# plot
richness_eidr <- ggplot() +
  tidyterra::stat_spatraster(data = terra::rast(zraster), alpha = 0.7) +
  theme_base() +
  scale_fill_gradientn(
    "Mammal Hosts",
    colors = rev(MoMAColors::moma.colors("OKeeffe"))
  ) +
  ggnewscale::new_scale_fill() +
  geom_point(
    data = eidr,
    aes(
      x = Longitude, y = Latitude, shape = `Pathogen Type`,
      fill = `Pathogen Type`
    ),
    color = "black", stroke = 0.6
  ) +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_fill_manual(values = c(
    MoMAColors::moma.colors("Budnitz")[c(2)],
    MoMAColors::moma.colors("Flash")[c(3)],
    MoMAColors::moma.colors("Ohchi")[c(3)]
  )) +
  xlab(NULL) +
  ylab(NULL) +
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 4
      )
    )
  )
ggsave(
  here::here("./figs/fig-1/map.png"),
  richness_eidr
)
