#' DESCRIPTION: three hockeystick plots of extinction, outbreaks, & temperature
#' AUTHOR: Cole Brookson & Colin Carlson
#' DATE: 03 July 2024

# set up =======================================================================
library(here)
library(readr)
library(ggplot2)
library(magrittr)
library(rstanram)
library(bayesplot)
library(modelr)
library(ggdist)
library(patchwork)
library(tidyr)
library(sf)
library(tidyterra)
library(raster)
library(fasterize)
library(ggnewscale)

# data for the map portion
clo_bac <- readr::read_csv(here::here("./data/clover/clover-1.0-bacteria.csv"))
clo_vir <- readr::read_csv(here::here("./data/clover/clover-1.0-viruses.csv"))
clo_other <- readr::read_csv(
    here::here("./data/clover/clover-1.0-hel-proto-fungi.csv")
)
iucn <- sf::st_read(here::here("./data/IUCN/"))
eidr <- readr::read_csv("./data/recreation/data-from-eidr.csv")

# data for the hockeystick partss
extinction <- readr::read_csv(
    here::here("./data/recreation/data-from-ceballos-etal-2015.csv")
)
