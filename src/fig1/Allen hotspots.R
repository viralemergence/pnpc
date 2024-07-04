#' DESCRIPTION: a map of viral biodiversity
#' AUTHOR: Cole Brookson
#' DATE: 06 June 2024

# set up =======================================================================
library(here)
library(ggplot2)
library(raster)

# File origin: https://github.com/ecohealthalliance/hotspots2/blob/master/inst/out/raster/bsm_response.tif
r <- raster("~/Downloads/bsm_response.tif")
r2 <- as.data.frame(r, xy = TRUE)

ne_sf <- ne_coastline(returnclass = "sf")

ggplot(data = ne_sf) +
  geom_sf() +
  geom_raster(data = r2, aes(x = x, y = y, fill = bsm_response)) +
  scale_fill_moma_c("ustwo",
    name = "Occ.",
    trans = "log",
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    direction = -1,
    na.value = 0
  ) +
  theme_light() +
  geom_sf() +
  coord_sf() +
  xlab("") +
  ylab("") +
  ggtitle("Ecological predictors of emerging infectious disease events")
