#' DESCRIPTION: a map of emerging disease hotspots
#' AUTHOR: Colin Carlson
#' DATE: June 2024

# set up =======================================================================
library(here)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(MoMAColors)

r <- raster::raster(
  here::here("./data/recreation/data-from-allen-etal-2017.tif")
)
r2 <- as.data.frame(r, xy = TRUE)
colnames(r2) <- c("x", "y", "response")

ne_sf <- rnaturalearth::ne_coastline(returnclass = "sf")

predictors <- ggplot(data = ne_sf) +
  geom_sf() +
  geom_raster(data = r2, aes(x = x, y = y, fill = response)) +
  MoMAColors::scale_fill_moma_c("ustwo",
    name = "Occ.",
    trans = "log",
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    direction = -1,
    na.value = 0
  ) +
  theme_base() +
  geom_sf() +
  coord_sf() +
  xlab("") +
  ylab("") +
  ggtitle("Ecological predictors of emerging infectious disease events")
ggsave(
  here::here("./figs/fig-1/predictors-of-dis-events.png"),
  predictors
)
