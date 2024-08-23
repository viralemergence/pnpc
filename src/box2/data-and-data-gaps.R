#' DESCRIPTION: figure for the third box
#' AUTHOR: Daniel Becker & Cole Brookson
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

iucn <- iucn %>%
  dplyr::mutate(binomial = stringr::str_to_lower(binomial)) %>%
  dplyr::mutate(anyViruses = as.numeric(binomial %in% vir$Host))

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
noraster <- fasterize(iucnno, mraster, fun = "count")
plot(noraster)

# need to fill the NA's that should actually be zero's as zeros
summary(noraster@data@values)
noraster@data@values[which(
  is.na(noraster@data@values) & !is.na(allraster@data@values)
)] <- 0

# now just get the raster with all the mammals
allraster <- fasterize(iucn, mraster, fun = "count")
plot(allraster)

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
