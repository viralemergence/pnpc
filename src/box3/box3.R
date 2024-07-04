#' DESCRIPTION: figure for the third box
#' AUTHOR: Daniel Becker
#' DATE: 19 June 2024

# set up =======================================================================

library(tidyverse)
library(sf)
library(vroom)
library(fasterize)
library(patchwork)
library(MetBrewer)

source(here::here("./src/global-funs.R"))

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
      source(here::here("./__main.R"))
    })
    warning <- function(w) {
      print() # dummy warning function to suppress the output of warnings
    }
  }
)

# IUCN data ====================================================================

iucn %>%
  mutate(binomial = str_to_lower(binomial)) %>%
  mutate(anyViruses = as.numeric(binomial %in% vir$Host)) -> iucn

# Bar plot =====================================================================

iucn %>%
  as.data.frame() %>%
  select(order_, binomial, anyViruses) %>%
  distinct() %>%
  group_by(order_, anyViruses) %>%
  count() %>%
  ungroup() %>%
  mutate(order_ = str_to_sentence(order_)) -> orderCounts

orderCounts %>%
  group_by(order_) %>%
  summarize(n = sum(n)) %>%
  top_n(10) %>%
  arrange(-n) %>%
  pull(order_) -> top10orders

# orderCounts %>%
#   filter(order_ %in% top10orders) %>%
#   mutate(order_ = factor(order_, levels = rev(top10orders))) %>%
#   rename(count = n) %>%
#   mutate(anyViruses = factor(anyViruses, levels = c('0', '1'))) %>%
#   ggplot(aes(fill=anyViruses, y = count, x = `order_`)) +
#   geom_bar(position="stack", stat="identity") +
#   coord_flip() +
#   xlab('Mammal orders (top 10 by descending species richness)') +
#   ylab('Number of species') +
#   theme_bw() +
#   theme(legend.position = c(0.6, 0.1),
#         legend.title = element_blank(),
#         legend.box.background = element_rect(colour = "black")) +
#   scale_fill_manual(values = c('lightgrey', 'darkblue'), labels = c("No viruses known", "Viruses recorded")) -> g1

## DB VERSION
orderCounts %>%
  filter(order_ %in% top10orders) %>%
  mutate(order_ = factor(order_, levels = top10orders)) %>%
  rename(count = n) %>%
  mutate(anyViruses = factor(anyViruses, levels = c("0", "1"))) %>%
  ggplot(aes(fill = anyViruses, y = count, x = `order_`)) +
  geom_bar(position = "stack", stat = "identity") +
  # coord_flip() +
  xlab("Mammal orders (top 10 by descending species richness)") +
  ylab("Number of species") +
  theme_base() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(2, 2, 2, 2)
  ) +
  # theme(legend.position = c(0.6, 0.1),
  #      legend.title = element_blank(),
  #      legend.box.background = element_rect(colour = "black")) +
  scale_fill_manual(
    values = c("#d3d3d3b4", met.brewer("Signac")[8]),
    labels = c("No viruses known", "Viruses recorded")
  ) -> g1

## Map

iucn %>% filter(anyViruses == 0) -> iucnno
iucn %>% filter(anyViruses == 1) -> iucnyes

mraster <- raster(iucn, res = 1 / 6)

noraster <- fasterize(iucnno, mraster, fun = "sum")
yesraster <- fasterize(iucnyes, mraster, fun = "sum")

diff <- (noraster - yesraster)
plot(diff)

propno <- (noraster) / (noraster + yesraster)
plot(propno)

scaleddiffdf <- raster::as.data.frame(propno, xy = TRUE)
# scaleddiffdf %>%
#   ggplot(aes(x = x, y = y, fill = layer)) +
#   geom_raster() + coord_sf() +
#   theme_void() +

## DB VERSION
scaleddiffdf %>%
  ggplot(aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  coord_sf() +
  theme_base() +
  theme(legend.position = "top") +
  scale_fill_gradientn(
    colors = met.brewer("Morgenstern"),
    name = "Proportion with\nno known viruses"
  ) -> g2

g22 <- ggplot() +
  tidyterra::stat_spatraster(data = terra::rast(propno)) +
  theme_base() +
  theme(
    legend.position = "top",
    legend.key.size = unit(2, "cm"), # change legend key size
    legend.key.height = unit(1, "cm"), # change legend key height
    legend.key.width = unit(1, "cm"), # change legend key width
    legend.title = element_text(size = 14), # change legend title font size
    legend.text = element_text(size = 10), # change legend text font size
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.title.align = 1
  ) +
  scale_fill_gradientn(
    colors = met.brewer("Morgenstern"),
    name = "Proportion of species  \nwith no known viruses  ",
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    labels = c("0.0", "0.25", "0.50", "0.75", "1.00"),
    limits = c(0, 1)
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
  width = 12, height = 10
)
