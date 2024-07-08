#' DESCRIPTION: plot of zoonotic viral discovery through time
#' AUTHOR: Colin Carlson
#' DATE: June 2024

# set up =======================================================================
library(dplyr)
library(vroom)
library(here)
library(ggplot2)
library(tidyr)

vir <- vroom::vroom(here::here("./data/virion/virion-zipped.csv.gz"))

vir <- vir %>%
  # filter(ICTVRatified == 'TRUE') %>%
  rowwise() %>%
  # in rows where there's no year for either it'll return INF
  mutate(year = min(PublicationYear, ReleaseYear, na.rm = TRUE))
vir$year[vir$year == Inf] <- NA

zoos <- vir %>%
  filter(Host == "homo sapiens") %>%
  pull(Virus) %>%
  unique()

vir <- vir %>%
  mutate(zoo = as.numeric(Virus %in% zoos)) %>%
  group_by(Virus) %>%
  summarize(zoo = max(zoo), year = min(year)) %>%
  filter(!is.na(year)) %>%
  dplyr::mutate(zoo = dplyr::case_when(
    zoo == 0 ~ "no",
    zoo == 1 ~ "yes"
  ))


# zoonotic_virs <-
vir %>%
  group_by(zoo) %>%
  count(year) %>%
  complete(year = 1955:2020, fill = list(n = 0)) %>%
  mutate(tally = cumsum(n)) %>%
  ggplot(aes(x = year, y = tally, group = zoo, fill = zoo)) +
  scale_fill_manual("Zoonotic", values = c("lightblue", "darkblue")) +
  geom_area() +
  labs(x = "Year", y = "Viral Discovery Count") +
  theme_base() +
  xlim(1990, 2020)
ggsave(
  here::here("./figs/fig-1/zoonotic-thru-time.png"),
  zoonotic_virs
)
