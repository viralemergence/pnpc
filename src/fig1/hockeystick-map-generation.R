#' DESCRIPTION: three hockeystick plots of extinction, outbreaks, & temperature
#' AUTHOR: Cole Brookson & Colin Carlson
#' DATE: 03 July 2024

# set up =======================================================================
library(here)
library(readr)
library(ggplot2)
library(magrittr)
library(rstanarm)
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
library(tidybayes)

source(here::here("./src/global-funs.R"))

# data for the map portion
clo_bac <- readr::read_csv(here::here("./data/clover/clover-1.0-bacteria.csv"))
clo_vir <- readr::read_csv(here::here("./data/clover/clover-1.0-viruses.csv"))
clo_other <- readr::read_csv(
    here::here("./data/clover/clover-1.0-hel-proto-fungi.csv")
)
# need to make sure we're all using the same IUCN
tryCatch(
    {
        iucn <- sf::read_sf(here::here("./data/IUCN/"))
    },
    warning = function(w) {
        print() # dummy warning function to suppress the output of warnings
    },
    error = function(err) {
        print(
            "Could not read data from current directory, ",
            "attempting download..."
        )
        tryCatch({
            source(here::here("./src/iucn-download.R"))
        })
        warning <- function(w) {
            print() # dummy warning function to suppress the output of warnings
        }
    }
)
eidr <- readr::read_csv("./data/recreation/data-from-eidr.csv")

# data for the hockeystick partss
extinction <- readr::read_csv(
    here::here("./data/recreation/data-from-ceballos-etal-2015.csv")
)
temperature <- readr::read_delim(
    here::here("./data/recreation/data-from-PAGES2k-const.txt"),
    delim = "\t",
    skip = 4
)
co2_reconstructed <- readr::read_delim(
    here::here("./data/recreation/data-from-frank-etal-2010.txt"),
    delim = "\t"
)
co2_observed <- readr::read_csv(
    here::here("./data/recreation/data-from-lan-etal-2024.csv"),
    skip = 32,
    col_names = c("year", "month", "day", "decimal", "ppm")
)
spillover <- readr::read_csv(
    here::here("./data/recreation/data-from-meadows-etal-2023.csv")
)

# map ==========================================================================

## data cleaning ===============================================================

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

## plot ========================================================================
map_title <- expression("Hotspots and hosts of emerging infectious diseases")
richness_eidr_map <- ggplot() +
    tidyterra::stat_spatraster(data = terra::rast(zraster), alpha = 0.7) +
    # these scale_continuous with the expands fill out the box better
    scale_x_continuous(expand = c(0.02, 0.02)) +
    scale_y_continuous(expand = c(0.03, 0.03)) +
    theme_base() +
    scale_fill_gradientn(
        "Mammal Hosts",
        colors = rev(MoMAColors::moma.colors("OKeeffe")),
        guide = guide_colorbar(order = 1)
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.22, 0.12),
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.title.position = "top",
        legend.background = element_rect(fill = alpha("white", 0.2)),
        legend.key = element_rect(fill = NA),
        legend.title.align = 0.5,
        legend.box.just = "bottom",
        legend.text = element_text(size = rel(1)),
        plot.title = element_text(size = rel(1.5), face = NULL)
    ) +
    # guides(
    #     fill = guide_colorbar(order = 1)
    # ) +
    ggnewscale::new_scale_fill() +
    geom_point(
        data = eidr,
        aes(
            x = Longitude, y = Latitude, shape = `Pathogen Type`,
            fill = `Pathogen Type`
        ),
        color = "black", stroke = 0.6, size = 3
    ) +
    scale_shape_manual(
        values = c(21, 22, 23)
    ) +
    scale_fill_manual(values = c(
        MoMAColors::moma.colors("Budnitz")[c(2)],
        MoMAColors::moma.colors("Flash")[c(3)],
        MoMAColors::moma.colors("Ohchi")[c(3)]
    )) +
    xlab(NULL) +
    ylab(NULL) +
    guides(shape = guide_legend(override.aes = list(size = 4))) +
    ggtitle(map_title)

ggsave(
    here::here("./figs/fig-1/map.png"),
    richness_eidr_map,
    width = 14, height = 7
)
ggsave(
    here::here("./figs/fig-1/submission-figs/map.pdf"),
    richness_eidr_map,
    width = 14, height = 7,
    dpi = 600
)
ggsave(
    here::here("./figs/fig-1/map-hi-def.png"),
    richness_eidr_map,
    width = 14, height = 7,
    dpi = 600
)

# hockey stick timeseries ======================================================

## extinctions =================================================================

### data cleaning ==============================================================
extinction <- extinction %>%
    dplyr::mutate(
        date_range = as.factor(dplyr::case_when(
            date_range_start == 1500 ~ "1550",
            date_range_start == 1600 ~ "1650",
            date_range_start == 1700 ~ "1750",
            date_range_start == 1800 ~ "1850",
            date_range_start == 1900 ~ "1955",
        )),
        taxa = factor(taxa, levels = c(
            "all_verts", "birds", "mammals", "other_verts", "background"
        ))
    ) %>%
    dplyr::mutate(
        # this will just make plotting easier
        helper = ifelse(taxa == "background", "background", "taxa")
    )

### plotting ===================================================================
extinctions_title <- expression("Biodiversity loss")
extinctions_plot <- ggplot() +
    geom_line(
        data = extinction,
        aes(
            x = date_range, y = cumulative_ext_rate,
            colour = taxa, group = taxa, linetype = helper
        ), linewidth = 2
    ) +
    theme_base() +
    scale_color_manual(
        name = "Taxa",
        values = c(
            MoMAColors::moma.colors("OKeeffe")[c(1, 4)],
            MoMAColors::moma.colors("Ernst")[c(4, 7)],
            "grey80"
        ),
        labels = c(
            "All Vertebrates", "Birds", "Mammals",
            "Other Vertebrates", "Background"
        )
    ) +
    guides(
        colour = guide_legend(
            override.aes = list(
                labels = c(
                    "All Verts", "Birds", "Mammals",
                    "Other Vertebrates", "Background"
                )
            )
        ),
        linetype = "none"
    ) +
    scale_linetype_manual(
        values = c("dashed", "solid")
    ) +
    labs(
        x = "Date Range Midpoint",
        y = "Cumulative Extinctions as % \nof IUCN-evaluated Species"
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.3, 0.95),
        legend.justification = "top",
        plot.title = element_text(size = rel(1.5))
    ) +
    ggtitle(extinctions_title)
ggsave(
    here::here("./figs/fig-1/extinctions.png"),
    extinctions_plot,
    height = 7, width = 7
)
ggsave(
    here::here("./figs/fig-1/submission-figs/extinctions.pdf"),
    extinctions_plot,
    width = 7, height = 7,
    dpi = 600
)
## outbreaks ===================================================================

### data cleaning ==============================================================
by_year_df <- spillover %>%
    dplyr::group_by(Event_start_year) %>%
    dplyr::reframe(
        reported_cases = sum(reported_cases),
        reported_events = unique(Event_name)
    ) %>%
    dplyr::group_by(Event_start_year) %>%
    dplyr::summarize(
        event_num = dplyr::n_distinct(reported_events)
    ) %>%
    tidyr::complete(
        Event_start_year = 1963:2019, fill = list(event_num = 0)
    ) %>%
    dplyr::rename(year = Event_start_year)

### model fitting ==============================================================

# fit simple model to show the prediction
mod <- rstanarm::stan_glm(
    event_num ~ year,
    data = by_year_df,
    family = rstanarm::neg_binomial_2(),
    iter = 10000,
    warmup = 3000,
    chains = 4,
    cores = 4
)
# draw some predicted values
mod_pred <- tidybayes::add_epred_draws(
    newdata = modelr::data_grid(
        data = by_year_df,
        year = modelr::seq_range(year, n = 101)
    ),
    object = mod
)

### plotting ===================================================================
spillover_title <- expression("Disease emergence")
spillover_plot <- ggplot() +
    ggdist::stat_lineribbon(
        data = mod_pred,
        aes(y = .epred, x = year), .width = c(.99, .95, .8, .5),
        color = MoMAColors::moma.colors("Flash")[8],
        alpha = 0.4
    ) +
    geom_point(
        data = by_year_df,
        aes(x = year, y = event_num),
        shape = 21, colour = "black", fill = "#74478f", alpha = 0.7
    ) +
    theme_base() +
    labs(x = "Year", y = "Reported Spillover Events") +
    scale_fill_manual(
        name = "Credible Interval",
        values = c(MoMAColors::moma.colors("Ernst")[c(4, 6, 7, 8)])
    ) +
    scale_x_continuous(
        breaks = c(
            1965, 1975, 1985, 1995, 2005, 2015
        )
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.3, 0.95),
        legend.justification = "top",
        plot.title = element_text(size = rel(1.5))
        # legend.title = element_text(hjust = 0.5)
    ) +
    ggtitle(spillover_title)
ggsave(
    here::here("./figs/fig-1/spillover.png"),
    spillover_plot,
    height = 7, width = 7
)
ggsave(
    here::here("./figs/fig-1/submission-figs/spillover.pdf"),
    spillover_plot,
    width = 7, height = 7,
    dpi = 600
)
## temperature timeseries ======================================================

### data cleaning ==============================================================
temperature <- temperature %>%
    dplyr::rename(
        year = Year,
        median = `Full ensemble median`,
        lo_ci = `Full ensemble 2.5th percentile`,
        hi_ci = `Full ensemble 97.5th percentile`,
        instrument = `Cowtan & Way instrumental target`,
        median_31 = `31-year filtered full ensemble median`,
        lo_ci_31 = `31-year filtered full ensemble 2.5th percentile`,
        hi_ci_31 = `31-year filtered full ensemble 97.5th percentile`
    ) %>%
    # dplyr::select(year, median, lo_ci, hi_ci) %>%
    dplyr::filter(year > 1600)
#' we're re-baselining these data to 1600-1699, so the values will all be
#' subtracted from the mean of the median, upper and lower CI's respecitvely
#' and then given as values relative to that time
mean_baseline <- temperature %>%
    dplyr::filter(year %in% c(1600:1699)) %>%
    dplyr::summarize(
        mean = mean(median),
        hi = mean(hi_ci),
        lo = mean(lo_ci)
    )
re_baselined_temp <- temperature %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        median = median - mean_baseline$mean,
        lo_ci = lo_ci - mean_baseline$mean,
        hi_ci = hi_ci - mean_baseline$mean,
        instrument = instrument - mean_baseline$mean
    ) %>%
    #' I want just ONE variable to use as a colour scale variable so I don't
    #' really care if it's EXACTLY accurate, but I'm making one out of the
    #' median pre-1850 and the instrument after 1850 so I can use that single
    #' value
    dplyr::mutate(
        single_val = ifelse(year < 1850, median, instrument)
    )

# clean and summarize the CO2 data
co2_observed <- co2_observed %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        ppm_mean = mean(ppm),
        ppm_sterr = std_err(ppm)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        up_ci = ppm_mean + (1.96 * ppm_sterr),
        lo_ci = ppm_mean - (1.96 * ppm_sterr)
    )
co2_reconstructed <- co2_reconstructed %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
        mean = mean(dplyr::c_across(ALL_50_full:ALL_200), na.rm = TRUE)
    )
# put the two together, deferring to the observed ones
co2_df <- data.frame(
    year = c(
        co2_reconstructed$Year[which(co2_reconstructed$Year < 1974)],
        co2_observed$year
    ),
    ppm = c(
        co2_reconstructed$mean[which(co2_reconstructed$Year < 1974)],
        co2_observed$ppm_mean
    )
)
# join the temperature data with the co2 data
re_baselined_temp <- dplyr::left_join(
    re_baselined_temp,
    co2_df,
    by = "year"
)

### plotting ===================================================================
temperature_title <- expression("Climate change")
temperature_plot <- ggplot() +
    geom_hline(
        aes(yintercept = 0.0),
        colour = "black", linetype = "dashed", alpha = 0.4
    ) +
    geom_ribbon(
        data = re_baselined_temp[which(re_baselined_temp$year < 1850), ],
        aes(
            x = year, ymin = lo_ci, ymax = hi_ci
        ), fill = "#faedbf", alpha = 0.6
    ) +
    geom_line(
        data = re_baselined_temp[which(re_baselined_temp$year < 1850), ],
        aes(
            x = year, y = single_val
        ), linewidth = 0.7, colour = "grey60", linetype = "11", alpha = 0.6
    ) +
    geom_line(
        data = re_baselined_temp[which(re_baselined_temp$year >= 1850), ],
        aes(
            x = year, y = single_val, colour = ppm
        ), linewidth = 0.8
    ) +
    scale_colour_gradient(
        bquote("Atmospheric" ~ CO[2] * " (ppm)"),
        # paste0("Atmospheric ", expression(paste(CO^2)), " (ppm)"),
        low = "#f3d567", high = "#b80422",
        limits = c(275, 407)
    ) +
    theme_base() +
    labs(
        x = "Year", y = "Temperature Anomaly °C From \n1600-1699 Baseline",
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.4, 0.95),
        legend.justification = "top",
        plot.title = element_text(size = rel(1.5))
    ) +
    scale_x_continuous(
        breaks = seq(from = 1600, to = 2020, by = 100)
    ) +
    scale_y_continuous(
        breaks = seq(from = -0.5, to = 1.5, by = 0.5),
        limits = c(-0.7, 1.6)
    ) +
    ggtitle(temperature_title)
ggsave(
    here::here("./figs/fig-1/temperature.png"),
    temperature_plot,
    height = 7, width = 7
)
ggsave(
    here::here("./figs/fig-1/submission-figs/temperature.pdf"),
    temperature_plot,
    width = 7, height = 7,
    dpi = 600
)
# putting plots together =======================================================

top_panels <- spillover_plot + extinctions_plot + temperature_plot
ggsave(
    here::here("./figs/fig-1/top-panels.png"),
    top_panels,
    height = 7, width = 21,
    dpi = 300
)
ggsave(
    here::here("./figs/fig-1/submission-figs/top-panels.pdf"),
    top_panels,
    width = 21, height = 7,
    dpi = 600
)

all_panels <- ggpubr::ggarrange(
    (spillover_plot +
        extinctions_plot +
        temperature_plot) /
        (richness_eidr_map),
    labels = c("A", "B", "C", "D"),
    nrow = 2,
    font.label = list(size = 28),
    heights = c(0.6, -1),
    widths = c(0.33, 0.33, 0.33, 1)
) +
    ggpubr::bgcolor("white")

all_panels <-
    (spillover_plot +
        extinctions_plot +
        temperature_plot) /
        (richness_eidr_map) +
        patchwork::plot_layout(
            heights = c(0.6, -1),
            widths = c(0.33, 0.33, 0.33, 1)
        ) +
        plot_annotation(
            tag_levels = "A"
        ) &
        theme(plot.tag = element_text(face = "bold", size = 30))

ggsave(
    here::here("./figs/fig-1/figure-1.png"),
    all_panels,
    dpi = 300,
    height = 14, width = 15
)
ggsave(
    here::here("./figs/fig-1/submission-figs/figure-1.pdf"),
    all_panels,
    width = 15, height = 14,
    dpi = 600
)
