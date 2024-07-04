#' DESCRIPTION: three hockeystick plots of extinction, outbreaks, & temperature
#' AUTHOR: Cole Brookson
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

extinction <- readr::read_csv(
    here::here("./data/recreation/data-from-ceballos-etal-2015.csv")
) %>%
    dplyr::mutate(
        date_range = as.factor(dplyr::case_when(
            date_range_start == 1500 ~ "1500 - 1600",
            date_range_start == 1600 ~ "1600 - 1700",
            date_range_start == 1700 ~ "1700 - 1800",
            date_range_start == 1800 ~ "1800 - 1900",
            date_range_start == 1900 ~ "1900 - 2010",
        )),
        taxa = factor(taxa, levels = c(
            "all_verts", "birds", "mammals", "other_verts", "background"
        ))
    ) %>%
    dplyr::mutate(
        # this will just make plotting easier
        helper = ifelse(taxa == "background", "background", "taxa")
    )
temperature <- readr::read_delim(
    here::here("./data/recreation/data-from-PAGES2k-const.txt"),
    delim = "\t",
    skip = 4
) %>%
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
spillover <- readr::read_csv(
    here::here("./data/recreation/data-from-meadows-etal-2023.csv")
)

# temperature plot =============================================================
temperature_plot <- ggplot() +
    geom_hline(
        aes(yintercept = 0.0),
        colour = "black", linetype = "dashed", alpha = 0.4
    ) +
    geom_ribbon(
        data = re_baselined_temp[which(re_baselined_temp$year < 1850), ],
        aes(
            x = year, ymin = lo_ci, ymax = hi_ci
        ), fill = "#ebd9f4", alpha = 0.3
    ) +
    geom_line(
        data = re_baselined_temp[which(re_baselined_temp$year < 1850), ],
        aes(
            x = year, y = single_val, colour = single_val
        ), size = 1
    ) +
    geom_line(
        data = re_baselined_temp[which(re_baselined_temp$year < 1850), ],
        aes(
            x = year, y = single_val
        ), size = 1, , colour = "white", linetype = "8f"
    ) +
    geom_line(
        data = re_baselined_temp[which(re_baselined_temp$year >= 1850), ],
        aes(
            x = year, y = single_val, colour = single_val
        ), size = 1
    ) +
    scale_colour_gradient(
        "Median Anomaly °C",
        low = "#e4d1f0", high = "#3b0b59",
        limits = c(-0.5, 1.5)
    ) +
    theme_base() +
    labs(
        x = "Year", y = "Temperature anomaly °C from 1600-1699 baseline",
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.8)
    ) +
    scale_x_continuous(
        breaks = seq(from = 1600, to = 2020, by = 50)
    ) +
    scale_y_continuous(
        breaks = seq(from = -0.5, to = 1.5, by = 0.5),
        limits = c(-0.7, 1.6)
    )
ggsave(
    here::here("./figs/fig-1/temperature.png"),
    temperature_plot,
    height = 7, width = 7
)

## examples
dat <- data.frame(x = seq(2, 10, 2), y = seq(4, 20, 4))

ggplot() +
    geom_colorpath(data = dat, aes(x = x, y = y), cols = c("red", "black")) +
    ggtitle("Default colors")

# plot extinction ==============================================================
extinctions_plot <- ggplot() +
    geom_line(
        data = extinction,
        aes(
            x = date_range, y = cumulative_ext_rate,
            colour = taxa, group = taxa, linetype = helper
        ), size = 2
    ) +
    theme_base() +
    scale_color_manual(
        name = "Taxa",
        values = c(MoMAColors::moma.colors("Klein")[c(1, 2, 4, 7)], "grey80"),
        labels = c(
            "All Verts", "Birds", "Mammals",
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
        x = "Date Range",
        y = "Cumulative Extinctions as % of IUCN-evaluated Species"
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.8)
    )
ggsave(
    here::here("./figs/fig-1/extinctions.png"),
    extinctions_plot,
    height = 7, width = 7
)

# outbreaks plot ===============================================================

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

# plot the results
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
        values = c(MoMAColors::moma.colors("Flash")[c(1, 3, 5, 7)])
    ) +
    scale_x_continuous(
        breaks = c(
            1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010,
            2015, 2020
        )
    ) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.8),
        # legend.title = element_text(hjust = 0.5)
    )
ggsave(
    here::here("./figs/fig-1/spillover.png"),
    spillover_plot,
    height = 7, width = 7
)

# put all the plots together ===================================================
all_panels <- spillover_plot + extinctions_plot + temperature_plot
ggsave(
    here::here("./figs/fig-1/all-panels.png"),
    height = 7, width = 21,
    dpi = 300
)
