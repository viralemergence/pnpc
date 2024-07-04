#' DESCRIPTION: three hockeystick plots of extinction, outbreaks, & temperature
#' AUTHOR: Cole Brookson
#' DATE: 03 July 2024

# set up =======================================================================
library(here)
library(readr)
library(ggplot2)
library(magrittr)

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
        hi_ci = `Full ensemble 97.5th percentile`
    ) %>%
    dplyr::select(year, median, lo_ci, hi_ci) %>%
    dplyr::filter(year > 1600)
spillover <- readr::read_csv(
    here::here("./data/recreation/data-from-meadows-etal-2023.csv")
)

# plot extinction ==============================================================
extinctions_plot <- ggplot() +
    geom_line(
        data = extinction,
        aes(
            x = date_range, y = cumulative_ext_rate,
            colour = taxa, group = taxa, linetype = helper
        ), size = 3
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
        y = "Cumulative Extinctions as % of IUCN-evaluated Spp"
    )
ggsave(
    here::here("./figs/fig-1/extinctions.png"),
    extinctions_plot,
    height = 8, width = 8
)

# temperature plot =============================================================
temperature_plot <- ggplot() +
    geom_ribbon(
        data = temperature,
        aes(
            x = year, ymin = lo_ci, ymax = hi_ci
        ),
        alpha = 0.2
    ) +
    geom_line(
        data = temperature,
        aes(
            x = year, y = median, colour = median
        ),
        size = 1
    ) +
    theme_base() +
    scale_colour_gradient(
        low = "blue", high = "red",
        limits = c(-1, 1)
    ) +
    guides(
        colour = "none"
    ) +
    labs(x = "Year", y = "Temperature Anomoly °C")
ggsave(
    here::here("./figs/fig-1/temperature.png"),
    temperature_plot,
    height = 7, width = 7
)
