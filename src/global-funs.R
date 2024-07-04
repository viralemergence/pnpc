#' DESCRIPTION: some consistently used functions
#' AUTHOR: Cole Brookson
#' DATE: 19 June 2024

#' Foundation Theme
#'
#' This theme is designed to be a foundation from which to build new
#' themes, and not meant to be used directly. \code{theme_foundation()}
#' is a complete theme with only minimal number of elements defined.
#' It is easier to create new themes by extending this one rather
#' than \code{\link[ggplot2]{theme_gray}()} or \code{\link[ggplot2]{theme_bw}()},
#' because those themes define elements deep in the hierarchy.
#'
#' This theme takes \code{\link[ggplot2]{theme_gray}()} and sets all
#' \code{colour} and \code{fill} values to \code{NULL}, except for the top-level
#' elements (\code{line}, \code{rect}, and \code{title}), which have
#' \code{colour = "black"}, and \code{fill = "white"}. This leaves the spacing
#' and-non colour defaults of the default \pkg{ggplot2} themes in place.
#'
#' @export
#' @inheritParams ggplot2::theme_grey
#'
#' @family themes
#' @export
#' @importFrom ggplot2 theme_grey
theme_foundation <- function(base_size = 12, base_family = "") {
    thm <- theme_grey(base_size = base_size, base_family = base_family)
    for (i in names(thm)) {
        if ("colour" %in% names(thm[[i]])) {
            thm[[i]]["colour"] <- list(NULL)
        }
        if ("fill" %in% names(thm[[i]])) {
            thm[[i]]["fill"] <- list(NULL)
        }
    }
    thm + theme(
        panel.border = element_rect(fill = NA),
        legend.background = element_rect(colour = NA),
        line = element_line(colour = "black"),
        rect = element_rect(fill = "white", colour = "black"),
        text = element_text(colour = "black")
    )
}

#' Theme Base
#'
#' Theme similar to the default settings of the \sQuote{base} R graphics.
#'
#' @inheritParams ggplot2::theme_bw
#' @export
#' @family themes
#' @example inst/examples/ex-theme_base.R
theme_base <- function(base_size = 16, base_family = "") {
    theme_foundation() +
        theme(
            line = element_line(
                colour = "black",
                lineend = "round",
                linetype = "solid"
            ),
            rect = element_rect(
                fill = "white",
                colour = "black",
                linetype = "solid"
            ),
            text = element_text(
                colour = "black",
                face = "plain",
                family = base_family,
                size = base_size,
                vjust = 0.5,
                hjust = 0.5,
                lineheight = 1
            ),
            panel.grid = element_blank(),
            strip.background = element_rect(colour = NA),
            legend.key = element_rect(colour = NA),
            title = element_text(size = rel(1)),
            plot.title = element_text(size = rel(1.2), face = "bold"),
            strip.text = element_text(),
            axis.ticks.length = unit(0.5, "lines"),
            # add my addition here
            plot.background = element_rect(colour = NA)
        )
    # TODO: get margins right
}

#' geom_colorpath
#' @description lines with alternating color "just for the effect".
#' @name colorpath
#' @author tjebo (on StackOverflow):
#' https://stackoverflow.com/a/70756000/10475274
#' @examples
#' @export
StatColorPath <- ggproto("StatColorPath", Stat,
    compute_group = function(data, scales, params,
                             n_seg = 20, n = 100,
                             cols = c("black", "white")) {
        # interpolate
        d <- approx(data$x, data$y, n = n)
        # create start and end points for segments
        d2 <- data.frame(
            x = head(d$x, -1), xend = d$x[-1],
            y = head(d$y, -1), yend = d$y[-1]
        )
        # create vector of segment colors
        d2$color <- rep(cols,
            each = ceiling((n - 1) / n_seg),
            length.out = n - 1
        )
        d2
    },
    required_aes = c("x", "y")
)

#' @rdname colorpath
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @author tjebo (on StackOverflow):
#' https://stackoverflow.com/a/70756000/10475274
#' @param n_seg number of segments along line, according to taste
#' @param n number of points at which interpolation takes place
#'   increase if line takes sharp turns
#' @param cols vector of alternating colors
#' @export
geom_colorpath <- function(mapping = NULL, data = NULL, geom = "segment",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE,
                           cols = c("black", "white"),
                           n_seg = 20, n = 100, ...) {
    layer(
        stat = StatColorPath, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(
            na.rm = na.rm, cols = cols, n = n, n_seg = n_seg, ...
        )
    )
}
