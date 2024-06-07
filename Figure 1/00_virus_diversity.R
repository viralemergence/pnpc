#' DESCRIPTION: a map of viral biodiversity
#' AUTHOR: Cole Brookson
#' DATE: 06 June 2024

# set up =======================================================================
library(here)
library(ggplot2)
library(vroom)
library(fasterize)
library(magrittr)
library(dplyr)
library(sf)
library(microbenchmark)

# pull updated data
outdated <- FALSE
if (outdated) {
    vir <- vroom::vroom(
        "https://github.com/viralemergence/virion/blob/main/Virion/Virion.csv.gz"
    )
    vroom::vroom_write(
        vir,
        here::here("./data/virion/viron.csv.gz")
    )
}
# read in
vir <- vroom::vroom(here::here("./data/virion/viron.csv.gz"))
edges <- vroom::vroom(here::here("./data/virion/Edgelist.csv"))
iucn_shp <- sf::st_read(here::here("./data/IUCN/"))

#' NOTES
#' Task at hand - take the host-virus matrix from virion and multiply it
#' with a rasterized set of the IUCN shapefiles (pixel-host matrix) to get the
#' inclusive set, in each grid cell, of the number of viruses, and then add
#' so that each virus is only counted once
#'
#' This will let us create a map of maximum potential number of viruses that
#' could be in each grid cell based on host community
#'
#' CAVEATS
#' 1. This is subject to weird sampling viruses
#' 2. There are IUCN/Virion matching processes involved here but this can be
#'    done with code Dan Becker has written

# virion manipulation ==========================================================

# keep out the records we don't like
good_taxons <- vir[which(vir$VirusNCBIResolved != FALSE &
    vir$HostNCBIResolved != FALSE & vir$HostFlagID != TRUE), ]
# keep only the edges that are represented here
edges <- edges[which(
    edges$HostTaxID %in% good_taxons$HostTaxID,
    edges$VirusTaxID %in% good_taxons$VirusTaxID
), ]
# get rid of AssocId
edges_complete <- edges %>%
    dplyr::select(-AssocID) %>%
    dplyr::mutate(edge = 1)
# tidyr::complete(., HostTaxID, VirusTaxID)
edges_complete$edge[which(is.na(edges_complete$edge))] <- 0
edge_mat <- as.matrix(
    table(edges_complete$HostTaxID, edges_complete$VirusTaxID)
)

# IUCN data manipulation =======================================================
mam_raster <- fasterize::raster(iucn_shp, res = 1 / 6)
bench2 <- microbenchmark::microbenchmark(
    mammals = mammal_raster <- fasterize(mammal_shapes, mammal_raster, fun = "sum"),
    times = 20, unit = "s"
)
par(mar = c(0, 0.5, 0, 0.5))
fasterize::plot(mam_raster, axes = FALSE, box = FALSE)
