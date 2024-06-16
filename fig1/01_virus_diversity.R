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
library(qs)
library(terra)
library(tidyterra)
library(viridis)
library(geodata)
wrld <- world(path = ".")

source(here::here("./fig1/00_raster_funs.R"))

# pull updated data
outdated <- FALSE
if (outdated) {
    virion_path <- paste0(
        "https://github.com/viralemergence/virion/blob/",
        "main/Virion/Virion.csv.gz"
    )
    vir <- vroom::vroom(virion_path, delim = ",")
    vroom::vroom_write(
        vir,
        here::here("./data/virion/viron.csv.gz")
    )
    edges <- vroom::vroom(
        "https://github.com/viralemergence/virion/blob/main/Virion/Edgelist.csv"
    )
    vroom::vroom_write(
        edges,
        here::here("./data/virion/Edgelist.csv")
    )
}
# read in
vir <- vroom::vroom(here::here("./data/virion/Virion.csv"))
edges <- vroom::vroom(here::here("./data/virion/Edgelist.csv"))
iucn_shp <- sf::st_read(here::here("./data/IUCN/"))
host_taxonomy <- vroom::vroom(here::here("./data/virion/TaxonomyHost.csv"))

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
good_taxons <- vir[which(
    vir$VirusNCBIResolved != FALSE &
        vir$HostNCBIResolved != FALSE &
        vir$HostFlagID != TRUE &
        vir$HostClass == "mammalia"
), ]
# keep only the edges that are represented here
edges <- edges[which(
    edges$HostTaxID %in% good_taxons$HostTaxID &
        edges$VirusTaxID %in% good_taxons$VirusTaxID
), ]
# get rid of AssocId
edges_matrix <- edges %>%
    dplyr::select(-AssocID) %>%
    dplyr::mutate(edge = 1) %>%
    tidyr::complete(., HostTaxID, VirusTaxID, fill = list(edge = 0)) %>%
    tidyr::pivot_wider(names_from = VirusTaxID, values_from = edge) %>%
    tibble::column_to_rownames(., var = "HostTaxID") %>%
    as.matrix()

# Create viral diversity matrix ================================================

mam_raster <- raster_extract(iucn_shp) # this is the full extent we want
mammals <- init_data_ob(iucn_shp, mam_raster) # the data object
data_ob <- mammals@data@values
data_ob[] <- 0 # set all of them to zero for now

# find all the mammals we need to do this process for
mams_binomials <- stringr::str_to_lower(unique(iucn_shp$binomial))

# get a dataframe of all the hosts that we have data for from both the IUCN and
# also virion
virion_mams <- match_mammal_taxonomy(
    iucn_data = iucn_shp,
    virion = good_taxons,
    virion_taxonomy = host_taxonomy
) %>%
    dplyr::arrange(., HostTaxID)

# make sure the IUCN shapefile uses the all lower case form of the data
iucn_data <- iucn_shp %>%
    dplyr::mutate(binomial = stringr::str_to_lower(iucn_shp$binomial))

# create empty data structures to populate
virus_counts <- vector(length(data_ob), mode = "numeric")
mammal_counts <- vector(length(data_ob), mode = "numeric")
virus_ids <- rep_len(list(character()), length(data_ob))
mammal_ids <- rep_len(list(character()), length(data_ob))

# go through each mammal and populate the data structures
if (count_outdated) {
    for (i in seq_len(nrow(virion_mams))) {
        # get the mammal
        mammal <- as.character(virion_mams$HostTaxID[i])
        if (as.numeric(mammal) %notin% edges$HostTaxID) {
            next
        }
        # figure out which cells it's present in
        cells <- find_populated_cells(
            mammal = mammal,
            iucn_data = iucn_data,
            mam_raster = mam_raster,
            virion_mams = virion_mams
        )
        # find the associated viruses
        viruses <- extract_virus_associations(
            mammal = mammal,
            edges_matrix = edges_matrix
        )
        # print(length(viruses))
        # add the mammal ID to the appropriate cells, and also
        for (cell in cells) {
            mammal_ids[[cell]] <- unique(c(mammal_ids[[cell]], mammal))
            virus_ids[[cell]] <- unique(c(virus_ids[[cell]], viruses))
        }
        if (i %% 100 == 0) {
            print(i)
        }
    }
    # save objects
    qs::qsave(mammal_counts, here::here("./data/outputs/mammal-counts.qs"))
    qs::qsave(virus_counts, here::here("./data/outputs/virus-counts.qs"))
} else {
    mammal_counts <- qs::qread(here::here("./data/outputs/mammal-counts.qs"))
    virus_counts <- qs::qread(here::here("./data/outputs/virus-counts.qs"))
}

for (cell in seq_len(length(data_ob))) {
    virus_counts[cell] <- length(virus_ids[[cell]])
    mammal_counts[cell] <- length(mammal_ids[[cell]])
}

# put the values back into the raster and plot =================================

## plot standard map of just viral richness ====================================
mammals_viral_rich <- mammals
mammals_viral_rich@data@values <- virus_counts
ggplot() +
    geom_sf(data = world)
p_viral_rich <- ggplot() +
    # geom_sf(world)
    geom_spatraster(data = terra::rast(mammals_viral_rich)) +
    theme_void() +
    scale_fill_viridis(name = "Viral Diversity", option = "C")
# scale_fill_gradient(low = "white", high = "yellow")
ggsave(
    here::here("./fig1/figs/viral-richness.png"),
    p_viral_rich
)

## viral ndvi plot =============================================================

# do an NDVI style map - (h-v)/(h+v)
viral_NDVI <- vector(length(data_ob), mode = "numeric") # empty
scale_mammal_counts <- range_01(scale_mammal_counts)
scale_virus_counts <- range_01(virus_counts)
for (i in seq_len(length(data_ob))) {
    viral_NDVI[i] <-
        (scale_mammal_counts[i] - scale_virus_counts[i]) /
            (scale_mammal_counts[i] + scale_virus_counts[i])
}
mammals@data@values <- viral_NDVI

p_viral_ndvi <- ggplot() +
    geom_spatraster(data = terra::rast(mammals)) +
    theme_void() +
    scale_fill_gradient2("Viral NDVI", low = "#c10707", high = "#850bd1")
ggsave(
    here::here("./fig1/figs/viral-ndvi.png"),
    p_viral_ndvi
)

## inset of expected viral diversity ===========================================

# OLD IDEAS:
# idea 1: inset map in the corner that is green-purple more/less viruses than
# expected based on host richness

# idea 2: the same setup (main map, inset) but for zoonotic host richness
