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

source(here::here("./fig1/00_raster_funs.R"))

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

# Create viral diversity matrix ================================================

mam_raster <- raster_extract(iucn_shp)
data_ob <- init_data_ob(iucn_shp, mam_raster)
data_ob[] <- 0 # set all of them to zero for now 

# find all the mammals we need to do this process for 
mams_binomials <- stringr::str_to_lower(unique(iucn_shp$binomial))
virion_host_binomials <- unique(
    good_taxons$Host[which(good_taxons$HostClass == "mammalia")]
    )
iucn_not_in_virion <- mams_binomials[which(
    mams_binomials %notin% virion_host_binomials)]
virion_not_in_iucn <- virion_host_binomials[which(
    virion_host_binomials %notin% mams_binomials
)]
length(iucn_not_in_virion); length(mams_binomials); length(virion_not_in_iucn)

# for now only deal with the taxa that are in both lists 
mams <- mams_binomials %in% virion_host_binomials


# IUCN data manipulation =======================================================
mam_raster <- fasterize::raster(iucn_shp, res = 1 / 6)
mammals <- fasterize::fasterize(iucn_shp, mam_raster, fun = "count")
str(mammals)
str(mammals@data)

lophoocc <- fasterize::fasterize(iucn_shp[which(
    iucn_shp$binomial == "Lophostoma occidentalis")
,], mam_raster, fun = "any")
str(lophoocc@data@values)
lophoocc@data@values[which(!is.na(lophoocc@data@values))]



par(mar = c(0, 0.5, 0, 0.5))
fasterize::plot(mammals, axes = FALSE, box = FALSE)








# playing around with an SF version of this ====================================
test_shp <- iucn_shp[which(iucn_shp$binomial == "Lophostoma occidentalis"),]
ggplot() + 
geom_sf(data = test_shp, aes(fill = "presence"))


ggplot() + 
geom_sf(data = iucn_shp)
