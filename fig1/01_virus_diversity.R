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
    vroom::vroom_write(
        vroom("https://github.com/viralemergence/virion/blob/main/Virion/Provenance.csv.gz", delim = ","),
        here::here("./data/virion/provenence.csv.gz")
    )
}
# read in
vir <- vroom::vroom(here::here("./data/virion/viron.csv.gz"))
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
    vir$HostClass == "mammalia"), ]
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
data_ob <- init_data_ob(iucn_shp, mam_raster) # the data object 
data_ob[] <- 0 # set all of them to zero for now 

# find all the mammals we need to do this process for 
mams_binomials <- stringr::str_to_lower(unique(iucn_shp$binomial))

# get a dataframe of all the hosts that we have data for from both the IUCN and
# also virion 
virion_mams <- match_mammal_taxonomy(
    iucn_data = iucn_shp, 
    virion = good_taxons, 
    virion_taxonomy = host_taxonomy
)

# make sure the IUCN shapefile uses the all lower case form of the data
iucn_data <- iucn_shp %>% 
dplyr::mutate(binomial = stringr::str_to_lower(iucn_shp$binomial))

# create empty data structures to populate 
virus_counts <- vector(length(data_ob), mode = "numeric")
mammal_counts <- vector(length(data_ob), mode = "numeric")
virus_ids <- rep_len(list(character()), length(data_ob))
mammal_ids <- rep_len(list(character()), length(data_ob))

# go through each mammal and populate the data structures 
for(i in seq_len(virion_mams)) {
    # get the mammal 
    mammal <- as.character(virion_mams$HostTaxID[i])
    # figure out which cells it's present in 
    cells <- find_populated_cells(
        mammal = mammal, 
        iucn_data = iucn_data, 
        mam_raster = mam_raster, 
        virion_mams = virion_mams
    )
    # find the associated viruses
    viruses <- extract_virus_associations(mammal, edges_matrix)
    # add the mammal ID to the appropriate cells, and also 
    for(cell in cells){
        mammal_ids[[cell]] <- unique(c(mammal_ids[[cell]], mammal))
        virus_ids[[cell]] <- unique(c(virus_ids[[cell]], viruses))
    }
}





par(mar = c(0, 0.5, 0, 0.5))
fasterize::plot(mammals, axes = FALSE, box = FALSE)

