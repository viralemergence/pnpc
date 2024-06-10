#' DESCRIPTION: functions to do the messy part of the viral checking
#' AUTHOR: Cole Brookson
#' DATE: 08 June 2024

`%notin%` <- Negate(`%in%`)

#' raster_extract
#' @description Gets the raster itself from the whole of the IUCN data and 
#' returns it in useful form for the extraction of each species values
#' @param iucn_data SF_MULTIPOLYGON. The iucn data that comes in shapefile form
#' from the IUCN itself
#' @return raster extent with all the count data present for the mammals
raster_extract <- function(iucn_data) {
    mam_raster <- fasterize::raster(iucn_shp, res = 1 / 6)
    return(mam_raster)
}

#' init_data_ob
#' @description The raster data object (Formal class '.SingleLayerData') that 
#' we need to put the counts of viruses in doesn't lend itself to easy 
#' in-place editing, so this function extracts it
#' and returns it for further manipulation
#' @param iucn_data SF_MULTIPOLYGON. The iucn data that comes in shapefile form
#' from the IUCN itself
#' @param mam_raster Formal class 'RasterLayer' [package "raster"]. The raster
#' which includes the count data of all the mammals in each raster cell.
#' @return raster data values that is just a numeric vector
init_data_ob <- function(iucn_data, mam_raster) {
    mammals <- fasterize::fasterize(iucn_shp, mam_raster, fun = "count")
    data <- mammals@data@values # keep only values not the whole object
    return(data)
}

#' match_mammal_taxonomy
#' @description There's some weird stuff happening with the mammal taxonomy 
#' between the IUCN vs VIRION stuff so this is tryng to fix that maybe?
#' @param iucn_data SF_MULTIPOLYGON. The iucn data that comes in shapefile form
#' from the IUCN itself
#' @param virion dataframe. The virion dataframe 
#' @param virion_taxonomy dataframe. The HostTaxonomy of the virion database
#'
#' @return dataframe all the mammals that are to be quieried
match_mammal_taxonomy <- function(iucn_data, virion, virion_taxonomy) { 
    # find all the mammals we need to do this process for 
    mams_binomials <- stringr::str_to_lower(unique(iucn_data$binomial))

    # get a dataframe with the HostTaxID and the Host name for each mammal 
    virion_taxonomy <- virion_taxonomy[which(
        virion_taxonomy$HostClass == "mammalia"
    ), c("HostTaxID", "Host")]

    # which mam_binomials aren't in here? 
    mams_binomials_in_virion <- mams_binomials[which(
        mams_binomials %in% virion_taxonomy$Host
    )]

    # keep just the ones that are in both 
    virion_hash <- virion_taxonomy[which(
        virion_taxonomy$Host %in% mams_binomials_in_virion), ]

    return(virion_hash)
}

#' extract_virus_associations
#' @description For a given mammal host name and taxonomic ID, find the 
#' associated viruses (number and taxonomy) from the virion database 
#' @param mammal 1 row dataframe that is the HostTaxId and the Host name of the 
#' mammal at hand
#' @param edges_matrix the matrix of edges that we can use the TaxIDs to
#' identify 
#' @return a vector of the virus association TaxIDs
extract_virus_associations <- function(mammal, edges_matrix) { 
    return(names(edges_matrix[mammal, ][which(
            edges_matrix[mammal, ] > 0)]))
}
