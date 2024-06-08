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

#' extract_virus_associations
#' @description 
extract_virus_associations <- function(mammal, virion) { 
    
}
