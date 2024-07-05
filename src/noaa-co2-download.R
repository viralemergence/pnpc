#' DESCRIPTION: donwnloading the data for the CO2 reconstructions
#' AUTHOR: Cole Brookson
#' DATE: 05 July 2024

# download =====================================================================

#' The data used for reconstructions of CO2 ppm are accesed from NOAA's
#' website at the following address:
#' https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=noaa-recon-10437
#' We download them here for use in this paper.

download.file(
    url = paste0(
        "https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/",
        "frank2010/ensembles-percentiles.txt"
    ),
    destfile = here::here("./data/recreation/data-from-frank-etal-2010.txt")
)

download.file(
    url = paste0(
        "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_daily_mlo.txt"
    ),
    destfile = here::here("./data/recreation/data-from-lan-etal-2024.txt")
)
