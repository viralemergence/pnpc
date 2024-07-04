#' DESCRIPTION: donwnloading the files from the CLOVER data
#' AUTHOR: Cole Brookson
#' DATE: 04 July 2024

# download =====================================================================

if (file.exists(here::here("./data/clover/"))) {
    print("Clover files exist alrady :)")
} else {
    print("You need the clover files!! They're getting downloaded now")

    # there are three files we need
    if (!require(here)) install.packages("here")
    library(here)
    download.file(
        url = paste0(
            "https://github.com/viralemergence/clover/blob/main/clover/",
            "clover_1.0_allpathogens/",
            "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv"
        ),
        destfile = here::here("./data/clover/clover-1.0-bacteria.csv")
    )
    download.file(
        url = paste0(
            "https://github.com/viralemergence/clover/blob/main/clover/",
            "clover_1.0_allpathogens/",
            "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv"
        ),
        destfile = here::here("./data/clover/clover-1.0-hel-proto-fungi.csv")
    )
    download.file(
        url = paste0(
            "https://github.com/viralemergence/clover/blob/main/clover/",
            "clover_1.0_allpathogens/",
            "CLOVER_1.0_Viruses_AssociationsFlatFile.csv"
        ),
        destfile = here::here("./data/clover/clover-1.0-viruses.csv")
    )
}
