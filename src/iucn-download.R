#' DESCRIPTION: donwnloading the files for the IUCN data
#' AUTHOR: Cole Brookson
#' DATE: 19 June 2024

# download =====================================================================

#' IUCN data are too big to be tracked by github. In lieu of doing something
#' actually good and correct, they now live on my (Cole's) personal google drive
#' and you will need to download them with the following script to use them.
#' If the files exist already locally, don't download and opent the zip, but if
#' they don't, then download and unzip
#' the download link created with: https://sites.google.com/site/gdocs2direct/
if (file.exists(here::here("./data/IUCN/"))) {
    print("IUCN files present OR other files are in the same location")
} else {
    print("You need the IUCN files!! They're getting downloaded now")

    # google drive package for this
    if (!require(googledrive)) install.packages("googledrive")
    library(googledrive)
    googledrive::drive_deauth()
    googledrive::drive_user()
    public_file <- googledrive::drive_get(
        googledrive::as_id("1Y00fF2TO9jq2_yK9v6xBLNOeDqtH9DCW")
    )
    googledrive::drive_download(public_file,
        path = here::here("./data/IUCN.zip"), overwrite = TRUE
    )
    utils::unzip(
        here::here("./data/IUCN.zip"),
        exdir = here::here("./data/"),
        overwrite = TRUE
    )
}
