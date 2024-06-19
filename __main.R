#' DESCRIPTION: The common tasks to get all data needed to create figures here
#' AUTHOR: Cole Brookson
#' DATE: 19 June 2024

# set up =======================================================================

#' IUCN data are too big to be tracked by github. In lieu of doing something
#' actually good and correct, they now live on my (Cole's) personal google drive
#' and you will need to download them with the following script to use them
link <- paste0(
    "https://drive.google.com/drive/folders/1VQekuMBQAHdI23T2OYsBhBW9SSsb73yg?",
    "usp=sharing"
)
#' if the files exist already locally, don't download and opent the zip, but if
#' they don't, then download and unzip
if (file.exists(here::here("./data/IUCN/"))) {
    print("Files present OR other files are in the same location")
} else {
    print("You need the IUCN files!! They're getting downloaded now")
}
