# Code For Figure Production 
## Cole B. Brookson
## 08 July 2024

All the code herein can be used to reproduce the figures in the paper referenced here in the main `REAMME.md` file. 

## Code in the `src` root:

- `global-funs.R` contains all functions referenced in some of the sub-files for this analysis, and is called at the beginning of each task-specific script
- `noaa-co2-downloard.R` contains the code that is used to download the $CO^2$ data from NOAA that we use herein. It's not called explicitly anywhere since the data are small enough to ship with the repository, but is left for posterity
- `cover-download.R` downloads data from the CLOVER data and similarly isn't called explicitly, but is left for posterity 
- `iucn-download.R` is the only download script which gets run explicitly since the data themselves are too big to attach to the repository and so will need to be downloaded

## Code in sub folders

Each sub-folder herein contains one main file that generates the single figure referenced in the folder name. This is to keep the worklfow minimal if the goal is to reproduce only one item from the paper. 

Details on the data used in each part of the sub-analysis are either self-explanatory and left as comments in the code / references in the main text of the paper, or are contained in `./data/recreation/README.md` for the data we used from previous publications / other sources that are not as obvious. 