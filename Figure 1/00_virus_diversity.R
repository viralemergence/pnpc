#' DESCRIPTION: a map of viral biodiversity
#' AUTHOR: Cole Brookson
#' DATE: 06 June 2024

# set up =======================================================================
library(here)
library(ggplot2)
library(vroom)

# pull updated data
vir <- vroom::vroom("https://viralemergence.github.io/virion/Virion.csv.gz")
str(vir)
