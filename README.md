# Pathogens & Planetary Change

This code is used to generate the figures for the review on biodiversity, climate & planetary change, and infectious disease, citation to come. 

## For Users

The code herein uses IUCN data, which takes the form of a large shapefile. Due to its size it can't be stored on GitHub, so it's currently stored on Google Drive. There's a file `src/iucn-download.R` that you should run to download that data. 

All other data are provided, and the sources are noted in the subfolder `README.md` files.

------------------

Please feel free to use and borrow from this code base according to the licence below!

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

------------------

## Accompanying information 

This document contains a brief run-down of all the analysis-related figures in the main text of the pathogens & planetary change manuscript referenced in the root README. The code to generate each of the figures can be found in their respectively named subfolders in the `src` folder. 

### Figure 1 

Figure 1 is the figure involving the most data generated in other publications. For details on how it was created, see `./data/recreation/README.md` or Text S2 in the main text of the manuscript. 

<img src="https://github.com/viralemergence/pnpc/blob/main/figs/fig-1/figure-1.png">


### Figure 4 

Figure 4 is based on data from GBIF exclusively. The download of those data are down directly in the assocatiated script. Note however that accessing GBIF to download the data anew is not just click & run, as it's specific to each GBIF user's profile, and a user would need to configure their R environment to handle the GBIF record pulling if you wanted a fresh version from online. We accessed our version for this paper on 07 July 2024. 

<img src="https://github.com/viralemergence/pnpc/blob/main/figs/fig-4/figure-4.png">


### Figure from Box 2

Box two discusses data opportunities and data gaps. The figure makes use of IUCN data and data from the Virion dataset, the IUCN data is mentioned above, and here we use a flat file version of Virion for consistency. Updated versions of Virion can be found [here](https://www.viralemergence.org/virion). 

<img src="https://github.com/viralemergence/pnpc/blob/main/figs/box-3/side-by-side.png">

## Packages used

We rely on a number of packages to do this analysis. Here are their citations: 

  - Bache S, Wickham H (2022). _magrittr: A Forward-Pipe Operator for R_. R package version 2.0.3, <https://CRAN.R-project.org/package=magrittr>.
  - Campitelli E (2024). _ggnewscale: Multiple Fill and Colour Scales in 'ggplot2'_. R package version 0.4.10, <https://CRAN.R-project.org/package=ggnewscale>.
  - Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K (2024). _rgbif: Interface to the Global Biodiversity Information Facility API_. R package version 3.8.0, <https://CRAN.R-project.org/package=rgbif>. Chamberlain S, Boettiger C (2017). “R Python, and Ruby clients for GBIF species occurrence data.” _PeerJ PrePrints_. <https://doi.org/10.7287/peerj.preprints.3304v1>.
  - D'Agostino McGowan L, Bryan J (2023). _googledrive: An Interface to Google Drive_. R package version 2.1.1, <https://CRAN.R-project.org/package=googledrive>.
  - Eddelbuettel D, Francois R, Allaire J, Ushey K, Kou Q, Russell N, Ucar I, Bates D, Chambers J (2024). _Rcpp: Seamless R and C++ Integration_. R package version 1.0.12, <https://CRAN.R-project.org/package=Rcpp>. Eddelbuettel D, François R (2011). “Rcpp: Seamless R and C++ Integration.” _Journal of Statistical Software_, *40*(8), 1-18. doi:10.18637/jss.v040.i08 <https://doi.org/10.18637/jss.v040.i08>. Eddelbuettel D (2013). _Seamless R and C++ Integration with Rcpp_. Springer, New York. doi:10.1007/978-1-4614-6868-4 <https://doi.org/10.1007/978-1-4614-6868-4>, ISBN 978-1-4614-6867-7. Eddelbuettel D, Balamuta J (2018). “Extending R with C++: A Brief Introduction to Rcpp.” _The American Statistician_, *72*(1), 28-36. doi:10.1080/00031305.2017.1375990 <https://doi.org/10.1080/00031305.2017.1375990>.
  - Gabry J, Mahr T (2024). “bayesplot: Plotting for Bayesian Models.” R package version 1.11.1, <https://mc-stan.org/bayesplot/>. Gabry J, Simpson D, Vehtari A, Betancourt M, Gelman A (2019). “Visualization in Bayesian workflow.” _J. R. Stat. Soc. A_, *182*, 389-402. doi:10.1111/rssa.12378 <https://doi.org/10.1111/rssa.12378>.
  - Goodrich B, Gabry J, Ali I, Brilleman S (2024). “rstanarm: Bayesian applied regression modeling via Stan.” R package version 2.32.1, <https://mc-stan.org/rstanarm/>. Brilleman S, Crowther M, Moreno-Betancur M, Buros Novik J, Wolfe R (2018). “Joint longitudinal and time-to-event models via Stan.” StanCon 2018. 10-12 Jan 2018. Pacific Grove, CA, USA., <https://github.com/stan-dev/stancon_talks/>.
  - Grolemund G, Wickham H (2011). “Dates and Times Made Easy with lubridate.” _Journal of Statistical Software_, *40*(3), 1-25. <https://www.jstatsoft.org/v40/i03/>.
  - Hernangómez D (2023). “Using the tidyverse with terra objects: the tidyterra package.” _Journal of Open Source Software_, *8*(91), 5751. ISSN 2475-9066, doi:10.21105/joss.05751 <https://doi.org/10.21105/joss.05751>, <https://doi.org/10.21105/joss.05751>.
  - Hester J, Wickham H, Bryan J (2023). _vroom: Read and Write Rectangular Text Data Quickly_. R package version 1.6.5, <https://CRAN.R-project.org/package=vroom>.
  - Hijmans R (2023). _raster: Geographic Data Analysis and Modeling_. R package version 3.6-26, <https://CRAN.R-project.org/package=raster>.
  - Johnston B (2022). _figpatch: Easily Arrange External Figures with Patchwork Alongside 'ggplot2' Figures_. R package version 0.2, <https://CRAN.R-project.org/package=figpatch>.
  - Kay M (2024). “ggdist: Visualizations of Distributions and Uncertainty in the Grammar of Graphics.” _IEEE Transactions on Visualization and Computer Graphics_, *30*(1), 414-424. doi:10.1109/TVCG.2023.3327195 <https://doi.org/10.1109/TVCG.2023.3327195>. Kay M (2024). _ggdist: Visualizations of Distributions and Uncertainty_. doi:10.5281/zenodo.3879620 <https://doi.org/10.5281/zenodo.3879620>, R package version 3.3.2, <https://mjskay.github.io/ggdist/>.
  - Makowski D, Lüdecke D, Patil I, Thériault R, Ben-Shachar M, Wiernik B (2023). “Automated Results Reporting as a Practical Tool to Improve Reproducibility and Methodological Best Practices Adoption.” _CRAN_. <https://easystats.github.io/report/>.
  - Massicotte P, South A (2023). _rnaturalearth: World Map Data from Natural Earth_. R package version 1.0.1, <https://CRAN.R-project.org/package=rnaturalearth>.
  - Mills BR (2022). _MetBrewer: Color Palettes Inspired by Works at the Metropolitan Museum of Art_. R package version 0.2.0, <https://CRAN.R-project.org/package=MetBrewer>.
  - Mills BR (2024). _MoMAColors: Color Palettes Inspired by Artwork at the Museum of Modern Art in New York City_. R package version 0.0.0.9000.
  - Müller K (2020). _here: A Simpler Way to Find Your Files_. R package version 1.0.1, <https://CRAN.R-project.org/package=here>.
  - Müller K, Wickham H (2023). _tibble: Simple Data Frames_. R package version 3.2.1, <https://CRAN.R-project.org/package=tibble>.
  - Pebesma E, Bivand R (2005). “Classes and methods for spatial data in R.” _R News_, *5*(2), 9-13. <https://CRAN.R-project.org/doc/Rnews/>. Bivand R, Pebesma E, Gomez-Rubio V (2013). _Applied spatial data analysis with R, Second edition_. Springer, NY. <https://asdar-book.org/>.
  - Pebesma E, Bivand R (2023). _Spatial Data Science: With applications in R_. Chapman and Hall/CRC. doi:10.1201/9780429459016 <https://doi.org/10.1201/9780429459016>, <https://r-spatial.org/book/>. Pebesma E (2018). “Simple Features for R: Standardized Support for Spatial Vector Data.” _The R Journal_, *10*(1), 439-446. doi:10.32614/RJ-2018-009 <https://doi.org/10.32614/RJ-2018-009>, <https://doi.org/10.32614/RJ-2018-009>.
  - Pedersen T (2024). _patchwork: The Composer of Plots_. R package version 1.2.0.9000, https://github.com/thomasp85/patchwork, <https://patchwork.data-imaginist.com>.
  - R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
  - Ross N (2024). _fasterize: Fast Polygon to Raster Conversion_. R package version 1.0.5, <https://github.com/ecohealthalliance/fasterize>.
  - Wickham H (2016). _ggplot2: Elegant Graphics for Data Analysis_. Springer-Verlag New York. ISBN 978-3-319-24277-4, <https://ggplot2.tidyverse.org>.
  - Wickham H (2023). _forcats: Tools for Working with Categorical Variables (Factors)_. R package version 1.0.0, <https://CRAN.R-project.org/package=forcats>.
  - Wickham H (2023). _modelr: Modelling Functions that Work with the Pipe_. R package version 0.1.11, <https://CRAN.R-project.org/package=modelr>.
  - Wickham H (2023). _stringr: Simple, Consistent Wrappers for Common String Operations_. R package version 1.5.1, <https://CRAN.R-project.org/package=stringr>.
  - Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
  - Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. R package version 1.1.4, <https://CRAN.R-project.org/package=dplyr>.
  - Wickham H, Henry L (2023). _purrr: Functional Programming Tools_. R package version 1.0.2, <https://CRAN.R-project.org/package=purrr>.
  - Wickham H, Hester J, Bryan J (2023). _readr: Read Rectangular Text Data_. R package version 2.1.4, <https://CRAN.R-project.org/package=readr>.
  - Wickham H, Vaughan D, Girlich M (2024). _tidyr: Tidy Messy Data_. R package version 1.3.1, <https://CRAN.R-project.org/package=tidyr>.
  - Xie Y (2023). _knitr: A General-Purpose Package for Dynamic Report Generation in R_. R package version 1.45, <https://yihui.org/knitr/>. Xie Y (2015). _Dynamic Documents with R and knitr_, 2nd edition. Chapman and Hall/CRC, Boca Raton, Florida. ISBN 978-1498716963, <https://yihui.org/knitr/>. Xie Y (2014). “knitr: A Comprehensive Tool for Reproducible Research in R.” In Stodden V, Leisch F, Peng RD (eds.), _Implementing Reproducible Computational Research_. Chapman and Hall/CRC. ISBN 978-1466561595.> 
