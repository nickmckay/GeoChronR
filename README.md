<!-- badges: start -->
[![DOI:10.5194/gchron-2020-25)](https://zenodo.org/badge/DOI/10.5194/gchron-2020-25.svg)](https://doi.org/10.5194/gchron-2020-25)
[![R](https://img.shields.io/badge/R-3.6.0-blue.svg)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/nickmckay/GeoChronR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nickmckay/GeoChronR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
# geoChronR <img src='man/figures/logo.png' align="right" height="139" />

## What is it?

Quantifying age uncertainties is a critical component of paleoscience (paleoclimatology, paleoecology, paleontology). geoChronR is an integrated framework that allows scientists to generate state-of-the-art age models for their records, create time-uncertain ensembles of their data, analyze those ensembles with a number of commonly-used techniques, and visualize their results in an intuitive way. The code is being developed in the open-source and community-supported R platform. 

A peer-reviewed paper describing geoChronR was published in March 2021 in Geochronology, [check it out here](https://doi.org/10.5194/gchron-3-149-2021)


## Pre-Installation

geoChronR is a package for R, and so you'll need R to run and install it. geoChronR should work with any version newer than 3.6.0. You can download R for your operating system [here](https://www.r-project.org/).

You'll also need an interface to use R. Although many can work, I strongly recommend RStudio. You can download RStudio for your operating system [here](https://rstudio.com/).

Once you have R and your interface up and running, you'll need to install some packages. Most of this will be handled automatically, but to get started install the remotes package: 

```
 install.packages("remotes")
```

## Installation

Install package in R Studio:

```
remotes::install_github("nickmckay/geoChronR")
```

Load the package into the environment:

```
library(geoChronR)
```

## Getting started with geoChronR

A great way to get started with geoChronR is to work through [some of the tutorials avialable at the top of the geoChronR documentation page](http://nickmckay.github.io/GeoChronR). These are also provided as "vignettes" that come installed with geoChronR, which you can access by running `browseVignettes("geoChronR")`. These examples run through how to create an age model ensemble, perform ensemble correlation, regression, spectral and principal components analysis, and visualize the results. They also show off some of the tools we've built for efficiently working with and visualizing LiPD datasets. 

Once you're ready to start applying these techniques to your new data, you'll need one more LiPD files. [Thousands of LiPD datasets are available at lipdverse.org](http://lipdverse.org) and the [World Data Service for Paleoclimatology](https://www.ncdc.noaa.gov/data-access/paleoclimatology-data). If you'd like to create your own dataset, the easiest way is using the [lipd.net/playground](http://lipd.net/playground). This can be a bit daunting at first, but check out the tutorial on the playground, and/or this [youtube tutorial](https://youtu.be/rHZ1oZXmF84) to get started.

## How to Cite this Code

McKay, N. P., Emile-Geay, J., and Khider, D.: geoChronR – an R package to model, analyze, and visualize age-uncertain data, Geochronology, 3, 149–169, https://doi.org/10.5194/gchron-3-149-2021, 2021.

[![DOI:10.5194/gchron-3-149-2021)](https://zenodo.org/badge/DOI/10.5194/gchron-3-149-2021.svg)](https://doi.org/10.5194/gchron-3-149-2021)

## Additional Resources 

[Github - lipdR](https://github.com/nickmckay/lipdR) 

[Github - LiPD Utilities](https://github.com/nickmckay/LiPD-utilities)

[Linked Earth Wiki](http://wiki.linked.earth/Main_Page)

[LiPD.net](http://www.lipd.net)


## Contact

If you are having issues, please [create an issue here](https://github.com/nickmckay/GeoChronR/issues), or if for some reason it's not appropriate for the Github issue tracker let us know at [nick@nau.edu](mailto:nick@nau.edu).

## License

The project is licensed under the [MIT License](https://github.com/nickmckay/GeoChronR/blob/master/LICENSE)

