# [GeoChronR](https://nickmckay.github.io/LiPD-utilities/)

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.60812.svg)](http://doi.org/10.5281/zenodo.60812)
[![R](https://img.shields.io/badge/R-3.3.3-blue.svg)]()
[![R Studio](https://img.shields.io/badge/RStudio-1.1.453-yellow.svg)]()
[![license](https://img.shields.io/badge/license-GPL-brightgreen.svg)]()


LiPD file analysis and manipulation in R

[About the Project](https://nickmckay.github.io/GeoChronR/about)

[Community](https://nickmckay.github.io/GeoChronR/community)

## What is it?

Quantifying age uncertainties is a critical component of paleoscience (paleoclimatology, paleoecology, paleontology). GeoChronR is an integrated framework that allows scientists to generate state-of-the-art age models for their records, create time-uncertain ensembles of their data, analyze those ensembles with a number of commonly-used techniques, and visualize their results in an intuitive way. The code is being developed in the open-source and community-supported R platform. 

## Requirements

R - v3.3.3

R Studio - v1.1.453

R devtools package - v1.12.0+

##

## Pre - Installation

Install the devtools package: 
```
 install.packages("devtools")
```

The packages below do not install automatically through geoChronR, so you need to install them manually.

Install lipdR and nuspectral from github:

```
devtools::install_github("nickmckay/lipd-utilities",subdir = "R")
devtools::install_github("nickmckay/nuspectral")
```

Install the rBacon package:

```
  install.packages("rbacon")
 
```

## Installation

Install package in R Studio:

```
devtools::install_github("nickmckay/geoChronR")
```

Load the package into the environment:

```
library("geoChronR")
```

## How to Cite this Code

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.60812.svg)](http://doi.org/10.5281/zenodo.60812)

Use this link to visit the Zenodo website. It provides citation information in many popular formats.


## Additional Resources 

[Github - lipdR](https://github.com/nickmckay/LiPD-utilities/tree/master/R) 

[Github - LiPD Utilities](https://github.com/nickmckay/LiPD-utilities)

[Linked Earth Wiki](http://wiki.linked.earth/Main_Page)

[LiPD.net](http://www.lipd.net)


## Contact

If you are having issues, please let me know at [heiser@nau.edu](mailto:heiser@nau.edu).

## License

The project is licensed under the [GNU Public License](https://github.com/nickmckay/GeoChronR/blob/master/Python/LICENSE)

