# [GeoChronR](https://nickmckay.github.io/GeoChronR/)
[![DOI:10.5194/gchron-2020-25)](https://zenodo.org/badge/DOI/10.5194/gchron-2020-25.svg)](https://doi.org/10.5194/gchron-2020-25)
[![R](https://img.shields.io/badge/R-3.3.3-blue.svg)]()
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


## What is it?

Quantifying age uncertainties is a critical component of paleoscience (paleoclimatology, paleoecology, paleontology). GeoChronR is an integrated framework that allows scientists to generate state-of-the-art age models for their records, create time-uncertain ensembles of their data, analyze those ensembles with a number of commonly-used techniques, and visualize their results in an intuitive way. The code is being developed in the open-source and community-supported R platform. 

GeoChronR is currently under review as a discussion paper in Geochronology, [check it out here](https://doi.org/10.5194/gchron-2020-25)


## Pre - Installation

Install the remotes package: 
```
 install.packages("remotes")
```

In addition, the packages are not required, but may be useful, and will not install automatically through geoChronR so if you're interested install lipdR and nuspectral from github:

```
remotes::install_github("nickmckay/lipd-utilities",subdir = "R")
remotes::install_github("nickmckay/nuspectral")
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

## How to Cite this Code

McKay, N. P., Emile-Geay, J., and Khider, D.: GeoChronR – an R package to model, analyze and visualize age-uncertain paleoscientific data, Geochronology Discuss., https://doi.org/10.5194/gchron-2020-25, in review, 2020.

[![DOI:10.5194/gchron-2020-25)](https://zenodo.org/badge/DOI/10.5194/gchron-2020-25.svg)](https://doi.org/10.5194/gchron-2020-25)


## Additional Resources 

[Github - lipdR](https://github.com/nickmckay/LiPD-utilities/tree/master/R) 

[Github - LiPD Utilities](https://github.com/nickmckay/LiPD-utilities)

[Linked Earth Wiki](http://wiki.linked.earth/Main_Page)

[LiPD.net](http://www.lipd.net)


## Contact

If you are having issues, please [create an issue here](https://github.com/nickmckay/GeoChronR/issues), or if for some reason it's not appropriate for the Github issue tracker let us know at [nick@nau.edu](mailto:nick@nau.edu).

## License

The project is licensed under the [MIT License](https://github.com/nickmckay/GeoChronR/blob/master/LICENSE)

