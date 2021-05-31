# geoChronR 1.0.10

* Added handling for shapes in `mapTs()` for tibbles.

# geoChronR 1.0.9

* Fixed bug writing Bchron output into LiPD structure. Thanks @alexkjames for recognizing the bug.

# geoChronR 1.0.8

* Modified the use of the spatstat package (to spatstat.core) to accommodate the changes in that package.
* Moved astrochron back to a "required" package, now that it's back.

# geoChronR 1.0.7

* Moved astrochron to a "suggested" package, after it was removed from CRAN.

# geoChronR 1.0.6

* Fixed sampleBaconAges() for handling different versions of R and rbacon.

# geoChronR 1.0.5

* Improve and document the `export.quantiles` option in `plotTimeseriesEnsRibbons`
* Fix bug in `plotTimeAvailabilityTs()` when only one instance of the `group.var` [see issue 49](https://github.com/nickmckay/GeoChronR/issues/49)
* Added support for flipping timeseries vertically  in `plotTimeseriesStack` to match interpertations, based on TS metadata, specified in `invert.var`. Details in `?plotTimeseriesStack`. [See issue 30](https://github.com/nickmckay/GeoChronR/issues/30)

# geoChronR 1.0.4

* Better automatic detection for mapping ranges in `baseMap()`

# geoChronR 1.0.3

* Added interoperability with "tidyTs" objects, that is, TS objects expressed as tidy (or nested) tibbles.
* Fixed binding warning in `plotTimeAvailabilityTs()`
* Fixed bug in `mapTs()` when the regions were really small

# geoChronR 1.0.2

* Fixed a bug in `plotTimeAvailabililyTs()` where if `group.var` had a variable with a single instance it would fail. 

# geoChronR 1.0.1

* Added a `NEWS.md` file to track changes to the package.
* Added pkgdown documentation, website available at nickmckay.github.io/GeoChronR
* Updated vignettes for consistency with Geochronology manuscript and version 1.0.0
* Fixed typo in `plotCorEns()` legend when using `use.fdr = FALSE`

# geoChronR 1.0.0

* This release marks the submission of the manuscript describing geoChronR to "Geochronology", the manuscript, which is under discussion, [is available here](https://doi.org/10.5194/gchron-2020-25). 
* Major changes to parameter names (terms within functions) were made for version 1.0.0, standardizing them all to lower case, period separated terminology. If you've been using geoChronR for awhile, this will almost certainly break some of your code, sorry.
